# Integration tests against a real MariaDB/MySQL server.
#
# Everything here skips unless SFT_TEST_MARIADB_HOST is set, so the suite stays
# green (and CRAN-safe) without a server. Run them with a throwaway container:
#
#   docker run -d --name sft-test-mariadb -e MARIADB_ROOT_PASSWORD=root \
#     -p 3306:3306 mariadb:11
#   SFT_TEST_MARIADB_HOST=127.0.0.1 SFT_TEST_MARIADB_USER=root \
#     SFT_TEST_MARIADB_PASSWORD=root Rscript -e 'devtools::test()'
#
# These cover what the server-free tests cannot: that the SQL the package emits
# is actually accepted, and that the behaviour it relies on (repeated NULLs in a
# unique index, collation, error codes) is really what the server does.

testthat::test_that("the schema lands on a live server with the intended types", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  tables <- DBI::dbListTables(conn)
  testthat::expect_true(all(
    c(sft_system_table_names(), "contacts") %in% tables
  ))

  # Payload columns must not carry MariaDB's 64KB TEXT ceiling.
  testthat::expect_identical(
    test_column_type(conn, "sft_forms", "config_json"), "mediumtext"
  )
  testthat::expect_identical(
    test_column_type(conn, "sft_audit_log", "new_data_json"), "mediumtext"
  )

  # Tables must not inherit a latin1 server default.
  collation <- DBI::dbGetQuery(
    conn,
    "SELECT TABLE_COLLATION FROM information_schema.TABLES
     WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'contacts'"
  )$TABLE_COLLATION
  testthat::expect_true(startsWith(collation, "utf8mb4"))

  # The composite unique index the uniqueness mechanism depends on.
  testthat::expect_true(
    "uq_contacts__email" %in% sft_list_index_names(conn, "contacts")
  )

  # A second pass has nothing left to do, i.e. the probe settles.
  testthat::expect_true(sft_schema_is_current(conn, contacts))
  testthat::expect_equal(nrow(plan_migration(contacts, conn)$actions), 0L)
})

testthat::test_that("a record's whole lifecycle survives a round trip", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")
  expect_record_count(contacts, conn, 1)

  update_record(contacts, list(name = "Ada L."), record_id = 1,
                conn = conn, user = "test")
  testthat::expect_identical(fetch_records(contacts, conn = conn)$name, "Ada L.")

  soft_delete_record(contacts, record_id = 1, conn = conn, user = "test")
  expect_record_count(contacts, conn, 0)
  expect_record_count(contacts, conn, 1, include_deleted = TRUE)

  restore_record(contacts, record_id = 1, conn = conn, user = "test")
  expect_record_count(contacts, conn, 1)

  audit <- fetch_audit_log(contacts, conn = conn)
  testthat::expect_true(
    all(c("insert", "update", "delete") %in% audit$action)
  )
  # version_no is allocated as MAX + 1 and guarded by a unique index.
  testthat::expect_false(any(duplicated(audit$version_no)))
})

testthat::test_that("unique values are enforced and freed again by a soft delete", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")

  testthat::expect_error(
    insert_record(contacts, list(name = "Other", email = "ada@example.com"),
                  conn = conn, user = "test")
  )

  # After a soft delete the value belongs to nobody, so it can be reused - the
  # whole point of the sft_unique_slot mechanism.
  soft_delete_record(contacts, record_id = 1, conn = conn, user = "test")
  testthat::expect_no_error(
    insert_record(contacts, list(name = "Reuse", email = "ada@example.com"),
                  conn = conn, user = "test")
  )

  # MariaDB collations are case- and accent-insensitive, so this counts as the
  # same value. Documented in ?db_mariadb; asserted here so a collation change
  # cannot pass unnoticed.
  testthat::expect_error(
    insert_record(contacts, list(name = "Case", email = "ADA@EXAMPLE.COM"),
                  conn = conn, user = "test")
  )
})

testthat::test_that("text outside latin1 round-trips unchanged", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  # Umlauts exist in latin1 and would survive either way; the CJK and emoji
  # cases are the ones a latin1 column rejects outright (error 1366).
  values <- c("Müller Straße", "北京", "Team \U0001F680")

  for (i in seq_along(values)) {
    insert_record(
      contacts,
      list(name = values[i], email = paste0("user", i, "@example.com")),
      conn = conn, user = "test"
    )
  }

  testthat::expect_identical(fetch_records(contacts, conn = conn)$name, values)
})

testthat::test_that("a database from an older package version is widened and keeps working", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  # Rewind the database to what an older version of the package left behind:
  # TEXT payload columns (the 64KB ceiling) and two system columns that have
  # since been dropped, one of them carrying a UNIQUE index.
  DBI::dbExecute(conn, "ALTER TABLE sft_forms MODIFY COLUMN config_json TEXT")
  DBI::dbExecute(conn, "ALTER TABLE sft_audit_log MODIFY COLUMN new_data_json TEXT")
  DBI::dbExecute(conn, "ALTER TABLE contacts ADD COLUMN sft_form_id VARCHAR(255)")
  DBI::dbExecute(conn, "ALTER TABLE contacts ADD COLUMN sft_uuid VARCHAR(255) UNIQUE")

  # ...including the stored schema signature, which is what actually makes the
  # probe notice. Extra columns alone do NOT: the probe looks for expected
  # structure that is missing, not for structure that is left over. A real
  # pre-upgrade database differs here because dropping those two system columns
  # changed the signature the old package had written.
  DBI::dbExecute(
    conn,
    "UPDATE sft_forms SET schema_hash = 'signature written by an older version'"
  )

  testthat::expect_false(sft_schema_is_current(conn, contacts))

  plan <- plan_migration(contacts, conn)
  testthat::expect_setequal(
    plan$actions$db_column[plan$actions$action == "retire_column"],
    c("sft_form_id", "sft_uuid")
  )

  # Two inserts, so the second one proves that repeated NULLs are accepted by
  # the legacy sft_uuid UNIQUE index rather than colliding with each other.
  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")
  insert_record(contacts, list(name = "Grace", email = "grace@example.com"),
                conn = conn, user = "test")
  expect_record_count(contacts, conn, 2)

  # The retired columns stay in place, unfilled.
  legacy <- DBI::dbGetQuery(conn, "SELECT sft_uuid, sft_form_id FROM contacts")
  testthat::expect_true(all(is.na(legacy$sft_uuid)))

  # The payload columns were widened on the way through.
  testthat::expect_identical(
    test_column_type(conn, "sft_forms", "config_json"), "mediumtext"
  )
  testthat::expect_identical(
    test_column_type(conn, "sft_audit_log", "new_data_json"), "mediumtext"
  )

  # And the probe settles instead of re-migrating on every call.
  testthat::expect_true(sft_schema_is_current(conn, contacts))
})

testthat::test_that("init_db widens the payload columns even without schema drift", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  # A database whose form signature is already current but whose payload columns
  # predate the move to MEDIUMTEXT. The probe cannot see this - system tables are
  # not part of the schema signature - so nothing reconciles on its own and the
  # 64KB ceiling would survive silently. An explicit init_db() is the way out,
  # which is why it is the documented setup step.
  DBI::dbExecute(conn, "ALTER TABLE sft_forms MODIFY COLUMN config_json TEXT")
  testthat::expect_true(sft_schema_is_current(conn, contacts))
  testthat::expect_identical(
    test_column_type(conn, "sft_forms", "config_json"), "text"
  )

  init_db(contacts, conn = conn, user = "test")

  testthat::expect_identical(
    test_column_type(conn, "sft_forms", "config_json"), "mediumtext"
  )
})

testthat::test_that("dropping a unique field removes its index", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")
  testthat::expect_true(
    "uq_contacts__email" %in% sft_list_index_names(conn, "contacts")
  )

  # Same form, email no longer unique. MySQL rejects DROP INDEX ... IF EXISTS,
  # so this is the case that has to work without it.
  relaxed <- form(
    form_id = "contacts", table_name = "contacts", db = db, version = 2,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail")
    )
  )

  testthat::expect_true(
    "drop_index" %in% plan_migration(relaxed, conn)$actions$action
  )
  apply_migration(relaxed, conn, user = "test")
  testthat::expect_false(
    "uq_contacts__email" %in% sft_list_index_names(conn, "contacts")
  )

  # Dropping it again is a no-op rather than an error.
  testthat::expect_true(sft_drop_index(conn, "contacts", "uq_contacts__email"))
})

testthat::test_that("a table name with an uppercase letter stays usable", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  # On a server with lower_case_table_names set (the Windows and macOS default)
  # this table is stored as `mystaff`, and a case-sensitive lookup would never
  # find it again: the probe would never go green, every CRUD call would re-run
  # the migration, and the second CREATE UNIQUE INDEX would fail with 1061. On a
  # case-sensitive server the same test simply exercises the exact-match path.
  mixed <- test_form_basic("mixed", table_name = "MyStaff", db = db)
  init_db(mixed, conn = conn, user = "test")

  testthat::expect_true(sft_table_exists(conn, "MyStaff"))
  testthat::expect_true(sft_schema_is_current(conn, mixed))

  insert_record(mixed, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")
  insert_record(mixed, list(name = "Grace", email = "grace@example.com"),
                conn = conn, user = "test")
  expect_record_count(mixed, conn, 2)

  testthat::expect_error(
    insert_record(mixed, list(name = "Dup", email = "ada@example.com"),
                  conn = conn, user = "test")
  )

  # Exactly one create_table, i.e. the migration ran once rather than on every
  # call.
  history <- fetch_schema_migrations(mixed, conn = conn)
  testthat::expect_equal(sum(history$action == "create_table"), 1L)
})

testthat::test_that("a must_be_unique rule enforces composite uniqueness", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  # This rule builds its own multi-column SQL, separate from the composite
  # unique INDEX behind `unique = TRUE`, so it needs its own live check.
  bookings <- form(
    form_id = "bookings", table_name = "bookings", db = db,
    fields = list(
      form_field(id = "room", label = "Room", mandatory = TRUE),
      form_field(id = "day", label = "Day", mandatory = TRUE),
      form_field(id = "who", label = "Who")
    ),
    validation_rules = list(
      must_be_unique(id = "room_day", fields = c("room", "day"),
                     message = "That room is taken that day.")
    )
  )
  init_db(bookings, conn = conn, user = "test")

  insert_record(bookings, list(room = "A", day = "2026-08-01", who = "Ada"),
                conn = conn, user = "test")

  # Only the COMBINATION is unique - the same room on another day is fine.
  testthat::expect_no_error(
    insert_record(bookings, list(room = "A", day = "2026-08-02", who = "Grace"),
                  conn = conn, user = "test")
  )

  testthat::expect_error(
    insert_record(bookings, list(room = "A", day = "2026-08-01", who = "Dup"),
                  conn = conn, user = "test"),
    "taken that day"
  )

  # Editing the row that holds the value must not collide with itself.
  testthat::expect_no_error(
    update_record(bookings, list(who = "Ada L."), record_id = 1,
                  conn = conn, user = "test")
  )
})

testthat::test_that("a stale edit is rejected instead of overwriting", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")
  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")

  # What the first user saw when they opened the record.
  opened <- fetch_records(contacts, conn = conn)[1, , drop = FALSE]

  # Someone else saves first.
  update_record(contacts, list(name = "Changed by B"), record_id = 1,
                conn = conn, user = "b")

  conflict <- tryCatch(
    {
      update_record(contacts, list(name = "Changed by A"), record_id = 1,
                    conn = conn, user = "a", expected_record = opened)
      NULL
    },
    sft_edit_conflict = function(c) c
  )

  testthat::expect_s3_class(conflict, "sft_edit_conflict")
  testthat::expect_identical(conflict$columns, "name")

  # The rejection has to be total: the second save writes nothing at all.
  testthat::expect_identical(fetch_records(contacts, conn = conn)$name, "Changed by B")

  # Saving what is already stored is not a conflict - only a real difference is.
  testthat::expect_no_error(
    update_record(contacts, list(name = "Changed by B"), record_id = 1,
                  conn = conn, user = "a",
                  expected_record = fetch_records(contacts, conn = conn)[1, , drop = FALSE])
  )
})

testthat::test_that("restore refuses to steal a unique value from a live record", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  insert_record(contacts, list(name = "Grace", email = "grace@example.com"),
                conn = conn, user = "test")
  soft_delete_record(contacts, record_id = 1, conn = conn, user = "test")

  # The soft delete freed the address, and somebody else took it.
  insert_record(contacts, list(name = "Taker", email = "grace@example.com"),
                conn = conn, user = "test")

  testthat::expect_error(
    restore_record(contacts, record_id = 1, conn = conn, user = "test"),
    "already held by an active record"
  )

  # Once it is free again the restore goes through.
  soft_delete_record(contacts, record_id = 2, conn = conn, user = "test")
  testthat::expect_no_error(
    restore_record(contacts, record_id = 1, conn = conn, user = "test")
  )
  expect_record_count(contacts, conn, 1)

  # Restoring an old version without reactivating leaves the deleted state alone.
  update_record(contacts, list(name = "Grace H."), record_id = 1, conn = conn, user = "test")
  testthat::expect_no_error(
    restore_record(contacts, record_id = 1, version_no = 1, conn = conn,
                   user = "test", reactivate = FALSE)
  )
})

testthat::test_that("a connection the server dropped is healed", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")

  victim <- db_connect(db)
  thread <- DBI::dbGetQuery(victim, "SELECT CONNECTION_ID() AS id")$id[1]

  # A real server-side drop, which is what a MariaDB wait_timeout does to a
  # long-idle Shiny session.
  DBI::dbExecute(conn, paste("KILL", as.character(thread)))
  Sys.sleep(0.5)

  # This is why the guard runs a probe query instead of trusting dbIsValid():
  # the client still believes the connection is fine.
  testthat::expect_true(DBI::dbIsValid(victim))
  testthat::expect_error(DBI::dbGetQuery(victim, "SELECT 1"))

  healed <- sft_live_connection(victim, db)
  withr::defer(db_disconnect(healed))

  testthat::expect_no_error(DBI::dbGetQuery(healed, "SELECT 1"))
  testthat::expect_no_error(
    insert_record(contacts, list(name = "After reconnect", email = "post@example.com"),
                  conn = healed, user = "test")
  )
})

testthat::test_that("a shape field round-trips, including a geometry past 64KB", {
  skip_if_no_mariadb()
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("geojsonsf")

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  sites <- form(
    form_id = "sites", table_name = "sites", db = db,
    fields = list(
      form_field(id = "code", label = "Code", mandatory = TRUE, unique = TRUE),
      shape_field(id = "geometry", label = "Boundary", crs = 4326)
    )
  )
  init_db(sites, conn = conn, user = "test")

  # Geometry is stored as serialised text, and a boundary at any real level of
  # detail is bigger than MariaDB's 64KB TEXT ceiling. A 3000-vertex ring
  # serialises to ~115KB and used to be rejected with "Data too long ... [1406]".
  ring <- function(n) {
    angle <- seq(0, 2 * pi, length.out = n)
    coords <- cbind(10 + cos(angle), 50 + sin(angle))
    coords[n, ] <- coords[1, ]
    coords
  }

  shapes <- sf::st_sf(
    code = c("small", "detailed"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(ring(50L))),
      sf::st_polygon(list(ring(3000L))),
      crs = 4326
    )
  )

  for (code in shapes$code) {
    insert_record(sites, list(code = code), conn = conn, user = "test")
  }

  attach_shapes(sites, shapes = shapes, key = c(code = "code"),
                conn = conn, user = "test")

  records <- fetch_records(sites, conn = conn)
  testthat::expect_equal(nrow(records), 2L)
  testthat::expect_gt(max(nchar(records$geometry)), 65535L)

  shape <- Filter(sft_is_shape_field, sites$fields)[[1L]]
  decoded <- decode_shape(records$geometry, shape)
  testthat::expect_length(decoded, 2L)
  testthat::expect_equal(
    as.numeric(sf::st_area(decoded)),
    as.numeric(sf::st_area(sf::st_geometry(shapes))),
    tolerance = 1e-6
  )
})

testthat::test_that("markdown, custom inputs, the rights table and column views work", {
  skip_if_no_mariadb()
  testthat::skip_if_not_installed("commonmark")
  testthat::skip_if_not_installed("shinyWidgets")

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  # --- markdown: the SOURCE is stored, rendering happens on the way out ------
  notes <- form(
    form_id = "notes", table_name = "notes", db = db,
    fields = list(
      form_field(id = "title", label = "Title", mandatory = TRUE),
      form_field(id = "body", label = "Body", markdown = TRUE)
    )
  )
  init_db(notes, conn = conn, user = "test")

  source_text <- "# Heading\n\n**bold**\n\n<script>alert(1)</script>"
  insert_record(notes, list(title = "T", body = source_text), conn = conn, user = "test")

  stored <- fetch_records(notes, conn = conn)$body
  testthat::expect_identical(stored, source_text)
  testthat::expect_match(sft_render_markdown(stored), "<strong>")
  testthat::expect_false(grepl("<script>", sft_render_markdown(stored), fixed = TRUE))

  # --- custom registered inputs, incl. a non-TEXT column --------------------
  register_input("live_knob", shinyWidgets::knobInput, value_arg = "value")
  register_input("live_picker", shinyWidgets::pickerInput, value_arg = "selected",
                 multiple = TRUE)

  custom <- form(
    form_id = "custom", table_name = "custom", db = db,
    fields = list(
      form_field(id = "level", label = "Level", input_type = "live_knob", db_type = "REAL"),
      form_field(id = "tags", label = "Tags", input_type = "live_picker",
                 args = list(choices = c("a", "b", "c")))
    )
  )
  init_db(custom, conn = conn, user = "test")

  insert_record(custom, list(level = 42, tags = c("a", "c")), conn = conn, user = "test")
  # A length-1 multi value used to be stored as a bare JSON scalar and could not
  # be read back; it must still arrive as an array.
  insert_record(custom, list(level = 7, tags = "b"), conn = conn, user = "test")

  raw <- DBI::dbGetQuery(conn, "SELECT level, tags FROM custom ORDER BY sft_id")
  testthat::expect_equal(raw$level, c(42, 7))
  testthat::expect_identical(sft_parse_json_vector(raw$tags[1]), c("a", "c"))
  testthat::expect_identical(sft_parse_json_vector(raw$tags[2]), "b")

  # --- rights table: multi-select forms stored as a JSON array --------------
  rights <- permissions_form(form_ids = c("notes", "custom"), db = db,
                             users = c("ada", "grace"))
  init_db(rights, conn = conn, user = "test")

  insert_record(
    rights,
    list(user = "ada", forms = c("notes", "custom"),
         can_add = TRUE, can_edit = TRUE, can_delete = FALSE),
    conn = conn, user = "test"
  )

  rules <- fetch_records(rights, conn = conn)
  resolved <- rights_permissions(rules, user = "ada", form_id = "notes")
  testthat::expect_true(resolved$can_add())
  testthat::expect_true(resolved$can_edit())
  testthat::expect_false(resolved$can_delete())

  stranger <- rights_permissions(rules, user = "grace", form_id = "notes")
  testthat::expect_false(stranger$can_add())

  # --- saved column views live in sft_user_preferences ---------------------
  sft_set_column_view(conn, notes, user = "ada", view_name = "short",
                      columns = c("title"))
  sft_set_shared_column_view(conn, notes, view_name = "all",
                             columns = c("title", "body"))

  testthat::expect_identical(
    sft_available_column_view_names(conn, notes, "ada"), "short"
  )
  testthat::expect_identical(
    sft_available_shared_column_view_names(conn, notes), "all"
  )
  testthat::expect_identical(
    sft_get_column_view(conn, notes, "ada", "short"), "title"
  )

  sft_set_active_column_view(conn, notes, "ada", "short")
  testthat::expect_identical(
    sft_get_active_column_view(conn, notes, "ada"), "short"
  )
})

testthat::test_that("concurrent writers on one record lose nothing silently", {
  skip_if_no_mariadb()
  testthat::skip_if_not_installed("parallel")
  testthat::skip_if_not_installed("pkgload")

  # The workers are separate R processes, so they need the package. Prefer the
  # SOURCE tree, because loading an installed copy would quietly test whatever
  # version happens to be installed rather than the one under test.
  source_dir <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)

  if (!file.exists(file.path(source_dir, "DESCRIPTION"))) {
    testthat::skip("package source not reachable from the test directory")
  }

  config <- sft_test_mariadb_config()
  db <- local_test_mariadb()

  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")
  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")

  workers <- 3L
  updates <- 4L

  # Every worker updates THE SAME record. That is what actually collides: the
  # audit log allocates version_no as MAX(version_no) + 1 per record, so two
  # writers reading the same MAX pick the same version and the covering unique
  # index rejects one of them. sft_db_with_transaction() rolls that writer back
  # and re-runs it, recomputing MAX + 1. Writing DIFFERENT records would prove
  # nothing - each new record starts at version 1 and nothing ever contends.
  #
  # The package does NOT promise that every writer gets through: after five
  # attempts with backoff a writer gives up loudly with sft_write_conflict
  # (the load test measured a few percent under this kind of contention, more
  # on a fast local server). What it does promise is that nothing is lost
  # silently: a writer is either committed and audited, or told it failed.
  worker <- function(id, source_dir, config, dbname, updates) {
    pkgload::load_all(source_dir, quiet = TRUE)

    db <- db_mariadb(
      dbname = dbname, host = config$host, port = config$port,
      user = config$user, password = config$password
    )

    contacts <- form(
      form_id = "contacts", table_name = "contacts", db = db,
      fields = list(
        form_field(id = "name", label = "Name", mandatory = TRUE),
        form_field(id = "email", label = "E-Mail", unique = TRUE)
      )
    )

    conn <- local_test_conn(db)

    committed <- 0L
    rejected <- character()

    for (k in seq_len(updates)) {
      tryCatch(
        {
          update_record(
            contacts,
            list(name = paste0("worker ", id, " pass ", k)),
            record_id = 1,
            conn = conn,
            user = paste0("worker", id)
          )
          committed <- committed + 1L
        },
        error = function(e) {
          # The class is what matters; it does not survive the trip back from
          # the worker process, so name it here.
          rejected <<- c(
            rejected,
            if (inherits(e, "sft_write_conflict")) {
              "sft_write_conflict"
            } else {
              conditionMessage(e)
            }
          )
        }
      )
    }

    list(committed = committed, rejected = rejected)
  }

  # The worker is sent to another process together with its environment.
  # Defined here, that environment leads to the package namespace, and
  # unserializing it makes the worker load the INSTALLED copy of the package
  # before load_all() runs: every function that copy has then ran as old
  # code. (Found with an installed 0.1.0, whose retry did not know 1020.)
  environment(worker) <- globalenv()
  cluster <- parallel::makePSOCKcluster(workers)
  withr::defer(parallel::stopCluster(cluster))

  outcomes <- parallel::clusterApply(
    cluster, seq_len(workers), worker,
    source_dir = source_dir, config = config,
    dbname = db$dbname, updates = updates
  )

  committed <- sum(vapply(outcomes, `[[`, integer(1), "committed"))
  rejected <- unlist(lapply(outcomes, `[[`, "rejected"))

  # A writer may be turned away, but only loudly, as a write conflict. Any
  # other error is a real failure.
  testthat::expect_identical(setdiff(unique(rejected), "sft_write_conflict"), character())
  testthat::expect_equal(committed + length(rejected), workers * updates)

  audit <- fetch_audit_log(contacts, conn = conn)

  # One insert plus every COMMITTED update, each with its own version, no gap:
  # no collision survived and nothing was silently dropped.
  testthat::expect_equal(nrow(audit), 1L + committed)
  testthat::expect_setequal(audit$version_no, seq_len(1L + committed))

  # And the record itself is intact: exactly one row, holding the value of the
  # last audited version.
  records <- fetch_records(contacts, conn = conn)
  testthat::expect_equal(nrow(records), 1L)
  last <- audit[which.max(audit$version_no), ]
  testthat::expect_identical(
    records$name,
    jsonlite::fromJSON(last$new_data_json)$name
  )
})

testthat::test_that("a real lock conflict is classified as retryable", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")
  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")

  # A second connection holds the row, so the first one runs into a genuine
  # lock-wait timeout rather than a message we made up.
  holder <- db_connect(db)
  withr::defer(db_disconnect(holder))
  DBI::dbBegin(holder)
  DBI::dbGetQuery(holder, "SELECT * FROM contacts WHERE sft_id = 1 FOR UPDATE")

  DBI::dbExecute(conn, "SET SESSION innodb_lock_wait_timeout = 1")
  blocked <- tryCatch(
    {
      DBI::dbBegin(conn)
      DBI::dbExecute(conn, "UPDATE contacts SET name = 'Blocked' WHERE sft_id = 1")
      NULL
    },
    error = function(e) e
  )
  try(DBI::dbRollback(conn), silent = TRUE)
  try(DBI::dbRollback(holder), silent = TRUE)

  testthat::expect_false(is.null(blocked))
  testthat::expect_true(sft_is_retryable_conflict(blocked))
})

testthat::test_that("upsert_records writes a whole grid in one transaction", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  # A numeric key field on purpose: stored values come back from the server
  # as numbers and must still match the incoming key.
  counts <- form(
    form_id = "counts", table_name = "counts", db = db,
    fields = list(
      form_field(id = "day", label = "Day", input_type = "numericInput"),
      form_field(id = "site", label = "Site", mandatory = TRUE),
      form_field(id = "species", label = "Species", mandatory = TRUE),
      form_field(id = "code", label = "Code", unique = TRUE),
      form_field(id = "first", label = "Adults", input_type = "numericInput"),
      form_field(id = "second", label = "Juveniles", input_type = "numericInput")
    )
  )
  init_db(counts, conn = conn, user = "test")
  key <- c("day", "site", "species")
  # `empty` sees every non-key field, `code` included; only the counts decide.
  counts_only <- function(values) {
    all(unlist(values[intersect(names(values), c("first", "second"))]) %in% c(0, NA))
  }

  first <- upsert_records(
    counts,
    data.frame(day = 1, site = "1", species = c("A", "B", "C"),
               code = c("a1", "b1", "c1"), first = c(10, 20, 30), second = 1),
    key = key, conn = conn, user = "ada", empty = counts_only
  )
  testthat::expect_identical(first$action, c("insert", "insert", "insert"))

  # One call: A unchanged, B updated, C emptied (soft-deleted), D new, and E
  # empty and never stored (skipped). Site 2 must not be touched by the
  # scope of site 1.
  insert_record(counts, list(day = 1, site = "2", species = "A", first = 5),
                conn = conn, user = "ada")
  second <- upsert_records(
    counts,
    data.frame(day = 1, site = "1", species = c("A", "B", "C", "D", "E"),
               code = c("a1", "b1", "c1", "d1", NA),
               first = c(10, 25, 0, 7, 0), second = c(1, 1, 0, 1, NA)),
    key = key, conn = conn, user = "bob", empty = counts_only,
    scope = list(day = 1, site = "1")
  )
  testthat::expect_identical(
    second$action, c("unchanged", "update", "delete", "insert", "skip")
  )
  testthat::expect_identical(second$sft_id[1:3], first$sft_id)

  live <- fetch_records(counts, conn = conn)
  testthat::expect_setequal(
    paste(live$site, live$species, live$first),
    c("1 A 10", "1 B 25", "1 D 7", "2 A 5")
  )

  # The code of the soft-deleted C is free again: entering C anew is an
  # insert of a NEW record that reuses the value.
  third <- upsert_records(
    counts,
    data.frame(day = 1, site = "1", species = c("A", "B", "C", "D"),
               code = c("a1", "b1", "c1", "d1"), first = c(10, 25, 3, 7), second = 1),
    key = key, conn = conn, user = "cy", empty = counts_only,
    scope = list(day = 1, site = "1")
  )
  testthat::expect_identical(third$action, c("unchanged", "unchanged", "insert", "unchanged"))
  testthat::expect_false(third$sft_id[3] %in% first$sft_id)

  # A unique collision in row 2 rolls back row 1 as well, audit included.
  audit_before <- nrow(fetch_audit_log(counts, conn = conn))
  records_before <- fetch_records(counts, conn = conn, include_deleted = TRUE)
  err <- tryCatch(
    upsert_records(
      counts,
      data.frame(day = 2, site = "1", species = c("A", "B"),
                 code = c("x1", "a1"), first = 1, second = 1),
      key = key, conn = conn, user = "dan"
    ),
    error = function(e) e
  )
  testthat::expect_s3_class(err, "error")
  testthat::expect_match(conditionMessage(err), "^Row 2 ")
  testthat::expect_equal(nrow(fetch_audit_log(counts, conn = conn)), audit_before)
  testthat::expect_identical(
    fetch_records(counts, conn = conn, include_deleted = TRUE), records_before
  )

  # Every record's versions are gapless, numbered per record.
  audit <- fetch_audit_log(counts, conn = conn)
  for (id in unique(audit$record_id)) {
    versions <- sort(audit$version_no[audit$record_id == id])
    testthat::expect_identical(as.integer(versions), seq_along(versions))
  }
  testthat::expect_identical(
    sort(table(audit$action)[c("insert", "update", "delete")], decreasing = TRUE),
    sort(c(insert = 6L, update = 1L, delete = 1L), decreasing = TRUE),
    ignore_attr = TRUE
  )
})

testthat::test_that("two upserts of the same new group do not both insert it", {
  skip_if_no_mariadb()
  testthat::skip_if_not_installed("parallel")
  testthat::skip_if_not_installed("pkgload")

  source_dir <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)

  if (!file.exists(file.path(source_dir, "DESCRIPTION"))) {
    testthat::skip("package source not reachable from the test directory")
  }

  config <- sft_test_mariadb_config()
  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  votes_fields <- function() {
    list(
      form_field(id = "site", label = "Site", mandatory = TRUE),
      form_field(id = "species", label = "Species", mandatory = TRUE),
      form_field(id = "first", label = "First", input_type = "numericInput")
    )
  }
  counts <- form(form_id = "counts", table_name = "counts", db = db, fields = votes_fields())
  init_db(counts, conn = conn, user = "test")

  # Without the lock both writers found no stored row and both inserted:
  # every key twice, and every later upsert of the group refused as ambiguous.
  worker <- function(id, source_dir, config, dbname, start_at, fields) {
    pkgload::load_all(source_dir, quiet = TRUE)
    db <- db_mariadb(
      dbname = dbname, host = config$host, port = config$port,
      user = config$user, password = config$password
    )
    counts <- form(form_id = "counts", table_name = "counts", db = db, fields = fields())
    conn <- local_test_conn(db)

    while (Sys.time() < start_at) Sys.sleep(0.001)

    upsert_records(
      counts, data.frame(site = "1", species = c("A", "B", "C"), first = id),
      key = c("site", "species"), conn = conn, user = paste0("w", id),
      scope = list(site = "1")
    )$action
  }

  # See "concurrent writers": nothing sent to the workers may carry the
  # package namespace in its environment.
  environment(worker) <- globalenv()
  environment(votes_fields) <- globalenv()
  cluster <- parallel::makePSOCKcluster(2L)
  withr::defer(parallel::stopCluster(cluster))

  actions <- parallel::clusterApply(
    cluster, 1:2, worker,
    source_dir = source_dir, config = config, dbname = db$dbname,
    start_at = Sys.time() + 3, fields = votes_fields
  )

  live <- fetch_records(counts, conn = conn)
  testthat::expect_equal(nrow(live), 3L)
  testthat::expect_setequal(live$species, c("A", "B", "C"))
  # One writer inserted, the other updated what it found.
  testthat::expect_setequal(
    vapply(actions, function(a) a[1], character(1)), c("insert", "update")
  )
})

testthat::test_that("an upsert that cannot get the table lock fails as a write conflict", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- local_test_conn(db)
  holder <- local_test_conn(db)

  counts <- form(
    form_id = "counts", table_name = "counts", db = db,
    fields = list(form_field(id = "species", label = "Species"))
  )
  init_db(counts, conn = conn, user = "test")

  release <- sft_upsert_lock(holder, counts)
  on.exit(release(), add = TRUE)

  err <- tryCatch(sft_upsert_lock(conn, counts, timeout = 1), error = function(e) e)
  testthat::expect_s3_class(err, "sft_write_conflict")

  # Once released, the next writer gets through.
  release()
  testthat::expect_no_error(
    upsert_records(counts, data.frame(species = "A"), key = "species", conn = conn)
  )
})

testthat::test_that("a slider range is stored on MariaDB, also where the column is still a number", {
  skip_if_no_mariadb()
  db <- local_test_mariadb()
  conn <- local_test_conn(db)

  f <- form(
    form_id = "sl", table_name = "sl", db = db,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "range", label = "Range", input_type = "sliderInput",
                 args = list(min = 0, max = 10, value = c(1, 2)))
    )
  )
  insert_record(f, list(name = "a", range = c(1.25, 7.5)), conn = conn)
  testthat::expect_identical(sft_ui_value(f$fields[[2]], fetch_records(f, conn = conn)$range), c(1.25, 7.5))

  DBI::dbExecute(conn, "DELETE FROM sl")
  DBI::dbExecute(conn, "ALTER TABLE sl MODIFY COLUMN `range` DOUBLE")
  testthat::expect_false(sft_schema_is_current(conn, f))
  insert_record(f, list(name = "b", range = c(2, 3)), conn = conn)
  testthat::expect_identical(sft_ui_value(f$fields[[2]], fetch_records(f, conn = conn)$range), c(2, 3))
  testthat::expect_true(sft_schema_is_current(conn, f))
})

testthat::test_that("with_transaction on MariaDB commits or rolls back as a whole", {
  skip_if_no_mariadb()
  db <- local_test_mariadb()
  conn <- local_test_conn(db)
  a <- form(form_id = "a", table_name = "a", db = db, fields = list(form_field(id = "x", label = "X")))
  b <- form(form_id = "b", table_name = "b", db = db, fields = list(
    form_field(id = "k", label = "K"), form_field(id = "n", label = "N", input_type = "numericInput")))
  init_db(a, conn = conn)
  init_db(b, conn = conn)

  testthat::expect_error(with_transaction(conn, {
    insert_record(a, list(x = "1"), conn = conn)
    upsert_records(b, data.frame(k = "p", n = 1), key = "k", conn = conn)
    stop("boom")
  }), "boom")
  testthat::expect_identical(nrow(fetch_records(a, conn = conn)), 0L)
  testthat::expect_identical(nrow(fetch_records(b, conn = conn)), 0L)

  with_transaction(conn, {
    insert_record(a, list(x = "1"), conn = conn)
    upsert_records(b, data.frame(k = "p", n = 1), key = "k", conn = conn)
  })
  testthat::expect_identical(nrow(fetch_records(a, conn = conn)), 1L)

  # The upsert lock was released when the block ended: another connection
  # gets it at once.
  other <- local_test_conn(db)
  release <- sft_upsert_lock(other, b, timeout = 1)
  release()
})
