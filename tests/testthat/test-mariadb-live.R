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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
  testthat::expect_equal(nrow(plan_migration(conn, contacts)$actions), 0L)
})

testthat::test_that("a record's whole lifecycle survives a round trip", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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

  plan <- plan_migration(conn, contacts)
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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
    "drop_index" %in% plan_migration(conn, relaxed)$actions$action
  )
  apply_migration(conn, relaxed, user = "test")
  testthat::expect_false(
    "uq_contacts__email" %in% sft_list_index_names(conn, "contacts")
  )

  # Dropping it again is a no-op rather than an error.
  testthat::expect_true(sft_drop_index(conn, "contacts", "uq_contacts__email"))
})

testthat::test_that("a table name with an uppercase letter stays usable", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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

testthat::test_that("concurrent writers on one record all commit, with no lost version", {
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

  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

  contacts <- test_form_basic("contacts", db = db)
  init_db(contacts, conn = conn, user = "test")
  insert_record(contacts, list(name = "Ada", email = "ada@example.com"),
                conn = conn, user = "test")

  workers <- 3L
  updates <- 4L

  # Every worker updates THE SAME record. That is what actually collides: the
  # audit log allocates version_no as MAX(version_no) + 1 per record, so two
  # writers reading the same MAX pick the same version and the covering unique
  # index rejects one of them. sft_db_with_transaction() is supposed to roll
  # that writer back and re-run it, recomputing MAX + 1, so that both commit.
  # Writing DIFFERENT records would prove nothing - each new record starts at
  # version 1 and nothing ever contends.
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

    conn <- db_connect(db)
    on.exit(db_disconnect(conn), add = TRUE)

    for (k in seq_len(updates)) {
      result <- tryCatch(
        {
          update_record(
            contacts,
            list(name = paste0("worker ", id, " pass ", k)),
            record_id = 1,
            conn = conn,
            user = paste0("worker", id)
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )

      if (!is.null(result)) {
        return(result)
      }
    }

    NA_character_
  }

  cluster <- parallel::makePSOCKcluster(workers)
  withr::defer(parallel::stopCluster(cluster))

  failures <- unlist(parallel::clusterApply(
    cluster, seq_len(workers), worker,
    source_dir = source_dir, config = config,
    dbname = db$dbname, updates = updates
  ))

  # Not one writer may be turned away.
  testthat::expect_identical(failures, rep(NA_character_, workers))

  audit <- fetch_audit_log(contacts, conn = conn)

  # One insert plus every update, each with its own version - no collision
  # survived and nothing was silently dropped.
  testthat::expect_equal(nrow(audit), 1L + workers * updates)
  testthat::expect_setequal(audit$version_no, seq_len(1L + workers * updates))

  # And the record itself is intact: exactly one row, holding one worker's value.
  records <- fetch_records(contacts, conn = conn)
  testthat::expect_equal(nrow(records), 1L)
  testthat::expect_match(records$name, "^worker [0-9]+ pass [0-9]+$")
})

testthat::test_that("a real lock conflict is classified as retryable", {
  skip_if_no_mariadb()

  db <- local_test_mariadb()
  conn <- db_connect(db)
  withr::defer(db_disconnect(conn))

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
