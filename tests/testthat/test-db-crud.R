testthat::test_that("sft_insert_record inserts a record and writes audit log", {
  conn <- local_test_conn()
  form <- test_form_basic()

  inserted <- insert_record(
    form = form,
    record = list(
      name = "Ada",
      email = "ada@example.org"
    ),
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(nrow(inserted), 1L)
  testthat::expect_equal(inserted$name, "Ada")
  testthat::expect_equal(inserted$email, "ada@example.org")
  testthat::expect_equal(inserted$sft_is_deleted, 0L)

  audit <- test_audit_log(conn)

  testthat::expect_equal(nrow(audit), 1L)
  testthat::expect_equal(audit$action, "insert")
  testthat::expect_equal(audit$changed_by, "tester")
})

testthat::test_that("sft_insert_record validates mandatory fields", {
  conn <- local_test_conn()

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      )
    )
  )

  testthat::expect_error(
    insert_record(
      form = form,
      record = list(name = ""),
      conn = conn
    ),
    "Mandatory fields missing"
  )
})

testthat::test_that("sft_insert_record validates unique fields", {
  conn <- local_test_conn()

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(
        id = "email",
        label = "E-Mail",
        unique = TRUE
      )
    )
  )

  insert_record(
    form = form,
    record = list(email = "ada@example.org"),
    conn = conn
  )

  testthat::expect_error(
    insert_record(
      form = form,
      record = list(email = "ada@example.org"),
      conn = conn
    ),
    "already taken"
  )
})

testthat::test_that("sft_update_record updates values and writes audit log", {
  conn <- local_test_conn()
  form <- test_form_basic()

  inserted <- insert_record(
    form = form,
    record = list(
      name = "Ada",
      email = "ada@example.org"
    ),
    conn = conn
  )

  updated <- update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(updated$name, "Ada Lovelace")

  audit <- test_audit_log(conn)

  testthat::expect_equal(audit$action, c("insert", "update"))
  testthat::expect_equal(audit$version_no, c(1L, 2L))
})

testthat::test_that("records carry no per-row schema hash or form id", {
  # Both columns were write-only: nothing read them back, and sft_schema_hash
  # stored the entire schema JSON on every row. A new table must not have them.
  conn <- local_test_conn()

  form <- form(
    form_id = "no_dead_columns",
    table_name = "no_dead_columns",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  inserted <- insert_record(form, list(name = "Ada"), conn = conn)

  testthat::expect_false("sft_schema_hash" %in% names(inserted))
  testthat::expect_false("sft_form_id" %in% names(inserted))
  testthat::expect_false("sft_schema_hash" %in% names(sft_system_columns()))
  testthat::expect_false("sft_form_id" %in% names(sft_system_columns()))
})

testthat::test_that("a table still carrying the retired columns keeps working", {
  # The upgrade path: databases written by an older version still have the two
  # columns. They are nullable, retire_column executes no SQL, and nothing reads
  # them - so CRUD must simply carry on and leave them alone.
  conn <- local_test_conn()

  form <- form(
    form_id = "legacy_columns",
    table_name = "legacy_columns",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  init_db(form, conn = conn, apply = TRUE)

  # Simulate the pre-upgrade shape.
  DBI::dbExecute(conn, "ALTER TABLE legacy_columns ADD COLUMN sft_form_id TEXT")
  DBI::dbExecute(conn, "ALTER TABLE legacy_columns ADD COLUMN sft_schema_hash TEXT")

  # The planner retires them rather than dropping them.
  plan <- plan_migration(conn, form)
  retired <- plan$actions$db_column[plan$actions$action == "retire_column"]
  testthat::expect_true(all(c("sft_form_id", "sft_schema_hash") %in% retired))
  testthat::expect_true(all(plan$actions$safe))

  inserted <- insert_record(form, list(name = "Ada"), conn = conn)
  record_id <- inserted$sft_id[1]

  updated <- update_record(
    form = form,
    record_id = record_id,
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )
  testthat::expect_equal(updated$name, "Ada Lovelace")

  fetched <- fetch_records(form, conn = conn)
  testthat::expect_equal(nrow(fetched), 1L)

  # The leftover columns survive untouched and stay empty.
  still_there <- DBI::dbGetQuery(
    conn,
    "SELECT sft_form_id, sft_schema_hash FROM legacy_columns"
  )
  testthat::expect_true(is.na(still_there$sft_form_id[1]))
  testthat::expect_true(is.na(still_there$sft_schema_hash[1]))
})

testthat::test_that("sft_soft_delete_record hides records by default", {
  conn <- local_test_conn()

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      )
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn
  )

  deleted <- soft_delete_record(
    form = form,
    record_id = inserted$sft_id[1],
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(deleted$sft_is_deleted, 1L)

  expect_record_count(form, conn, 0L, include_deleted = FALSE)
  expect_record_count(form, conn, 1L, include_deleted = TRUE)

  audit <- test_audit_log(conn)

  testthat::expect_equal(audit$action, c("insert", "delete"))
})


testthat::test_that("sft_update_record rejects emptied supplied mandatory fields", {
  conn <- local_test_conn()

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      ),
      form_field(
        id = "email",
        label = "E-Mail",
        mandatory = TRUE,
        unique = TRUE
      )
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(
      name = "Ada",
      email = "ada@example.org"
    ),
    conn = conn
  )

  testthat::expect_error(
    update_record(
      form = form,
      record_id = inserted$sft_id[1],
      values = list(
        name = "",
        email = "ada@example.org"
      ),
      conn = conn
    ),
    "Mandatory fields are empty"
  )
})
testthat::test_that("sft_update_record does not overwrite non-editable fields", {
  conn <- local_test_conn()

  form <- form(
    form_id = "locked_fields",
    table_name = "locked_fields",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      ),
      form_field(
        id = "created_note",
        label = "Erstellnotiz",
        editable = FALSE
      )
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(
      name = "Ada",
      created_note = "initial"
    ),
    conn = conn
  )

  updated <- update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(
      name = "Ada Lovelace",
      created_note = "changed"
    ),
    conn = conn
  )

  testthat::expect_equal(updated$name, "Ada Lovelace")
  testthat::expect_equal(updated$created_note, "initial")
})
