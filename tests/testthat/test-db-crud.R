testthat::test_that("sft_insert_record inserts a record and writes audit log", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      ),
      form_field(
        id = "email",
        label = "E-Mail",
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
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(nrow(inserted), 1L)
  testthat::expect_equal(inserted$name, "Ada")
  testthat::expect_equal(inserted$email, "ada@example.org")
  testthat::expect_equal(inserted$sft_is_deleted, 0L)

  audit <- DBI::dbGetQuery(conn, "SELECT * FROM sft_audit_log")

  testthat::expect_equal(nrow(audit), 1L)
  testthat::expect_equal(audit$action, "insert")
  testthat::expect_equal(audit$changed_by, "tester")
})

testthat::test_that("sft_insert_record validates mandatory fields", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
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
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
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
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      ),
      form_field(
        id = "email",
        label = "E-Mail",
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

  updated <- update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(updated$name, "Ada Lovelace")

  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action, version_no FROM sft_audit_log ORDER BY log_id"
  )

  testthat::expect_equal(audit$action, c("insert", "update"))
  testthat::expect_equal(audit$version_no, c(1L, 2L))
})

testthat::test_that("sft_update_record refreshes the record schema hash", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "schema_hash_mirror",
    table_name = "schema_hash_mirror",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  inserted <- insert_record(form, list(name = "Ada"), conn = conn)
  record_id <- inserted$sft_id[1]

  signature <- sft_schema_signature(form)
  testthat::expect_equal(inserted$sft_schema_hash[1], signature)

  # Simulate a record whose stored hash is stale (e.g. written before a
  # migration), then confirm an update rewrites it to the current signature.
  DBI::dbExecute(
    conn,
    "UPDATE schema_hash_mirror SET sft_schema_hash = 'stale' WHERE sft_id = ?",
    params = list(record_id)
  )

  update_record(
    form = form,
    record_id = record_id,
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  refreshed <- sft_get_record(
    conn = conn,
    form = form,
    record_id = record_id,
    include_deleted = TRUE
  )

  testthat::expect_equal(refreshed$sft_schema_hash[1], signature)
})

testthat::test_that("sft_soft_delete_record hides records by default", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
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

  visible <- fetch_records(
    form = form,
    conn = conn,
    include_deleted = FALSE
  )

  all_records <- fetch_records(
    form = form,
    conn = conn,
    include_deleted = TRUE
  )

  testthat::expect_equal(nrow(visible), 0L)
  testthat::expect_equal(nrow(all_records), 1L)

  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action FROM sft_audit_log ORDER BY log_id"
  )

  testthat::expect_equal(audit$action, c("insert", "delete"))
})


testthat::test_that("sft_update_record rejects emptied supplied mandatory fields", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
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
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "locked_fields",
    table_name = "locked_fields",
    db_path = db_path,
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
