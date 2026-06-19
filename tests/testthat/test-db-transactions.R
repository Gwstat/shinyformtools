testthat::test_that("insert rolls back when audit logging fails", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "transaction_insert",
    table_name = "transaction_insert",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  init_db(form, conn = conn, apply = TRUE)

  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_audit_insert
     BEFORE INSERT ON sft_audit_log
     BEGIN
       SELECT RAISE(ABORT, 'forced audit failure');
     END"
  )

  testthat::expect_error(
    insert_record(
      form = form,
      record = list(name = "Ada"),
      conn = conn,
      user = "tester"
    ),
    "forced audit failure"
  )

  records <- fetch_records(
    form = form,
    conn = conn,
    include_deleted = TRUE
  )

  audit <- DBI::dbGetQuery(conn, "SELECT * FROM sft_audit_log")

  testthat::expect_equal(nrow(records), 0L)
  testthat::expect_equal(nrow(audit), 0L)
})

testthat::test_that("update rolls back when audit logging fails", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "transaction_update",
    table_name = "transaction_update",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn,
    user = "tester"
  )

  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_audit_update
     BEFORE INSERT ON sft_audit_log
     BEGIN
       SELECT RAISE(ABORT, 'forced audit failure');
     END"
  )

  testthat::expect_error(
    update_record(
      form = form,
      record_id = inserted$sft_id[1],
      values = list(name = "Ada Lovelace"),
      conn = conn,
      user = "tester"
    ),
    "forced audit failure"
  )

  record <- sft_get_record(
    conn = conn,
    form = form,
    record_id = inserted$sft_id[1],
    include_deleted = TRUE
  )

  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action FROM sft_audit_log ORDER BY log_id"
  )

  testthat::expect_equal(record$name, "Ada")
  testthat::expect_equal(audit$action, "insert")
})

testthat::test_that("soft delete rolls back when audit logging fails", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "transaction_delete",
    table_name = "transaction_delete",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn,
    user = "tester"
  )

  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_audit_delete
     BEFORE INSERT ON sft_audit_log
     BEGIN
       SELECT RAISE(ABORT, 'forced audit failure');
     END"
  )

  testthat::expect_error(
    soft_delete_record(
      form = form,
      record_id = inserted$sft_id[1],
      conn = conn,
      user = "tester"
    ),
    "forced audit failure"
  )

  record <- sft_get_record(
    conn = conn,
    form = form,
    record_id = inserted$sft_id[1],
    include_deleted = TRUE
  )

  testthat::expect_equal(record$sft_is_deleted, 0L)
})

testthat::test_that("restore rolls back when audit logging fails", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "transaction_restore",
    table_name = "transaction_restore",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn,
    user = "tester"
  )

  soft_delete_record(
    form = form,
    record_id = inserted$sft_id[1],
    conn = conn,
    user = "tester"
  )

  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_audit_restore
     BEFORE INSERT ON sft_audit_log
     BEGIN
       SELECT RAISE(ABORT, 'forced audit failure');
     END"
  )

  testthat::expect_error(
    restore_record(
      form = form,
      record_id = inserted$sft_id[1],
      conn = conn,
      user = "tester"
    ),
    "forced audit failure"
  )

  record <- sft_get_record(
    conn = conn,
    form = form,
    record_id = inserted$sft_id[1],
    include_deleted = TRUE
  )

  testthat::expect_equal(record$sft_is_deleted, 1L)
})

testthat::test_that("preference replacement rolls back when preference insert fails", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "transaction_preferences",
    table_name = "transaction_preferences",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

  init_db(form, conn = conn, apply = TRUE)

  sft_set_user_preference(
    conn = conn,
    form = form,
    user = "tester",
    key = "record_columns",
    value = list(columns = c("name"))
  )

  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_preference_insert
     BEFORE INSERT ON sft_user_preferences
     BEGIN
       SELECT RAISE(ABORT, 'forced preference failure');
     END"
  )

  testthat::expect_error(
    sft_set_user_preference(
      conn = conn,
      form = form,
      user = "tester",
      key = "record_columns",
      value = list(columns = c("sft_easy_id", "name"))
    ),
    "forced preference failure"
  )

  value <- sft_get_user_preference(
    conn = conn,
    form = form,
    user = "tester",
    key = "record_columns"
  )

  testthat::expect_equal(as.character(value$columns), "name")
})
