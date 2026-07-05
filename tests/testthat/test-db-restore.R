testthat::test_that("sft_list_versions returns audit versions for a record", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

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
    conn = conn,
    user = "tester"
  )

  update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  versions <- list_versions(
    form = form,
    conn = conn,
    record_id = inserted$sft_id[1]
  )

  testthat::expect_equal(nrow(versions), 2L)
  testthat::expect_equal(versions$action, c("insert", "update"))
  testthat::expect_equal(versions$version_no, c(1L, 2L))
})

testthat::test_that("sft_restore_record restores a deleted record to latest non-deleted version", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

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
    conn = conn,
    user = "tester"
  )

  updated <- update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  soft_delete_record(
    form = form,
    record_id = updated$sft_id[1],
    conn = conn,
    user = "tester"
  )

  visible_before <- fetch_records(
    form = form,
    conn = conn,
    include_deleted = FALSE
  )

  testthat::expect_equal(nrow(visible_before), 0L)

  restored <- restore_record(
    form = form,
    record_id = inserted$sft_id[1],
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(restored$name, "Ada Lovelace")
  testthat::expect_equal(restored$sft_is_deleted, 0L)

  visible_after <- fetch_records(
    form = form,
    conn = conn,
    include_deleted = FALSE
  )

  testthat::expect_equal(nrow(visible_after), 1L)
  testthat::expect_equal(visible_after$name, "Ada Lovelace")

  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action, version_no FROM sft_audit_log ORDER BY log_id"
  )

  testthat::expect_equal(
    audit$action,
    c("insert", "update", "delete", "restore")
  )

  testthat::expect_equal(
    audit$version_no,
    c(1L, 2L, 3L, 4L)
  )
})

testthat::test_that("sft_restore_record rejects a restore that collides on a unique field", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "restore_unique",
    table_name = "restore_unique",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )

  original <- insert_record(
    form = form,
    record = list(name = "Ada", email = "ada@example.org"),
    conn = conn,
    user = "tester"
  )

  # Soft-delete frees the unique value, so a live row can then claim it.
  soft_delete_record(
    form = form,
    record_id = original$sft_id[1],
    conn = conn,
    user = "tester"
  )

  insert_record(
    form = form,
    record = list(name = "Grace", email = "ada@example.org"),
    conn = conn,
    user = "tester"
  )

  # Reactivating the original would duplicate the now-live email: the friendly
  # pre-check rejects it instead of leaking the raw constraint error.
  testthat::expect_error(
    restore_record(
      form = form,
      record_id = original$sft_id[1],
      conn = conn,
      user = "tester"
    ),
    "already held by an active record"
  )

  # The original stays soft-deleted (transaction rolled back).
  original_after <- sft_get_record(
    conn = conn,
    form = form,
    record_id = original$sft_id[1],
    include_deleted = TRUE
  )

  testthat::expect_equal(original_after$sft_is_deleted, 1L)

  # The collision clears once the live holder is gone, and the restore succeeds.
  live_holder <- fetch_records(form, conn = conn, include_deleted = FALSE)
  soft_delete_record(
    form = form,
    record_id = live_holder$sft_id[live_holder$email == "ada@example.org"][1],
    conn = conn,
    user = "tester"
  )

  restored <- restore_record(
    form = form,
    record_id = original$sft_id[1],
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(restored$sft_is_deleted, 0L)
  testthat::expect_equal(restored$email, "ada@example.org")
})

testthat::test_that("sft_restore_record with reactivate = FALSE keeps the record deleted", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "restore_keep_deleted",
    table_name = "restore_keep_deleted",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(name = "v1", email = "a@example.org"),
    conn = conn,
    user = "tester"
  )
  record_id <- inserted$sft_id[1]

  update_record(
    form = form,
    record_id = record_id,
    values = list(name = "v2"),
    conn = conn,
    user = "tester"
  )

  soft_delete_record(
    form = form,
    record_id = record_id,
    conn = conn,
    user = "tester"
  )

  # A live record now holds the same unique email.
  insert_record(
    form = form,
    record = list(name = "live", email = "a@example.org"),
    conn = conn,
    user = "tester"
  )

  # Restore version 1's values without reactivating: this must not revive the
  # record or reclaim the live unique slot, so it succeeds despite the live
  # holder of the email.
  restored <- restore_record(
    form = form,
    record_id = record_id,
    version_no = 1L,
    conn = conn,
    user = "tester",
    reactivate = FALSE
  )

  testthat::expect_equal(restored$name, "v1")
  testthat::expect_equal(restored$sft_is_deleted, 1L)

  row <- DBI::dbGetQuery(
    conn,
    paste0(
      "SELECT sft_is_deleted, sft_unique_slot FROM restore_keep_deleted ",
      "WHERE sft_id = ?"
    ),
    params = list(record_id)
  )

  # The record stays soft-deleted, holding slot = sft_id rather than the live 0.
  testthat::expect_equal(row$sft_is_deleted, 1L)
  testthat::expect_equal(row$sft_unique_slot, record_id)

  # The live record is untouched and still the only visible row.
  live <- fetch_records(form = form, conn = conn, include_deleted = FALSE)
  testthat::expect_equal(nrow(live), 1L)
  testthat::expect_equal(live$name, "live")
})

testthat::test_that("sft_restore_record can restore a specific version", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

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
    conn = conn,
    user = "tester"
  )

  update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  restored <- restore_record(
    form = form,
    record_id = inserted$sft_id[1],
    version_no = 1L,
    conn = conn,
    user = "tester"
  )

  testthat::expect_equal(restored$name, "Ada")
  testthat::expect_equal(restored$sft_is_deleted, 0L)

  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action, version_no FROM sft_audit_log ORDER BY log_id"
  )

  testthat::expect_equal(
    audit$action,
    c("insert", "update", "restore")
  )

  testthat::expect_equal(
    audit$version_no,
    c(1L, 2L, 3L)
  )
})


testthat::test_that("sft_list_restorable_versions excludes delete actions", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

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
    conn = conn,
    user = "tester"
  )

  update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )

  soft_delete_record(
    form = form,
    record_id = inserted$sft_id[1],
    conn = conn,
    user = "tester"
  )

  versions <- list_versions(
    form = form,
    conn = conn,
    record_id = inserted$sft_id[1]
  )

  restorable <- list_restorable_versions(
    form = form,
    conn = conn,
    record_id = inserted$sft_id[1]
  )

  testthat::expect_equal(
    versions$action,
    c("insert", "update", "delete")
  )

  testthat::expect_equal(
    restorable$action,
    c("insert", "update")
  )
})

testthat::test_that("audit log enforces a unique version per record", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "audit_unique",
    table_name = "audit_unique",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

  init_db(form, conn = conn)

  # The guarding unique index is created during system-table init.
  testthat::expect_true(
    "sft_audit_log_version_idx" %in% sft_list_index_names(conn, "sft_audit_log")
  )

  rec <- insert_record(form, list(name = "a"), conn = conn)
  record_id <- rec$sft_id[1]

  # The insert already wrote version_no 1 for this record. Forcing a second row
  # with the same (form_id, table_name, record_id, version_no) simulates a
  # MAX+1 race and must be rejected by the unique index rather than duplicated.
  columns <- c("form_id", "table_name", "record_id", "action", "version_no", "changed_at")
  values <- list(form$form_id, form$table_name, record_id, "update", 1L, sft_now())
  prepared <- sft_prepend_explicit_id(conn, "sft_audit_log", "log_id", columns, values)

  testthat::expect_error(
    DBI::dbExecute(
      conn,
      paste0(
        "INSERT INTO sft_audit_log (",
        paste(
          vapply(prepared$columns, function(x) sft_quote_identifier(conn, x), character(1)),
          collapse = ", "
        ),
        ") VALUES (",
        paste(rep("?", length(prepared$values)), collapse = ", "),
        ")"
      ),
      params = prepared$values
    )
  )
})