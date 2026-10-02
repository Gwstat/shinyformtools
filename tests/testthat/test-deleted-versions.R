# Runtime coverage for the extracted deleted-records / versions-restore flow
# (sft_register_deleted_versions, wired into form_server). Drives the module
# server with testServer so the moved reactives and observers actually execute.

testthat::test_that("deleted-records and restore flow run end to end in the module", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- test_form_name("dv_flow", db_path = db_path)

  init_db(form, conn = conn)

  rec <- insert_record(form, list(name = "v1"), conn = conn)
  record_id <- rec$sft_id[1]
  # Create a second version, then soft-delete so the record is restorable.
  update_record(form, record_id = record_id, values = list(name = "v2"), conn = conn)
  soft_delete_record(form, record_id = record_id, conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn),
    {
      # Renderers exercise display_deleted_records -> deleted_records and
      # current_record_columns; a missing dependency would error here.
      testthat::expect_no_error(output$deleted_records)

      # Select the soft-deleted record and restore it in one click (latest
      # version), without going through a version picker.
      session$setInputs(deleted_records_rows_selected = 1L)
      session$setInputs(restore_deleted = 1L)
    }
  )

  # The record is no longer soft-deleted after the restore observer ran.
  live <- fetch_records(form, conn = conn, include_deleted = FALSE)
  testthat::expect_true(record_id %in% live$sft_id)
})

testthat::test_that("opening versions for a live record sets up the restore list", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- test_form_name("dv_versions", db_path = db_path)

  init_db(form, conn = conn)
  rec <- insert_record(form, list(name = "a"), conn = conn)
  record_id <- rec$sft_id[1]
  update_record(form, record_id = record_id, values = list(name = "b"), conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn),
    {
      # Select the live record in the main table, then open its versions.
      session$setInputs(records_rows_selected = 1L)
      session$setInputs(open_versions = 1L)

      # The versions output renders without error for the selected record.
      testthat::expect_no_error(output$restore_versions)
    }
  )
})

testthat::test_that("deleted records and versions are not served without permission", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- test_form_name("dv_gate", db_path = db_path)

  init_db(form, conn = conn)

  live <- insert_record(form, list(name = "live_v1"), conn = conn)
  update_record(form, record_id = live$sft_id[1], values = list(name = "live_v2"), conn = conn)

  gone <- insert_record(form, list(name = "secret_deleted"), conn = conn)
  soft_delete_record(form, record_id = gone$sft_id[1], conn = conn)

  shiny::testServer(
    form_server,
    args = list(
      form = form,
      conn = conn,
      permissions = list(
        can_view_deleted_records = FALSE,
        can_view_versions = FALSE
      )
    ),
    {
      # Outputs compute as soon as something subscribes, so a client-injected
      # binding would receive whatever the renderers emit. With the permissions
      # denied, neither payload may contain any record data.
      session$setInputs(records_rows_selected = 1L)
      session$setInputs(open_edit = 1L)

      deleted_payload <- paste(unlist(output$deleted_records), collapse = " ")
      testthat::expect_false(grepl("secret_deleted", deleted_payload, fixed = TRUE))

      versions_payload <- paste(unlist(output$restore_versions), collapse = " ")
      testthat::expect_false(grepl("live_v1", versions_payload, fixed = TRUE))
    }
  )
})

testthat::test_that("restoring a version needs can_edit and respects locked fields", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  form <- form(
    form_id = "dv_rights", table_name = "dv_rights", db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "salary", label = "Salary", input_type = "numericInput",
                 editable = function(user) identical(user, "admin"))
    )
  )
  init_db(form, conn = conn)
  rec <- insert_record(form, list(name = "a", salary = 1), conn = conn)
  update_record(form, record_id = rec$sft_id[1], values = list(name = "b", salary = 2), conn = conn, user = "admin")
  shown <- NULL
  testthat::local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  restore_first <- function(session) {
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_versions = 1L)
    versions <- list_restorable_versions(form, conn = conn, record_id = 1)
    session$setInputs(restore_versions_rows_selected = which(versions$version_no == 1L))
    session$setInputs(confirm_restore = 1L)
  }

  # A reader without can_edit: refused.
  shiny::testServer(form_server, args = list(form = form, conn = conn, user = "admin",
                                             permissions = list(can_edit = FALSE)), {
    restore_first(session)
    testthat::expect_identical(fetch_records(form, conn = conn)$name, "b")
  })

  # A user who may not edit salary: version 1 differs in salary, refused.
  shiny::testServer(form_server, args = list(form = form, conn = conn, user = "ada"), {
    restore_first(session)
    testthat::expect_identical(fetch_records(form, conn = conn)$salary, 2)
    testthat::expect_match(shown, "Salary")
  })

  # admin may: restored.
  shiny::testServer(form_server, args = list(form = form, conn = conn, user = "admin"), {
    restore_first(session)
    testthat::expect_identical(fetch_records(form, conn = conn)$salary, 1)
  })
})

testthat::test_that("an injected include_deleted does not show deleted records without the permission", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  form <- test_form_name("dv_inject", db_path = db_path)
  init_db(form, conn = conn)
  insert_record(form, list(name = "live"), conn = conn)
  gone <- insert_record(form, list(name = "secret_deleted"), conn = conn)
  soft_delete_record(form, record_id = gone$sft_id[1], conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn, permissions = list(can_view_deleted_records = FALSE)),
    {
      session$setInputs(include_deleted = TRUE)
      testthat::expect_identical(session$returned$records()$name, "live")
    }
  )

  shiny::testServer(form_server, args = list(form = form, conn = conn), {
    session$setInputs(include_deleted = TRUE)
    testthat::expect_setequal(session$returned$records()$name, c("live", "secret_deleted"))
  })
})
