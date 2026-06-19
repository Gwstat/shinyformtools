# Runtime coverage for the extracted deleted-records / versions-restore flow
# (sft_register_deleted_versions, wired into form_server). Drives the module
# server with testServer so the moved reactives and observers actually execute.

testthat::test_that("deleted-records and restore flow run end to end in the module", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "dv_flow",
    table_name = "dv_flow",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

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
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "dv_versions",
    table_name = "dv_versions",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

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
