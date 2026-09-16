# Extracted from test-deleted-versions.R:43

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
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
live <- fetch_records(form, conn = conn, include_deleted = FALSE)
testthat::expect_true(record_id %in% live$sft_id)
