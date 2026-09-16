# Extracted from test-column-settings.R:87

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "cs_standard",
    table_name = "cs_standard",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )
init_db(form, conn = conn)
insert_record(form, list(name = "Ada"), conn = conn)
shiny::testServer(
    form_server,
    args = list(form = form, conn = conn),
    {
      # Empty / "Standard" view name is rejected, leaving the active view as is.
      session$setInputs(
        column_settings_order = c("name"),
        column_settings_view_name = "Standard",
        save_column_view = 1L
      )

      testthat::expect_equal(active_column_view(), "Standard")
    }
  )
