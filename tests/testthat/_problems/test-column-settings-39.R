# Extracted from test-column-settings.R:39

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "cs_flow",
    table_name = "cs_flow",
    db_path = db_path,
    easy_id = TRUE,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "city", label = "City")
    )
  )
init_db(form, conn = conn)
insert_record(form, list(name = "Ada", city = "London"), conn = conn)
shiny::testServer(
    form_server,
    args = list(form = form, conn = conn),
    {
      # The widget renderUI exercises resolve_column_view_columns /
      # column_choices; a missing threaded dependency would error here.
      session$setInputs(column_settings_view = "Standard")
      testthat::expect_no_error(output$column_settings_widget_ui)

      # Save a named shared view with a reduced column set.
      session$setInputs(
        column_settings_order = c("sft_easy_id", "name"),
        column_settings_view_name = "NameOnly",
        save_column_view = 1L
      )

      testthat::expect_equal(active_column_view(), "NameOnly")
      testthat::expect_true(all(c("sft_easy_id", "name") %in% record_columns()))
      testthat::expect_false("city" %in% record_columns())

      # Load the Standard view back through the load observer.
      session$setInputs(column_settings_view = "Standard")
      session$setInputs(load_column_view = 1L)
      testthat::expect_equal(active_column_view(), "Standard")
    }
  )
