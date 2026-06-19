# Runtime coverage for the extracted column-settings / column-view glue
# (sft_register_column_settings, wired into form_server). Drives the module
# server with testServer so the moved renderUI and observers actually execute.

testthat::test_that("column-settings widget, save, and load run in the module", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "cs_flow",
    table_name = "cs_flow",
    db_path = db_path,
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

  # The saved view persisted to the database.
  view_names <- sft_column_view_names(
    table_views = NULL,
    persist_column_settings = TRUE,
    conn = conn,
    form = form
  )
  testthat::expect_true("NameOnly" %in% view_names)
})

testthat::test_that("the Standard view cannot be overwritten via save", {
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

  # The rejected save persisted no shared view.
  saved <- sft_available_shared_column_view_names(conn = conn, form = form)
  testthat::expect_length(saved, 0L)
})
