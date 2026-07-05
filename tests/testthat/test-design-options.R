testthat::test_that("button options normalize placement and classes", {
  options <- sft_normalize_button_options(
    list(
      placement = "bottom",
      align = "right",
      class = "btn-sm",
      button_classes = c(open_add = "btn-success")
    )
  )

  testthat::expect_equal(options$placement, "bottom")
  testthat::expect_equal(options$align, "right")
  testthat::expect_equal(options$button_classes$open_add, "btn-success")

  testthat::expect_error(
    sft_normalize_button_options(list(placement = "side")),
    "button_options\\$placement"
  )
})

testthat::test_that("records datatable accepts classes and table_format", {
  form <- form(
    form_id = "design_options_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "status", label = "Status")
    )
  )

  data <- data.frame(
    sft_easy_id = "D-000001",
    name = "Ada",
    status = "offen",
    stringsAsFactors = FALSE
  )

  table <- records_datatable(
    data = data,
    form = form,
    columns = c("sft_easy_id", "name", "status"),
    class = "display compact stripe"
  )

  testthat::expect_s3_class(table, "datatables")

  formatted <- sft_apply_table_format(
    table = table,
    data = data,
    table_format = function(table) {
      DT::formatStyle(
        table,
        "Status",
        backgroundColor = DT::styleEqual("offen", "transparent")
      )
    }
  )

  testthat::expect_s3_class(formatted, "datatables")

  testthat::expect_error(
    sft_apply_table_format(
      table = table,
      data = data,
      table_format = function(table) data.frame()
    ),
    "table_format must return a DT table widget"
  )
})

testthat::test_that("external form buttons use the module namespace", {
  buttons <- form_buttons(
    id = "external",
    show_edit = FALSE,
    show_delete = FALSE,
    show_versions = FALSE,
    show_deleted_records = FALSE,
    show_column_settings = FALSE,
    show_column_selection = FALSE,
    labels = list(open_add = "Neu")
  )

  html <- paste(as.character(buttons), collapse = "")
  testthat::expect_true(grepl('id="external-open_add"', html, fixed = TRUE))
  testthat::expect_true(grepl("Neu", html, fixed = TRUE))
  testthat::expect_false(grepl('id="external-open_edit"', html, fixed = TRUE))
})

testthat::test_that("default DT options avoid forced horizontal scrolling", {
  options <- sft_dt_options()

  testthat::expect_false(options$scrollX)
  testthat::expect_false(options$autoWidth)
  testthat::expect_equal(options$pageLength, 10)

  custom <- sft_dt_options(list(scrollX = TRUE, pageLength = 5))
  testthat::expect_true(custom$scrollX)
  testthat::expect_equal(custom$pageLength, 5)
})

testthat::test_that("records datatable preserves DT client state by default", {
  form <- form(
    form_id = "dt_state_test",
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

  table <- records_datatable(
    data = data.frame(
      sft_easy_id = "D-000001",
      name = "Ada",
      stringsAsFactors = FALSE
    ),
    form = form,
    columns = c("sft_easy_id", "name")
  )

  testthat::expect_true(isTRUE(table$x$options$stateSave))
})


testthat::test_that("column controls use a single visible button", {
  buttons <- form_buttons(
    id = "columns",
    show_add = FALSE,
    show_edit = FALSE,
    show_delete = FALSE,
    show_versions = FALSE,
    show_deleted_records = FALSE,
    show_column_settings = TRUE,
    show_column_selection = TRUE
  )

  html <- paste(as.character(buttons), collapse = "")
  testthat::expect_true(grepl('id="columns-open_column_selection"', html, fixed = TRUE))
  testthat::expect_false(grepl('id="columns-open_column_settings"', html, fixed = TRUE))
})

testthat::test_that("standalone versions button is not rendered by default button row", {
  buttons <- form_buttons(
    id = "versions",
    show_add = FALSE,
    show_edit = FALSE,
    show_delete = FALSE,
    show_refresh_table = FALSE,
    show_versions = TRUE,
    show_deleted_records = FALSE,
    show_column_settings = FALSE,
    show_column_selection = FALSE
  )

  html <- paste(as.character(buttons), collapse = "")
  testthat::expect_false(grepl('id="versions-open_versions"', html, fixed = TRUE))
})
