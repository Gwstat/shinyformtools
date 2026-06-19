testthat::test_that("show = FALSE fields are not rendered or collected from module inputs", {
  form <- form(
    form_id = "field_visibility_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "internal_note", label = "Internal", show = FALSE)
    )
  )

  ui <- render_form_fields(
    form = form,
    ns = identity,
    prefix = "add_"
  )

  html <- paste(as.character(ui), collapse = "")
  testthat::expect_true(grepl('add_name', html, fixed = TRUE))
  testthat::expect_false(grepl('add_internal_note', html, fixed = TRUE))

  input <- list(
    add_name = "Ada",
    add_internal_note = "client-side tampering"
  )

  values <- collect_input_values(
    form = form,
    input = input,
    prefix = "add_"
  )

  testthat::expect_equal(names(values), "name")
  testthat::expect_equal(values$name, "Ada")
})

testthat::test_that("editable = FALSE fields are visible but not collected for edit updates", {
  form <- form(
    form_id = "field_editability_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "created_note", label = "Created note", editable = FALSE)
    )
  )

  ui <- render_form_fields(
    form = form,
    ns = identity,
    prefix = "edit_",
    values = list(name = "Ada", created_note = "initial")
  )

  html <- paste(as.character(ui), collapse = "")
  testthat::expect_true(grepl('edit_name', html, fixed = TRUE))
  testthat::expect_true(grepl('edit_created_note', html, fixed = TRUE))

  input <- list(
    edit_name = "Ada Lovelace",
    edit_created_note = "client-side tampering"
  )

  values <- collect_input_values(
    form = form,
    input = input,
    prefix = "edit_",
    editable_only = TRUE
  )

  testthat::expect_equal(names(values), "name")
  testthat::expect_equal(values$name, "Ada Lovelace")
})

testthat::test_that("records table data uses display formatting without rebuilding the widget", {
  form <- form(
    form_id = "records_table_data_test",
    fields = list(
      form_field(
        id = "topics",
        label = "Themen",
        input_type = "checkboxGroupInput"
      )
    )
  )

  data <- data.frame(
    sft_easy_id = "D-000001",
    topics = '["A","B"]',
    stringsAsFactors = FALSE
  )

  out <- sft_records_table_data(
    data = data,
    form = form,
    columns = c("sft_easy_id", "topics")
  )

  testthat::expect_equal(names(out), c("ID", "Themen"))
  testthat::expect_equal(out$Themen, "A; B")
})
