testthat::test_that("modal_header can read values and context", {
  form <- form(
    form_id = "modal_header_test",
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

  ui <- sft_call_modal_header(
    modal_header = function(values, context, prefix) {
      paste0(prefix, values$name, ":", context$extra)
    },
    form = form,
    prefix = "edit_",
    values = list(name = "Rathaus"),
    context = list(extra = "linked")
  )

  testthat::expect_equal(ui, "edit_Rathaus:linked")
})

testthat::test_that("modal input values prefer live inputs over record values", {
  form <- form(
    form_id = "modal_values_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "phone", label = "Telefon")
    )
  )

  input <- list(
    edit_name = "Live Name"
  )

  record <- data.frame(
    name = "Stored Name",
    phone = "0391 111",
    stringsAsFactors = FALSE
  )

  values <- sft_modal_input_values(
    form = form,
    input = input,
    prefix = "edit_",
    record = record
  )

  testthat::expect_equal(values$name, "Live Name")
  testthat::expect_equal(values$phone, "0391 111")
})
