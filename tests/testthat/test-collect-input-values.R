test_that("a cleared multi-value input stays in the collected values as empty", {
  tags_form <- form(
    form_id = "t", table_name = "t", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "tags", label = "Tags", input_type = "checkboxGroupInput",
                 args = list(choices = c("A", "B")), mandatory = TRUE),
      form_field(id = "day", label = "Day", input_type = "dateInput")
    )
  )
  # What Shiny's input holds after the user unticks everything and clears the
  # date: NULL for both.
  input <- list(edit_name = "x")

  # Default as before 0.4.0: empty inputs are left out.
  expect_false(any(c("tags", "day") %in% names(collect_input_values(tags_form, input, prefix = "edit_"))))

  values <- collect_input_values(tags_form, input, prefix = "edit_", keep_empty = TRUE)
  expect_true(all(c("tags", "day") %in% names(values)))
  expect_null(values$tags)

  conn <- local_test_conn(tags_form$db)
  insert_record(tags_form, list(name = "x", tags = c("A", "B"), day = as.Date("2026-01-01")), conn = conn)
  update_record(tags_form, values[c("name", "day")], record_id = 1, conn = conn)
  expect_true(is.na(fetch_records(tags_form, conn = conn)$day))

  # The cleared mandatory multi-select is caught instead of passing.
  expect_error(update_record(tags_form, values, record_id = 1, conn = conn), class = "sft_validation_error")
})
