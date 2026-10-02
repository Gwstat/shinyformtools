test_that("a record's empty values render as empty inputs, not as the inputs' defaults", {
  f <- form(
    form_id = "blank", table_name = "blank", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day", input_type = "dateInput"),
      form_field(id = "result", label = "Result", input_type = "radioButtons",
                 args = list(choices = c("pass", "fail"))),
      form_field(id = "count", label = "Count", input_type = "numericInput", args = list(value = 0)),
      form_field(id = "kind", label = "Kind", input_type = "selectInput",
                 args = list(choices = c("a", "b"))),
      form_field(id = "done", label = "Done", input_type = "checkboxInput", args = list(value = TRUE)),
      form_field(id = "span", label = "Span", input_type = "dateRangeInput")
    )
  )
  field <- function(id) Filter(function(x) identical(x$id, id), f$fields)[[1]]
  args <- function(id, value) sft_prepare_input_args(field(id), value = value)

  # Stored empty (NA): empty inputs.
  expect_true(is.na(args("day", NA)$value))
  expect_identical(args("result", NA)$selected, character(0))
  expect_true(is.na(args("count", NA)$value))
  expect_identical(args("kind", NA)$selected, character(0))
  expect_false(args("done", NA)$value)
  expect_true(all(is.na(c(args("span", NA)$start, args("span", NA)$end))))

  html <- as.character(render_field(field("result"), value = NA))
  expect_false(grepl("checked", html, fixed = TRUE))

  # A new record (no stored value): the defaults stay.
  expect_identical(args("count", NULL)$value, 0)
  expect_true(args("done", NULL)$value)

  # A stored value is shown as it is.
  expect_identical(args("count", 7)$value, 7)
  expect_identical(args("result", "fail")$selected, "fail")
})

test_that("saving the edit dialog does not fill empty fields that have no empty state", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  f <- form(
    form_id = "untouched", table_name = "untouched", db_path = db_path,
    fields = list(
      form_field(id = "title", label = "Title"),
      form_field(id = "done", label = "Done", input_type = "checkboxInput"),
      form_field(id = "range", label = "Range", input_type = "sliderInput",
                 args = list(min = 0, max = 10, value = c(1, 2)))
    )
  )
  init_db(f, conn = conn)
  insert_record(f, list(title = "a"), conn = conn)

  # What the dialog shows for the empty fields comes back unchanged.
  shiny::testServer(form_server, args = list(form = f, conn = conn), {
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_edit = 1L)
    session$setInputs(edit_title = "a2", edit_done = FALSE, edit_range = c(1, 2))
    session$setInputs(submit_edit = 1L)
  })
  stored <- fetch_records(f, conn = conn)
  expect_identical(stored$title, "a2")
  expect_true(is.na(stored$done))
  expect_true(is.na(stored$range))

  # A value the user sets is stored.
  shiny::testServer(form_server, args = list(form = f, conn = conn), {
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_edit = 1L)
    session$setInputs(edit_title = "a2", edit_done = TRUE, edit_range = c(3, 4))
    session$setInputs(submit_edit = 1L)
  })
  stored <- fetch_records(f, conn = conn)
  expect_identical(as.integer(stored$done), 1L)
  expect_identical(sft_ui_value(f$fields[[3]], stored$range), c(3, 4))
})

test_that("a numericInput whose args leave out value renders, empty", {
  f <- form(
    form_id = "novalue", table_name = "novalue", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(form_field(id = "days", label = "Days", input_type = "numericInput",
                             args = list(min = 0, max = 90, step = 1)))
  )
  expect_no_error(html <- as.character(render_form_fields(f, prefix = "add_")))
  expect_match(html, 'id="add_days"', fixed = TRUE)
  expect_true(is.na(sft_prepare_input_args(f$fields[[1]])$value))
})

test_that("an empty date renders without Shiny's coercion warning", {
  f <- form(
    form_id = "nodate", table_name = "nodate", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day", input_type = "dateInput"),
      form_field(id = "span", label = "Span", input_type = "dateRangeInput")
    )
  )
  expect_no_warning(html <- as.character(render_form_fields(f, values = list(day = NA, span = NA))))
  expect_match(html, 'data-initial-date(="")?[ />]')
})
