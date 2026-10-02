# Validation warnings (warning_if() rules, on_edit_missing_required = "warn")
# are raised with base warning(). run_mutation() must show them to the user and
# still complete the save; before the handler existed they went to the R
# console only, so "warn" behaved exactly like "ignore" inside the module.

sft_test_warning_form <- function(db_path) {
  form(
    form_id = "warnings",
    table_name = "warnings",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "note", label = "Note", mandatory = TRUE)
    ),
    validation_rules = list(
      validation_rule(
        id = "short_note",
        validate = function(values) nchar(values$note %||% "") >= 3L,
        severity = "warning",
        message = "The note is very short."
      )
    )
  )
}

test_that("a validation warning is shown as a notification and the record is still saved", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  notes <- sft_test_warning_form(db_path)

  shown <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown <<- c(shown, paste0(type, ": ", as.character(ui)))
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(
    form_server,
    args = list(id = "warnings", form = notes, user = function() "alice"),
    {
      session$flushReact()
      session$setInputs(add_note = "x")
      session$setInputs(submit_add = 1)

      stored <- fetch_records(notes, conn = conn)
      expect_identical(stored$note, "x")
    }
  )

  expect_true(any(grepl("^warning: .*very short", shown)))
  # The success notification still follows: a warning does not block the save.
  expect_true(any(grepl("^message: ", shown)))
})

test_that("a validation error still keeps the dialog open and writes nothing", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  notes <- sft_test_warning_form(db_path)

  shown <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown <<- c(shown, paste0(type, ": ", as.character(ui)))
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(
    form_server,
    args = list(id = "warnings", form = notes, user = function() "alice"),
    {
      session$flushReact()
      session$setInputs(add_note = "")
      session$setInputs(submit_add = 1)

      expect_equal(nrow(fetch_records(notes, conn = conn)), 0L)
    }
  )

  expect_true(any(grepl("^error: ", shown)))
  expect_false(any(grepl("^message: ", shown)))
})
