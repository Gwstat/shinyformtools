# Validation reports structured issues (fields, severity, message, source).
# validate_record() keeps its messages and its "TRUE or error" contract, but the
# error is classed and carries the issues, and the form module uses them to
# glow the fields a rejected save complained about.

sft_test_issues_form <- function(db_path = tempfile(fileext = ".sqlite")) {
  form(
    form_id = "issues",
    table_name = "issues",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "Email", unique = TRUE),
      form_field(id = "status", label = "Status"),
      form_field(id = "reason", label = "Reason")
    ),
    validation_rules = list(
      required_if(
        id = "reason_when_rejected",
        condition = function(values) identical(values$status, "rejected"),
        fields = "reason",
        message = "A rejection needs a reason."
      ),
      warning_if(
        id = "no_email",
        condition = function(values) sft_is_empty_value(values$email),
        fields = "email",
        message = "No email address given."
      )
    )
  )
}

test_that("validation_issues lists every issue with its fields, severity and source", {
  f <- sft_test_issues_form()

  issues <- validation_issues(f, list(name = "", email = "", status = "rejected", reason = ""))

  expect_s3_class(issues, "data.frame")
  expect_identical(issues$source, c("mandatory", "rule:reason_when_rejected", "rule:no_email"))
  expect_identical(issues$severity, c("error", "error", "warning"))
  expect_identical(unclass(issues$fields), list("name", "reason", "email"))
  expect_match(issues$message[1], "Mandatory fields missing: name")
  expect_identical(issues$message[2], "A rejection needs a reason.")

  # A valid record has no issues, and the frame keeps its columns.
  clean <- validation_issues(f, list(name = "Ada", email = "a@b.c", status = "ok", reason = ""))
  expect_identical(nrow(clean), 0L)
  expect_identical(names(clean), c("severity", "source", "message", "fields"))
})

test_that("unique issues need a connection and name the field", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  f <- sft_test_issues_form(db_path)
  init_db(f, conn = conn)
  rec <- insert_record(f, list(name = "Ada", email = "a@b.c"), conn = conn)

  taken <- validation_issues(f, list(name = "Bob", email = "a@b.c"), conn = conn)
  expect_identical(taken$source, "unique")
  expect_identical(unclass(taken$fields), list("email"))

  # The record itself is not its own duplicate.
  own <- validation_issues(f, list(name = "Ada", email = "a@b.c"), conn = conn, record_id = rec$sft_id[1])
  expect_identical(nrow(own), 0L)

  # Without a connection the unique check is skipped, as in validate_record().
  expect_identical(nrow(validation_issues(f, list(name = "Bob", email = "a@b.c"))), 0L)
})

test_that("validate_record raises a classed error with the same message and the issues", {
  f <- sft_test_issues_form()
  bad <- list(name = "", email = "x@y.z", status = "rejected", reason = "")

  cond <- tryCatch(validate_record(f, bad), error = function(e) e)

  expect_s3_class(cond, "sft_validation_error")
  expect_s3_class(cond, "error")
  # The message is what it always was: every error, one per line.
  expect_identical(
    conditionMessage(cond),
    "Mandatory fields missing: name.\nA rejection needs a reason."
  )
  expect_identical(cond$fields, c("name", "reason"))
  expect_identical(cond$issues$source, c("mandatory", "rule:reason_when_rejected"))

  # Warnings still warn and do not stop.
  expect_warning(
    ok <- validate_record(f, list(name = "Ada", email = "", status = "ok", reason = "")),
    "No email address given"
  )
  expect_true(ok)
})

test_that("update_record classes its empty-mandatory refusal too", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  f <- sft_test_issues_form(db_path)
  init_db(f, conn = conn)
  rec <- insert_record(f, list(name = "Ada", email = "a@b.c"), conn = conn)

  cond <- tryCatch(
    update_record(f, record_id = rec$sft_id[1], values = list(name = ""), conn = conn),
    error = function(e) e
  )

  expect_s3_class(cond, "sft_validation_error")
  expect_identical(cond$fields, "name")
  expect_match(conditionMessage(cond), "Mandatory fields are empty: name")
})

test_that("a rejected save marks the fields in the module and a good save clears them", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_issues_form(db_path)

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
    args = list(id = "issues", form = f, user = function() "alice"),
    {
      session$flushReact()
      expect_identical(state$invalid_fields(), character())

      session$setInputs(
        add_name = "", add_email = "a@b.c", add_status = "rejected", add_reason = ""
      )
      session$setInputs(submit_add = 1)

      expect_setequal(state$invalid_fields(), c("name", "reason"))
      expect_equal(nrow(fetch_records(f, conn = state$conn())), 0L)

      # The stylesheet glows exactly those two add-form containers.
      css <- paste(as.character(output$sft_highlight_style$html), collapse = "")
      expect_match(css, "issues-sft_field_container_add_name", fixed = TRUE)
      expect_match(css, "issues-sft_field_container_add_reason", fixed = TRUE)
      expect_false(grepl("issues-sft_field_container_add_status", css, fixed = TRUE))

      # Fixing the record saves it and clears the marks.
      session$setInputs(add_name = "Ada", add_reason = "duplicate")
      session$setInputs(submit_add = 2)

      expect_identical(state$invalid_fields(), character())
      expect_equal(nrow(fetch_records(f, conn = state$conn())), 1L)

      # Opening a form again starts clean.
      state$invalid_fields("name")
      session$setInputs(open_add = 1)
      expect_identical(state$invalid_fields(), character())
    }
  )

  expect_true(any(grepl("^error: Mandatory fields missing: name", shown)))
})

test_that("highlight = list(invalid = FALSE) keeps the marks out of the stylesheet", {
  skip_if_not_installed("DT")

  f <- sft_test_issues_form()

  shiny::testServer(
    form_server,
    args = list(id = "issues", form = f, highlight = list(invalid = FALSE)),
    {
      session$flushReact()
      state$invalid_fields("name")
      session$flushReact()

      css <- paste(as.character(output$sft_highlight_style$html), collapse = "")
      expect_false(grepl("issues-sft_field_container_add_name", css, fixed = TRUE))
    }
  )
})
