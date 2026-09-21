# Structured validation results.
#
# Every check (mandatory fields, unique fields, validation rules) reports
# "issues": which fields, how severe, what message, which check. The messages
# are the same strings validate_record() has always raised; the structure
# around them is what lets a caller attribute a failure to fields - the form
# module glows them, a script can branch on them - instead of parsing text.

# One issue. `fields` are field ids (character(0) when a rule names none).
sft_issue <- function(fields, severity, message, source) {
  list(
    fields = as.character(fields %||% character()),
    severity = severity,
    message = as.character(message),
    source = source
  )
}

sft_mandatory_issues <- function(form, record) {
  missing <- sft_missing_mandatory_fields(form, record)

  if (length(missing) == 0L) {
    return(list())
  }

  list(sft_issue(
    fields = missing,
    severity = "error",
    message = sft_message(
      form = form,
      key = "mandatory_missing",
      values = list(fields = paste(missing, collapse = ", "))
    ),
    source = "mandatory"
  ))
}

# All issues of one record, in the order validate_record() reports them:
# mandatory, unique (needs a connection), rules.
sft_validation_issues <- function(form,
                                  record,
                                  conn = NULL,
                                  current_id = NULL,
                                  require_all_mandatory = TRUE) {
  issues <- list()

  if (isTRUE(require_all_mandatory)) {
    issues <- c(issues, sft_mandatory_issues(form, record))
  }

  if (!is.null(conn)) {
    issues <- c(
      issues,
      sft_unique_field_issues(
        form = form,
        record = record,
        conn = conn,
        current_id = current_id
      )
    )
  }

  c(
    issues,
    sft_rule_issues(
      form = form,
      record = record,
      conn = conn,
      current_id = current_id
    )
  )
}

sft_issue_messages <- function(issues, severity) {
  keep <- Filter(function(issue) identical(issue$severity, severity), issues)
  unlist(lapply(keep, function(issue) issue$message), use.names = FALSE) %||% character()
}

sft_issue_fields <- function(issues, severity = "error") {
  keep <- Filter(function(issue) identical(issue$severity, severity), issues)
  unique(unlist(lapply(keep, function(issue) issue$fields), use.names = FALSE)) %||% character()
}

# Issues as a data frame: one row per issue, `fields` a list column.
sft_issues_data_frame <- function(issues) {
  out <- data.frame(
    severity = vapply(issues, function(issue) issue$severity, character(1)),
    source = vapply(issues, function(issue) issue$source, character(1)),
    message = vapply(
      issues,
      function(issue) paste(issue$message, collapse = "\n"),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  out$fields <- I(lapply(issues, function(issue) issue$fields))
  out
}

# The error validate_record() raises. Its message is the familiar one (every
# error message, one per line); `issues` carries the structure, and the class
# lets a caller catch validation failures apart from database errors.
sft_validation_error <- function(issues) {
  errors <- Filter(function(issue) identical(issue$severity, "error"), issues)

  structure(
    class = c("sft_validation_error", "error", "condition"),
    list(
      message = paste(sft_issue_messages(errors, "error"), collapse = "\n"),
      call = NULL,
      issues = sft_issues_data_frame(errors),
      fields = sft_issue_fields(errors, "error")
    )
  )
}

#' List the validation issues of a record
#'
#' The non-throwing counterpart of [validate_record()]: instead of stopping at
#' the first failed record it returns every issue with the fields it concerns,
#' so a script (or a custom UI) can react to them individually.
#'
#' @inheritParams validate_record
#'
#' @return A data frame with one row per issue and the columns `severity`
#'   (`"error"` or `"warning"`), `source` (`"mandatory"`, `"unique"` or
#'   `"rule:<id>"`), `message` and `fields` (a list column of field ids; empty
#'   for a rule that names no fields). Zero rows mean the record is valid.
#' @seealso [validate_record()], which raises the errors among these issues as
#'   a condition of class `sft_validation_error` carrying the same data frame
#'   in its `issues` element.
#' @examples
#' f <- form(
#'   form_id = "contacts",
#'   table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name", mandatory = TRUE),
#'     form_field(id = "email", label = "Email")
#'   ),
#'   validation_rules = list(
#'     warning_if(
#'       id = "no_email",
#'       condition = function(values) !nzchar(values$email),
#'       fields = "email",
#'       message = "No email address given."
#'     )
#'   )
#' )
#'
#' issues <- validation_issues(f, list(name = "", email = ""))
#' issues[, c("severity", "source", "message")]
#' issues$fields
#' @export
validation_issues <- function(form,
                              record,
                              conn = NULL,
                              record_id = NULL,
                              require_all_mandatory = TRUE) {
  record <- sft_check_validation_input(form, record)

  sft_issues_data_frame(
    sft_validation_issues(
      form = form,
      record = record,
      conn = conn,
      current_id = record_id,
      require_all_mandatory = require_all_mandatory
    )
  )
}

# Shared argument checks of validate_record() / validation_issues(); returns
# the record as a named list.
sft_check_validation_input <- function(form, record) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (is.data.frame(record)) {
    if (nrow(record) != 1L) {
      stop("record data frames must have exactly one row.", call. = FALSE)
    }

    record <- sft_row_to_list(record)
  }

  if (!is.list(record)) {
    stop("record must be a named list or a one-row data frame.", call. = FALSE)
  }

  if (is.null(names(record)) || any(!nzchar(names(record)))) {
    stop("record must be named.", call. = FALSE)
  }

  record
}
