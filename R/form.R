sft_check_optional_named_or_unnamed_labels <- function(labels, name) {
  if (is.null(labels)) {
    return(invisible(TRUE))
  }

  if (!is.character(labels) && !is.list(labels)) {
    stop(name, " must be NULL, a character vector or a list.", call. = FALSE)
  }

  values <- unlist(labels, use.names = FALSE)

  if (!is.character(values) || any(is.na(values))) {
    stop(name, " must contain character values only.", call. = FALSE)
  }

  invisible(TRUE)
}

sft_check_form_region <- function(region, name) {
  if (is.null(region)) {
    return(invisible(TRUE))
  }

  if (is.function(region)) {
    return(invisible(TRUE))
  }

  if (is.character(region) && length(region) == 1L && !is.na(region)) {
    return(invisible(TRUE))
  }

  if (is.list(region)) {
    return(invisible(TRUE))
  }

  stop(
    name,
    " must be NULL, a character scalar, a Shiny tag/tagList, or a function.",
    call. = FALSE
  )
}

#' Define a form schema
#'
#' Creates a shinyformtools form schema from a list of field definitions.
#'
#' @param form_id Stable form identifier.
#' @param fields List of objects created with [form_field()].
#' @param table_name Database table name. Defaults to `form_id`.
#' @param db_path SQLite database path. Defaults to `"form_data.sqlite"`.
#'   Kept for backwards compatibility. Prefer `db = db_sqlite(...)` for
#'   new code.
#' @param db Database backend configuration created with `db_sqlite()` or
#'   `db_mariadb()` or `db_duckdb()`. If omitted, `db_path` is used as SQLite path.
#' @param form_name Optional user-facing form name.
#' @param version Integer-like form schema version.
#' @param schema_policy Schema handling policy. Currently `"safe"` or
#'   `"manual"`.
#' @param on_edit_missing_required How to treat new mandatory fields when old
#'   records are edited. One of `"warn"`, `"require"` or `"ignore"`.
#' @param tab_labels Optional labels for tab indices. Named labels use the tab
#'   value as name; unnamed labels are interpreted as zero-based layout labels.
#' @param slide_labels Optional labels for slide indices. Named labels use the
#'   slide value as name; unnamed labels are interpreted as zero-based layout
#'   labels.
#' @param messages Optional named list of validation messages.
#' @param validation_rules Optional list of objects created with
#'   [validation_rule()] or [required_if()]. Rules are evaluated
#'   server-side during insert and update.
#' @param header Optional form header. Can be a HTML string, Shiny UI object, or
#'   function returning Shiny UI. Function arguments may include `ns`, `prefix`,
#'   `values` and `form`.
#' @param footer Optional form footer. Can be a HTML string, Shiny UI object, or
#'   function returning Shiny UI. Function arguments may include `ns`, `prefix`,
#'   `values` and `form`.
#' @param server Optional Shiny server hook with signature
#'   `function(input, output, session)`.
#'
#' @return A form definition object of class `"sft_form"`.
#' @examples
#' # A form is described once; the schema, CRUD and Shiny module derive from it.
#' contacts <- form(
#'   form_id = "contacts",
#'   table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name", mandatory = TRUE),
#'     form_field(id = "email", label = "Email", unique = TRUE),
#'     form_field(
#'       id = "team", label = "Team", input_type = "selectInput",
#'       args = list(choices = c("Sales", "Support"))
#'     )
#'   )
#' )
#' contacts$form_id
#' @export
form <- function(form_id,
                     fields,
                     table_name = form_id,
                     db_path = "form_data.sqlite",
                     db = NULL,
                     form_name = form_id,
                     version = 1L,
                     schema_policy = c("safe", "manual"),
                     on_edit_missing_required = c("warn", "require", "ignore"),
                     tab_labels = NULL,
                     slide_labels = NULL,
                     messages = list(),
                     validation_rules = list(),
                     header = NULL,
                     footer = NULL,
                     server = NULL) {
  schema_policy <- match.arg(schema_policy)
  on_edit_missing_required <- match.arg(on_edit_missing_required)

  db <- db %||% db_sqlite(db_path)

  if (is.character(db) && length(db) == 1L && !is.na(db)) {
    db <- db_sqlite(db)
  }

  form <- list(
    form_id = form_id,
    form_name = form_name,
    table_name = table_name,
    db_path = db_path,
    db = db,
    version = as.integer(version),
    schema_policy = schema_policy,
    on_edit_missing_required = on_edit_missing_required,
    tab_labels = tab_labels,
    slide_labels = slide_labels,
    messages = messages,
    validation_rules = validation_rules,
    header = header,
    footer = footer,
    fields = fields,
    server = server
  )

  sft_validate_form(form)

  structure(form, class = c("sft_form", "list"))
}

sft_validate_form <- function(form) {
  sft_check_identifier(form$form_id, "form_id")
  sft_check_identifier(form$table_name, "table_name")

  if (!sft_is_scalar_character(form$form_name)) {
    stop("form_name must be a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_character(form$db_path)) {
    stop("db_path must be a non-empty character scalar.", call. = FALSE)
  }

  sft_validate_db_config(form$db)

  if (!sft_is_scalar_number(form$version) || form$version < 1L) {
    stop("version must be an integer-like scalar greater than or equal to 1.", call. = FALSE)
  }

  if (!is.list(form$fields) || length(form$fields) == 0L) {
    stop("fields must be a non-empty list of form_field objects.", call. = FALSE)
  }

  valid_field <- vapply(
    form$fields,
    inherits,
    logical(1),
    what = "sft_field"
  )

  if (!all(valid_field)) {
    stop("all entries in fields must be created with form_field().", call. = FALSE)
  }

  sft_check_optional_named_or_unnamed_labels(form$tab_labels, "tab_labels")
  sft_check_optional_named_or_unnamed_labels(form$slide_labels, "slide_labels")
  sft_check_form_region(form$header, "header")
  sft_check_form_region(form$footer, "footer")

  if (!is.list(form$messages)) {
    stop("messages must be a named list.", call. = FALSE)
  }

  sft_validate_validation_rules(form$validation_rules, form = form)

  if (length(form$messages) > 0L && (is.null(names(form$messages)) || any(!nzchar(names(form$messages))))) {
    stop("messages must be a named list.", call. = FALSE)
  }

  if (length(form$messages) > 0L) {
    valid_message <- vapply(
      form$messages,
      function(value) is.character(value) && length(value) == 1L && !is.na(value),
      logical(1)
    )

    if (!all(valid_message)) {
      stop("messages must contain character scalar values only.", call. = FALSE)
    }
  }

  if (!is.null(form$server) && !is.function(form$server)) {
    stop("server must be NULL or a function.", call. = FALSE)
  }

  field_ids <- vapply(form$fields, function(x) x$id, character(1))

  if (anyDuplicated(field_ids)) {
    duplicated_ids <- unique(field_ids[duplicated(field_ids)])
    stop(
      "field ids must be unique. Duplicated: ",
      paste(duplicated_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  renamed_from <- vapply(
    form$fields,
    function(field) field$renamed_from %||% NA_character_,
    character(1)
  )
  renamed_from <- renamed_from[!is.na(renamed_from)]

  if (length(renamed_from) > 0L) {
    invalid_renames <- intersect(renamed_from, field_ids)

    if (length(invalid_renames) > 0L) {
      stop(
        "renamed_from must refer to previous field ids, not fields present in the current form. Invalid: ",
        paste(invalid_renames, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    if (anyDuplicated(renamed_from)) {
      duplicated_renames <- unique(renamed_from[duplicated(renamed_from)])
      stop(
        "renamed_from values must be unique. Duplicated: ",
        paste(duplicated_renames, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  input_fields <- Filter(sft_is_input_field, form$fields)
  db_columns <- vapply(input_fields, function(x) x$db_column, character(1))

  if (length(db_columns) > 0L && anyDuplicated(db_columns)) {
    duplicated_columns <- unique(db_columns[duplicated(db_columns)])
    stop(
      "database columns must be unique. Duplicated: ",
      paste(duplicated_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(form)
}

#' Get field ids from a form
#'
#' @param form Object created with [form()].
#'
#' @return Character vector of field ids.
#' @examples
#' f <- form(
#'   form_id = "contacts",
#'   table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name", mandatory = TRUE),
#'     form_field(id = "email", label = "Email")
#'   )
#' )
#' field_ids(f)
#' @export
field_ids <- function(form) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  vapply(form$fields, function(x) x$id, character(1))
}

#' Get database columns from a form
#'
#' @param form Object created with [form()].
#'
#' @return Character vector of database column names for input fields.
#' @examples
#' f <- form(
#'   form_id = "contacts",
#'   table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name"),
#'     form_field(id = "email", label = "Email", db_column = "email_address")
#'   )
#' )
#' db_columns(f)
#' @export
db_columns <- function(form) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  input_fields <- Filter(sft_is_input_field, form$fields)

  vapply(input_fields, function(x) x$db_column, character(1))
}

#' Print a form schema
#'
#' @param x Object created with [form()].
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @examples
#' f <- form(
#'   form_id = "contacts",
#'   table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name"))
#' )
#' print(f)
#' @export
print.sft_form <- function(x, ...) {
  cat("<form>\n")
  cat("  form_id:       ", x$form_id, "\n", sep = "")
  cat("  form_name:     ", x$form_name, "\n", sep = "")
  cat("  table_name:    ", x$table_name, "\n", sep = "")
  cat("  db_path:       ", x$db_path, "\n", sep = "")
  cat("  version:       ", x$version, "\n", sep = "")
  cat("  schema_policy: ", x$schema_policy, "\n", sep = "")
  cat("  fields:        ", length(x$fields), "\n", sep = "")
  cat("  input fields:  ", length(Filter(sft_is_input_field, x$fields)), "\n", sep = "")
  invisible(x)
}
