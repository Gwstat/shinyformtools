sft_active_fields <- function(form) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  Filter(
    function(field) identical(field$status, "active"),
    form$fields
  )
}

sft_active_input_fields <- function(form) {
  Filter(
    sft_is_input_field,
    sft_active_fields(form)
  )
}

sft_visible_active_fields <- function(form) {
  Filter(
    function(field) isTRUE(field$show),
    sft_active_fields(form)
  )
}

sft_visible_input_fields <- function(form) {
  Filter(
    sft_is_input_field,
    sft_visible_active_fields(form)
  )
}

sft_editable_input_fields <- function(form, visible_only = FALSE) {
  fields <- if (isTRUE(visible_only)) {
    sft_visible_input_fields(form)
  } else {
    sft_active_input_fields(form)
  }

  Filter(
    function(field) isTRUE(field$editable),
    fields
  )
}

# Resolve a field's `editable` (logical or function of the user) to a logical
# for a given user. A function that errors is treated as not editable
# (fail-closed), so a broken rule never silently grants write access.
sft_field_editable_for <- function(field, user) {
  if (is.function(field$editable)) {
    return(isTRUE(tryCatch(field$editable(user), error = function(e) FALSE)))
  }

  isTRUE(field$editable)
}

# Return a copy of the form whose function-valued `editable` flags have been
# resolved to plain logicals for the given user. Everything downstream
# (the field renderers, editable_only save filtering) then works unchanged.
sft_resolve_editable <- function(form, user) {
  form$fields <- lapply(form$fields, function(field) {
    if (is.function(field$editable)) {
      field$editable <- sft_field_editable_for(field, user)
    }

    field
  })

  form
}

# Stored value for the `editable` schema-metadata column. A function cannot be
# persisted, so a function-valued editable stores 1 (the live gate is the
# per-user resolution at render and save time, not this metadata column).
sft_field_editable_storage <- function(field) {
  if (is.function(field$editable)) {
    return(1L)
  }

  as.integer(isTRUE(field$editable))
}

# Field ids locked for this user specifically by a function-valued `editable`
# (not by a static editable = FALSE). Used to drop those fields on add, where
# static non-editable fields keep their existing behaviour.
sft_user_locked_input_fields <- function(form, user) {
  locked <- Filter(
    function(field) is.function(field$editable) && !sft_field_editable_for(field, user),
    sft_active_input_fields(form)
  )

  vapply(locked, function(field) field$id, character(1))
}

sft_record_has_value <- function(record, field) {
  field$id %in% names(record) || field$db_column %in% names(record)
}

sft_record_get_value <- function(record, field) {
  if (field$id %in% names(record)) {
    return(record[[field$id]])
  }

  if (field$db_column %in% names(record)) {
    return(record[[field$db_column]])
  }

  NULL
}

sft_is_empty_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(TRUE)
  }

  if (length(x) > 1L) {
    return(all(is.na(x)) || length(stats::na.omit(x)) == 0L)
  }

  if (is.na(x)) {
    return(TRUE)
  }

  if (is.character(x) && !nzchar(trimws(x))) {
    return(TRUE)
  }

  FALSE
}

sft_missing_mandatory_fields <- function(form, record) {
  fields <- sft_active_input_fields(form)

  missing <- vapply(
    fields,
    function(field) {
      isTRUE(field$mandatory) &&
        sft_is_empty_value(sft_record_get_value(record, field))
    },
    logical(1)
  )

  vapply(fields[missing], function(field) field$id, character(1))
}

sft_validate_unique_fields <- function(form,
                                       record,
                                       conn,
                                       current_id = NULL) {
  fields <- sft_active_input_fields(form)
  errors <- character()

  unique_fields <- Filter(
    function(field) isTRUE(field$unique),
    fields
  )

  for (field in unique_fields) {
    value <- sft_record_get_value(record, field)

    if (sft_is_empty_value(value)) {
      next
    }

    sql <- paste0(
      "SELECT COUNT(*) AS n FROM ",
      sft_quote_identifier(conn, form$table_name),
      " WHERE ",
      sft_quote_identifier(conn, field$db_column),
      " = ? AND sft_is_deleted = 0"
    )

    params <- list(sft_clean_db_value(value))

    if (!is.null(current_id)) {
      sql <- paste0(sql, " AND sft_id <> ?")
      params <- c(params, list(current_id))
    }

    n <- DBI::dbGetQuery(conn, sql, params = params)$n[1]

    if (n > 0L) {
      errors <- c(
        errors,
        sft_message(
          form = form,
          key = "unique",
          values = list(
            field = field$id,
            label = field$label,
            value = value
          )
        )
      )
    }
  }

  errors
}

#' Validate a record against a form schema
#'
#' @param form Object created with [form()].
#' @param record Named list or one-row data frame.
#' @param conn Optional DBI connection. Required for unique checks.
#' @param current_id Optional current `sft_id` for update checks.
#' @param require_all_mandatory Logical. Whether missing mandatory fields are
#'   treated as validation errors.
#'
#' @return Invisibly returns `TRUE`.
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
#' # A complete record passes (mandatory-field check needs no database).
#' validate_record(f, list(name = "Ada", email = "ada@example.com"))
#'
#' # A missing mandatory field raises an error.
#' try(validate_record(f, list(email = "ada@example.com")))
#' @export
validate_record <- function(form,
                                record,
                                conn = NULL,
                                current_id = NULL,
                                require_all_mandatory = TRUE) {
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

  errors <- character()

  if (isTRUE(require_all_mandatory)) {
    missing <- sft_missing_mandatory_fields(form, record)

    if (length(missing) > 0L) {
      errors <- c(
        errors,
        sft_message(
          form = form,
          key = "mandatory_missing",
          values = list(fields = paste(missing, collapse = ", "))
        )
      )
    }
  }

  if (!is.null(conn)) {
    errors <- c(
      errors,
      sft_validate_unique_fields(
        form = form,
        record = record,
        conn = conn,
        current_id = current_id
      )
    )
  }

  rule_result <- sft_validate_rules(
    form = form,
    record = record,
    conn = conn,
    current_id = current_id
  )

  if (length(rule_result$warnings) > 0L) {
    warning(paste(rule_result$warnings, collapse = "\n"), call. = FALSE)
  }

  errors <- c(errors, rule_result$errors)

  if (length(errors) > 0L) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}


sft_supplied_empty_mandatory_fields <- function(form, record) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (is.data.frame(record)) {
    record <- sft_row_to_list(record)
  }

  if (!is.list(record)) {
    stop("record must be a named list or a one-row data frame.", call. = FALSE)
  }

  fields <- sft_active_input_fields(form)

  empty <- vapply(
    fields,
    function(field) {
      isTRUE(field$mandatory) &&
        sft_record_has_value(record, field) &&
        sft_is_empty_value(sft_record_get_value(record, field))
    },
    logical(1)
  )

  vapply(fields[empty], function(field) field$id, character(1))
}