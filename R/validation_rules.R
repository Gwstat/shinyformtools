# Cross-field validation rules.

sft_check_validation_rule_id <- function(id) {
  if (!sft_is_scalar_character(id)) {
    stop("id must be a non-empty character scalar.", call. = FALSE)
  }

  invisible(id)
}

sft_check_validation_fields <- function(fields) {
  if (is.null(fields)) {
    return(invisible(TRUE))
  }

  if (!is.character(fields) || any(is.na(fields)) || any(!nzchar(fields))) {
    stop("fields must be NULL or a character vector of field ids.", call. = FALSE)
  }

  invisible(TRUE)
}

sft_check_validation_fun <- function(fun, name) {
  if (is.null(fun)) {
    return(invisible(TRUE))
  }

  if (!is.function(fun)) {
    stop(name, " must be NULL or a function.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Define a cross-field validation rule
#'
#' Validation rules are evaluated on the server during insert and update, so they
#' cannot be bypassed by changing client-side state. A rule can either provide a
#' custom `validate` function, or combine `condition` with `fields` to implement
#' conditional required fields.
#'
#' @param id Stable rule id.
#' @param validate Optional function returning `TRUE` for a valid record,
#'   `FALSE` for an invalid record, or a character vector with validation
#'   messages. Functions may declare any of `values`, `record`, `form`, `conn`,
#'   `current_id` and `context`.
#' @param condition Optional function used with `fields`. If it returns `TRUE`,
#'   the listed fields must be non-empty. Functions may declare any of `values`,
#'   `record`, `form`, `conn`, `current_id` and `context`.
#' @param fields Optional field ids checked as conditionally required when
#'   `condition` is supplied.
#' @param severity Either `"error"` or `"warning"`. Errors block insert/update;
#'   warnings are emitted but do not block persistence.
#' @param message Validation message. May be a character scalar or a function.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' rule <- validation_rule(
#'   id = "end_after_start",
#'   validate = function(values) {
#'     is.null(values$end) || is.null(values$start) || values$end >= values$start
#'   },
#'   message = "End date must not be before the start date."
#' )
#' rule$id
#' rule$severity
#' @export
validation_rule <- function(id,
                                validate = NULL,
                                condition = NULL,
                                fields = NULL,
                                severity = c("error", "warning"),
                                message = NULL) {
  severity <- match.arg(severity)
  sft_check_validation_rule_id(id)
  sft_check_validation_fun(validate, "validate")
  sft_check_validation_fun(condition, "condition")
  sft_check_validation_fields(fields)

  if (is.null(validate) && is.null(condition)) {
    stop("validate or condition must be supplied.", call. = FALSE)
  }

  if (is.null(validate) && is.null(fields)) {
    stop("fields must be supplied when validate is NULL.", call. = FALSE)
  }

  if (!is.null(message) && !is.character(message) && !is.function(message)) {
    stop("message must be NULL, a character scalar or a function.", call. = FALSE)
  }

  if (is.character(message) && (length(message) != 1L || is.na(message))) {
    stop("message must be a non-missing character scalar.", call. = FALSE)
  }

  structure(
    list(
      id = id,
      validate = validate,
      condition = condition,
      fields = fields,
      severity = severity,
      message = message
    ),
    class = c("sft_validation_rule", "list")
  )
}

#' Require fields conditionally
#'
#' Convenience wrapper for [validation_rule()] for the common pattern
#' "if a condition is true, these fields must be filled".
#'
#' @param id Stable rule id.
#' @param condition Function returning `TRUE` when the listed fields are required.
#' @param fields Field ids that become required.
#' @param severity Either `"error"` or `"warning"`.
#' @param message Optional custom message.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' rule <- required_if(
#'   id = "reason_req",
#'   condition = function(values) identical(values$status, "Rejected"),
#'   fields = "reason",
#'   message = "Reason required when Rejected."
#' )
#' rule$id
#' rule$fields
#' @export
required_if <- function(id,
                            condition,
                            fields,
                            severity = c("error", "warning"),
                            message = NULL) {
  validation_rule(
    id = id,
    condition = condition,
    fields = fields,
    severity = severity,
    message = message
  )
}

sft_validate_validation_rules <- function(rules, form = NULL) {
  if (is.null(rules)) {
    return(invisible(TRUE))
  }

  if (!is.list(rules)) {
    stop("validation_rules must be NULL or a list.", call. = FALSE)
  }

  if (length(rules) == 0L) {
    return(invisible(TRUE))
  }

  valid_rule <- vapply(
    rules,
    inherits,
    logical(1),
    what = "sft_validation_rule"
  )

  if (!all(valid_rule)) {
    stop("all validation_rules entries must be created with validation_rule().", call. = FALSE)
  }

  ids <- vapply(rules, function(rule) rule$id, character(1))

  if (anyDuplicated(ids)) {
    duplicated_ids <- unique(ids[duplicated(ids)])
    stop(
      "validation rule ids must be unique. Duplicated: ",
      paste(duplicated_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(form)) {
    field_ids <- vapply(form$fields, function(field) field$id, character(1))

    for (rule in rules) {
      invalid_fields <- setdiff(rule$fields %||% character(), field_ids)

      if (length(invalid_fields) > 0L) {
        stop(
          "validation rule '", rule$id, "' refers to unknown fields: ",
          paste(invalid_fields, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}

sft_record_values_by_field_id <- function(form, record) {
  if (is.data.frame(record)) {
    record <- sft_row_to_list(record)
  }

  fields <- sft_active_input_fields(form)
  out <- list()

  for (field in fields) {
    out[[field$id]] <- sft_record_get_value(record, field)
  }

  out
}

sft_call_validation_fun <- function(fun,
                                    values,
                                    record,
                                    form,
                                    conn = NULL,
                                    current_id = NULL,
                                    context = NULL) {
  args <- list(
    values = values,
    record = record,
    form = form,
    conn = conn,
    current_id = current_id,
    context = context
  )

  formals_names <- names(formals(fun))

  if ("..." %in% formals_names) {
    return(do.call(fun, args))
  }

  do.call(fun, args[intersect(names(args), formals_names)])
}

sft_validation_rule_message <- function(rule,
                                        form,
                                        values,
                                        record,
                                        fields = NULL,
                                        messages = NULL) {
  if (!is.null(messages) && length(messages) > 0L) {
    return(messages)
  }

  field_text <- paste(fields %||% rule$fields %||% character(), collapse = ", ")

  fallback <- if (length(rule$fields %||% character()) > 0L && is.null(rule$validate)) {
    sft_message(
      form = form,
      key = "conditional_required",
      values = list(rule = rule$id, fields = field_text)
    )
  } else {
    sft_message(
      form = form,
      key = "validation_rule_failed",
      values = list(rule = rule$id, fields = field_text)
    )
  }

  message <- rule$message %||% fallback

  sft_interpolate_text(
    text = message,
    values = list(
      rule = rule$id,
      fields = field_text,
      severity = rule$severity
    )
  )
}

sft_apply_validation_rule <- function(rule,
                                      form,
                                      values,
                                      record,
                                      conn = NULL,
                                      current_id = NULL,
                                      context = NULL) {
  if (!inherits(rule, "sft_validation_rule")) {
    stop("rule must be an validation_rule object.", call. = FALSE)
  }

  active <- TRUE

  if (!is.null(rule$condition)) {
    active <- isTRUE(sft_call_validation_fun(
      fun = rule$condition,
      values = values,
      record = record,
      form = form,
      conn = conn,
      current_id = current_id,
      context = context
    ))
  }

  if (!isTRUE(active)) {
    return(list(errors = character(), warnings = character()))
  }

  messages <- character()

  if (!is.null(rule$validate)) {
    result <- sft_call_validation_fun(
      fun = rule$validate,
      values = values,
      record = record,
      form = form,
      conn = conn,
      current_id = current_id,
      context = context
    )

    if (isTRUE(result) || is.null(result)) {
      messages <- character()
    } else if (isFALSE(result)) {
      messages <- sft_validation_rule_message(
        rule = rule,
        form = form,
        values = values,
        record = record
      )
    } else if (is.character(result)) {
      messages <- result[nzchar(result) & !is.na(result)]
    } else {
      stop(
        "validation rule '", rule$id, "' must return TRUE, FALSE, NULL or a character vector.",
        call. = FALSE
      )
    }
  } else {
    missing <- rule$fields[vapply(
      rule$fields,
      function(field) sft_is_empty_value(values[[field]] %||% record[[field]]),
      logical(1)
    )]

    if (length(missing) > 0L) {
      messages <- sft_validation_rule_message(
        rule = rule,
        form = form,
        values = values,
        record = record,
        fields = missing
      )
    }
  }

  if (length(messages) == 0L) {
    return(list(errors = character(), warnings = character()))
  }

  if (identical(rule$severity, "warning")) {
    return(list(errors = character(), warnings = messages))
  }

  list(errors = messages, warnings = character())
}

sft_validate_rules <- function(form,
                               record,
                               conn = NULL,
                               current_id = NULL,
                               context = NULL) {
  rules <- form$validation_rules %||% list()

  if (length(rules) == 0L) {
    return(list(errors = character(), warnings = character()))
  }

  if (is.data.frame(record)) {
    record <- sft_row_to_list(record)
  }

  values <- sft_record_values_by_field_id(form, record)

  errors <- character()
  warnings <- character()

  for (rule in rules) {
    result <- sft_apply_validation_rule(
      rule = rule,
      form = form,
      values = values,
      record = record,
      conn = conn,
      current_id = current_id,
      context = context
    )

    errors <- c(errors, result$errors)
    warnings <- c(warnings, result$warnings)
  }

  list(errors = errors, warnings = warnings)
}

#' Forbid a condition
#'
#' Convenience wrapper for [validation_rule()] for the common pattern
#' "this condition must not be true". The rule is evaluated server-side during
#' insert and update.
#'
#' @param id Stable rule id.
#' @param condition Function returning `TRUE` when the record is invalid.
#' @param fields Optional field ids associated with the rule.
#' @param severity Either `"error"` or `"warning"`.
#' @param message Optional custom message.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' rule <- forbid_if(
#'   id = "no_negative_price",
#'   condition = function(values) !is.null(values$price) && values$price < 0,
#'   fields = "price",
#'   message = "Price must not be negative."
#' )
#' rule$id
#' rule$severity
#' @export
forbid_if <- function(id,
                          condition,
                          fields = NULL,
                          severity = c("error", "warning"),
                          message = NULL) {
  sft_check_validation_fun(condition, "condition")

  validate <- function(values, record, form, conn = NULL, current_id = NULL, context = NULL) {
    !isTRUE(sft_call_validation_fun(
      fun = condition,
      values = values,
      record = record,
      form = form,
      conn = conn,
      current_id = current_id,
      context = context
    ))
  }

  validation_rule(
    id = id,
    validate = validate,
    fields = fields,
    severity = severity,
    message = message
  )
}

sft_compare_values <- function(left, operator, right) {
  if (sft_is_empty_value(left) || sft_is_empty_value(right)) {
    return(TRUE)
  }

  left <- left[[1L]]
  right <- right[[1L]]

  switch(
    operator,
    `==` = identical(as.character(left), as.character(right)),
    `!=` = !identical(as.character(left), as.character(right)),
    `<` = suppressWarnings(left < right),
    `<=` = suppressWarnings(left <= right),
    `>` = suppressWarnings(left > right),
    `>=` = suppressWarnings(left >= right),
    stop("Unsupported comparison operator.", call. = FALSE)
  ) |>
    isTRUE()
}

#' Compare two fields or one field with a fixed value
#'
#' Builds a server-side validation rule for simple field comparisons. Empty
#' values are ignored so that optional fields can remain blank; combine with
#' `mandatory = TRUE` or [required_if()] when emptiness should be invalid.
#'
#' @param id Stable rule id.
#' @param left Left field id.
#' @param operator One of `"=="`, `"!="`, `"<"`, `"<="`, `">"`, `">="`.
#' @param right Optional right field id.
#' @param value Optional fixed right-hand value. Exactly one of `right` or
#'   `value` should be supplied.
#' @param severity Either `"error"` or `"warning"`.
#' @param message Optional custom message.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' rule <- compare_fields(
#'   id = "end_after_start",
#'   left = "start_date",
#'   operator = "<=",
#'   right = "end_date",
#'   message = "Start date must be on or before the end date."
#' )
#' rule$id
#' rule$fields
#' @export
compare_fields <- function(id,
                               left,
                               operator = c("==", "!=", "<", "<=", ">", ">="),
                               right = NULL,
                               value = NULL,
                               severity = c("error", "warning"),
                               message = NULL) {
  operator <- match.arg(operator)
  sft_check_validation_fields(left)
  sft_check_validation_fields(right)

  if (length(left) != 1L) {
    stop("left must be a single field id.", call. = FALSE)
  }

  if (is.null(right) == is.null(value)) {
    stop("Exactly one of right or value must be supplied.", call. = FALSE)
  }

  if (!is.null(right) && length(right) != 1L) {
    stop("right must be a single field id.", call. = FALSE)
  }

  fields <- c(left, right %||% character())

  validate <- function(values, record, form, conn = NULL, current_id = NULL, context = NULL) {
    left_value <- values[[left]] %||% record[[left]]
    right_value <- if (!is.null(right)) {
      values[[right]] %||% record[[right]]
    } else {
      value
    }

    sft_compare_values(left_value, operator, right_value)
  }

  validation_rule(
    id = id,
    validate = validate,
    fields = fields,
    severity = severity,
    message = message
  )
}

sft_field_for_id <- function(form, field_id) {
  for (field in sft_active_input_fields(form)) {
    if (identical(field$id, field_id)) {
      return(field)
    }
  }

  stop("Unknown field id: ", field_id, ".", call. = FALSE)
}

#' Require a unique combination of fields
#'
#' Builds a server-side validation rule that rejects records where the given
#' field combination already exists in a non-deleted record. This is useful for
#' composite uniqueness constraints that cannot be expressed with a single
#' field's `unique = TRUE` setting.
#'
#' @param id Stable rule id.
#' @param fields Field ids forming the uniqueness key.
#' @param severity Either `"error"` or `"warning"`.
#' @param message Optional custom message.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' # Building the rule needs no database; the uniqueness query only runs
#' # server-side when form_server() validates an insert or update.
#' rule <- must_be_unique(
#'   id = "unique_name_per_team",
#'   fields = c("team", "name"),
#'   message = "This name already exists within the team."
#' )
#' rule$id
#' rule$fields
#' @export
must_be_unique <- function(id,
                               fields,
                               severity = c("error", "warning"),
                               message = NULL) {
  sft_check_validation_fields(fields)

  if (length(fields) == 0L) {
    stop("fields must contain at least one field id.", call. = FALSE)
  }

  validate <- function(values, record, form, conn = NULL, current_id = NULL, context = NULL) {
    if (is.null(conn)) {
      return("A database connection is required for uniqueness validation.")
    }

    form_fields <- lapply(fields, function(field_id) sft_field_for_id(form, field_id))
    names(form_fields) <- fields

    field_values <- lapply(fields, function(field_id) values[[field_id]] %||% record[[field_id]])
    names(field_values) <- fields

    if (any(vapply(field_values, sft_is_empty_value, logical(1)))) {
      return(TRUE)
    }

    where_sql <- paste(
      vapply(
        form_fields,
        function(field) paste0(sft_quote_identifier(conn, field$db_column), " = ?"),
        character(1)
      ),
      collapse = " AND "
    )

    sql <- paste0(
      "SELECT COUNT(*) AS n FROM ",
      sft_quote_identifier(conn, form$table_name),
      " WHERE ",
      where_sql,
      " AND sft_is_deleted = 0"
    )

    params <- Map(
      function(field, value) sft_field_db_value(field, value),
      form_fields,
      field_values
    )
    params <- unname(params)

    if (!is.null(current_id)) {
      sql <- paste0(sql, " AND sft_id <> ?")
      params <- c(params, list(current_id))
    }

    result <- tryCatch(
      DBI::dbGetQuery(conn, sql, params = unname(params)),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      return(paste0(
        "Uniqueness could not be checked: ", conditionMessage(result)
      ))
    }

    if (!is.data.frame(result) || nrow(result) == 0L || !"n" %in% names(result)) {
      return("Uniqueness could not be checked: unexpected database result.")
    }

    result$n[1] == 0L
  }

  validation_rule(
    id = id,
    validate = validate,
    fields = fields,
    severity = severity,
    message = message
  )
}

#' Emit a warning when a condition is true
#'
#' Convenience wrapper around [forbid_if()] with `severity = "warning"`.
#' Warnings are emitted server-side but do not block insert or update.
#'
#' @param id Stable rule id.
#' @param condition Function returning `TRUE` when a warning should be emitted.
#' @param fields Optional field ids associated with the warning.
#' @param message Optional warning message.
#'
#' @return An object of class `"sft_validation_rule"`.
#' @examples
#' rule <- warning_if(
#'   id = "high_amount",
#'   condition = function(values) !is.null(values$amount) && values$amount > 10000,
#'   fields = "amount",
#'   message = "Amount is unusually high; please double-check."
#' )
#' rule$id
#' rule$severity
#' @export
warning_if <- function(id,
                           condition,
                           fields = NULL,
                           message = NULL) {
  forbid_if(
    id = id,
    condition = condition,
    fields = fields,
    severity = "warning",
    message = message
  )
}
