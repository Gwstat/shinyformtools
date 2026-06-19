sft_supported_input_binding_types <- function() {
  c("choices", "value", "visibility")
}

sft_check_input_binding_field <- function(field, name = "field") {
  if (!is.character(field) || length(field) != 1L || is.na(field) || !nzchar(field)) {
    stop(name, " must be a non-empty character scalar.", call. = FALSE)
  }

  invisible(field)
}

sft_check_input_binding_depends_on <- function(depends_on) {
  if (is.null(depends_on)) {
    return(character())
  }

  if (!is.character(depends_on) || any(is.na(depends_on)) || any(!nzchar(depends_on))) {
    stop("depends_on must be NULL or a character vector of field ids.", call. = FALSE)
  }

  unique(depends_on)
}

sft_check_input_binding_fun <- function(fun, name) {
  if (!is.function(fun)) {
    stop(name, " must be a function.", call. = FALSE)
  }

  invisible(fun)
}

sft_find_input_field <- function(form, field_id) {
  fields <- sft_active_input_fields(form)

  for (field in fields) {
    if (identical(field$id, field_id)) {
      return(field)
    }
  }

  stop("Unknown input binding field: ", field_id, ".", call. = FALSE)
}

sft_current_input_values <- function(form, input, prefix = "") {
  fields <- sft_active_input_fields(form)
  out <- lapply(
    fields,
    function(field) {
      input[[paste0(prefix, field$id)]]
    }
  )
  names(out) <- vapply(fields, function(field) field$id, character(1))
  out
}

sft_call_input_binding_fun <- function(fun,
                                       input,
                                       context,
                                       field,
                                       prefix,
                                       values,
                                       choices = NULL,
                                       current = NULL) {
  args <- list(
    input = input,
    context = context,
    field = field,
    prefix = prefix,
    values = values,
    choices = choices,
    current = current
  )

  formals_names <- names(formals(fun))

  if ("..." %in% formals_names) {
    return(do.call(fun, args))
  }

  do.call(fun, args[intersect(names(args), formals_names)])
}

sft_normalize_choices <- function(choices) {
  if (is.null(choices)) {
    return(character())
  }

  if (is.data.frame(choices)) {
    if (all(c("value", "label") %in% names(choices))) {
      values <- as.character(choices$value)
      labels <- as.character(choices$label)
      return(stats::setNames(values, labels))
    }

    if (ncol(choices) == 1L) {
      values <- as.character(choices[[1L]])
      return(values)
    }

    stop(
      "Choice data frames must contain value and label columns, or exactly one column.",
      call. = FALSE
    )
  }

  if (is.list(choices) && !is.data.frame(choices)) {
    choices <- unlist(choices, use.names = TRUE)
  }

  if (length(choices) == 0L) {
    return(character())
  }

  choices
}

sft_choice_values <- function(choices) {
  if (is.null(choices) || length(choices) == 0L) {
    return(character())
  }

  values <- unname(choices)
  values <- values[!is.na(values)]
  as.character(values)
}

sft_first_choice_value <- function(choices) {
  values <- sft_choice_values(choices)

  if (length(values) == 0L) {
    return(NULL)
  }

  values[[1L]]
}

sft_resolve_choice_selection <- function(selected,
                                         choices,
                                         input,
                                         context,
                                         field,
                                         prefix,
                                         values,
                                         current) {
  if (is.function(selected)) {
    return(sft_call_input_binding_fun(
      fun = selected,
      input = input,
      context = context,
      field = field,
      prefix = prefix,
      values = values,
      choices = choices,
      current = current
    ))
  }

  if (is.null(selected)) {
    selected <- "preserve"
  }

  if (!is.character(selected) || length(selected) != 1L || is.na(selected)) {
    return(selected)
  }

  choice_values <- sft_choice_values(choices)

  switch(
    selected,
    preserve = {
      if (!is.null(current) && length(current) > 0L) {
        current_values <- as.character(current)
        current_values <- current_values[current_values %in% choice_values]

        if (length(current_values) > 0L) {
          return(current_values)
        }
      }

      sft_first_choice_value(choices)
    },
    first = sft_first_choice_value(choices),
    none = character(),
    selected
  )
}

#' Define dynamic choices for a form input
#'
#' Creates an input binding for [form_server()] that updates the choices of
#' one select-like input whenever the add/edit dialog opens or dependency fields
#' change. This replaces legacy string-based `observeEvent()` snippets for
#' dependent choice lists.
#'
#' @param field Field id whose choices should be updated.
#' @param choices Function returning choices. It may declare any of `input`,
#'   `context`, `field`, `prefix` and `values` as arguments. Return a vector, a
#'   named vector, or a data frame with `value` and `label` columns.
#' @param depends_on Optional field ids that trigger recomputation.
#' @param selected Selection policy. `"preserve"` keeps the current value if it
#'   remains valid and otherwise selects the first available value. `"first"`
#'   always selects the first value, and `"none"` clears the selection. A
#'   function may be supplied for custom logic.
#' @param update_args Additional arguments passed to the relevant Shiny update
#'   function, for example `list(server = TRUE, options = list(create = TRUE))`
#'   for `selectizeInput`.
#'
#' @return An input binding object.
#' @examples
#' b <- dynamic_choices(
#'   field = "city",
#'   depends_on = "country",
#'   choices = function(values) {
#'     if (identical(values$country, "DE")) c("Berlin", "Munich") else c("Paris", "Lyon")
#'   }
#' )
#' b$field
#' b$depends_on
#' @export
dynamic_choices <- function(field,
                                choices,
                                depends_on = NULL,
                                selected = "preserve",
                                update_args = list()) {
  sft_check_input_binding_field(field)
  sft_check_input_binding_fun(choices, "choices")
  depends_on <- sft_check_input_binding_depends_on(depends_on)

  if (!is.list(update_args)) {
    stop("update_args must be a list.", call. = FALSE)
  }

  structure(
    list(
      type = "choices",
      field = field,
      depends_on = depends_on,
      choices = choices,
      selected = selected,
      update_args = update_args
    ),
    class = c("sft_input_binding", "list")
  )
}

#' Define a dynamic value for a form input
#'
#' Creates an input binding for [form_server()] that updates one input value
#' whenever the add/edit dialog opens or dependency fields change. This is useful
#' for derived fields such as postal codes from street and house number inputs.
#'
#' @param field Field id whose value should be updated.
#' @param value Function returning the new value. It may declare any of `input`,
#'   `context`, `field`, `prefix` and `values` as arguments.
#' @param depends_on Optional field ids that trigger recomputation.
#' @param update_args Additional arguments passed to the relevant Shiny update
#'   function.
#'
#' @return An input binding object.
#' @examples
#' b <- dynamic_value(
#'   field = "zip",
#'   depends_on = "city",
#'   value = function(values) if (identical(values$city, "Berlin")) "10115" else ""
#' )
#' b$field
#' b$depends_on
#' @export
dynamic_value <- function(field,
                              value,
                              depends_on = NULL,
                              update_args = list()) {
  sft_check_input_binding_field(field)
  sft_check_input_binding_fun(value, "value")
  depends_on <- sft_check_input_binding_depends_on(depends_on)

  if (!is.list(update_args)) {
    stop("update_args must be a list.", call. = FALSE)
  }

  structure(
    list(
      type = "value",
      field = field,
      depends_on = depends_on,
      value = value,
      update_args = update_args
    ),
    class = c("sft_input_binding", "list")
  )
}

#' Define dynamic visibility for a form input
#'
#' Creates an input binding for [form_server()] that shows or hides one form
#' field depending on the live values of other inputs. The field's container is
#' toggled in the add/edit dialog whenever the dialog opens or a dependency field
#' changes, so a field can "pop up" only when it is relevant.
#'
#' The same predicate is re-evaluated server-side on save: when it returns
#' `FALSE` the field's value is dropped before insert/update, so a hidden field
#' never persists a stale value. This cannot be bypassed from the client.
#'
#' @param field Field id whose visibility should be controlled.
#' @param visible Function returning `TRUE` to show the field and `FALSE` to hide
#'   it. It may declare any of `input`, `context`, `field`, `prefix` and
#'   `values` as arguments; `values` (the current field values, keyed by field
#'   id) is the one available both in the dialog and on save, so predicates
#'   should key off it.
#' @param depends_on Optional field ids that trigger re-evaluation. Defaults to
#'   the fields the predicate reacts to; list them so the dialog updates live.
#'
#' @return An input binding object.
#' @examples
#' b <- dynamic_visibility(
#'   field = "reason",
#'   depends_on = "status",
#'   visible = function(values) identical(values$status, "Rejected")
#' )
#' b$field
#' b$depends_on
#' @export
dynamic_visibility <- function(field,
                                   visible,
                                   depends_on = NULL) {
  sft_check_input_binding_field(field)
  sft_check_input_binding_fun(visible, "visible")
  depends_on <- sft_check_input_binding_depends_on(depends_on)

  structure(
    list(
      type = "visibility",
      field = field,
      depends_on = depends_on,
      visible = visible
    ),
    class = c("sft_input_binding", "list")
  )
}

sft_validate_input_bindings <- function(input_bindings, form) {
  if (is.null(input_bindings)) {
    return(list())
  }

  if (!is.list(input_bindings)) {
    stop("input_bindings must be NULL or a list of input binding objects.", call. = FALSE)
  }

  if (length(input_bindings) == 0L) {
    return(list())
  }

  field_ids <- vapply(
    sft_active_input_fields(form),
    function(field) field$id,
    character(1)
  )

  lapply(
    input_bindings,
    function(binding) {
      if (!inherits(binding, "sft_input_binding")) {
        stop("All input_bindings entries must be created with dynamic_choices(), dynamic_value() or dynamic_visibility().", call. = FALSE)
      }

      if (!binding$type %in% sft_supported_input_binding_types()) {
        stop("Unsupported input binding type: ", binding$type, ".", call. = FALSE)
      }

      if (!binding$field %in% field_ids) {
        stop("Unknown input binding field: ", binding$field, ".", call. = FALSE)
      }

      invalid_dependencies <- setdiff(binding$depends_on, field_ids)

      if (length(invalid_dependencies) > 0L) {
        stop(
          "Unknown input binding dependency for field ", binding$field, ": ",
          paste(invalid_dependencies, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      binding
    }
  )
}

sft_update_choices_input <- function(session,
                                     input_type,
                                     input_id,
                                     choices,
                                     selected = NULL,
                                     update_args = list()) {
  args <- c(
    list(
      session = session,
      inputId = input_id,
      choices = choices,
      selected = selected
    ),
    update_args
  )

  switch(
    input_type,
    selectInput = do.call(shiny::updateSelectInput, args),
    selectizeInput = do.call(shiny::updateSelectizeInput, args),
    radioButtons = do.call(shiny::updateRadioButtons, args),
    checkboxGroupInput = do.call(shiny::updateCheckboxGroupInput, args),
    multiInput = do.call(shinyWidgets::updateMultiInput, args),
    stop(
      "Dynamic choices are only supported for selectInput, selectizeInput, radioButtons, checkboxGroupInput and multiInput fields. Field '",
      input_id,
      "' uses ",
      input_type,
      ".",
      call. = FALSE
    )
  )
}

sft_update_value_input <- function(session,
                                   input_type,
                                   input_id,
                                   value,
                                   update_args = list()) {
  args <- c(
    list(
      session = session,
      inputId = input_id
    ),
    sft_input_value_args(
      input_type = input_type,
      value = value
    ),
    update_args
  )

  switch(
    input_type,
    textInput = do.call(shiny::updateTextInput, args),
    passwordInput = do.call(shiny::updateTextInput, args),
    textAreaInput = do.call(shiny::updateTextAreaInput, args),
    numericInput = do.call(shiny::updateNumericInput, args),
    sliderInput = do.call(shiny::updateSliderInput, args),
    dateInput = do.call(shiny::updateDateInput, args),
    dateRangeInput = do.call(shiny::updateDateRangeInput, args),
    checkboxInput = do.call(shiny::updateCheckboxInput, args),
    timeInput = do.call(shinyTime::updateTimeInput, args),
    ibanInput = do.call(updateIBANInput, args),
    stop(
      "Dynamic values are not supported for input type ",
      input_type,
      " yet.",
      call. = FALSE
    )
  )
}

sft_binding_event_key <- function(input, prefix, depends_on, open_input_id) {
  values <- lapply(
    paste0(prefix, depends_on),
    function(input_id) input[[input_id]]
  )
  names(values) <- depends_on

  c(
    list(open = input[[open_input_id]]),
    values
  )
}

sft_run_choice_binding <- function(binding,
                                   field,
                                   input,
                                   session,
                                   prefix,
                                   context) {
  values <- sft_current_input_values(
    form = context$form,
    input = input,
    prefix = prefix
  )

  current <- input[[paste0(prefix, field$id)]]

  choices <- sft_call_input_binding_fun(
    fun = binding$choices,
    input = input,
    context = context,
    field = field,
    prefix = prefix,
    values = values,
    current = current
  )
  choices <- sft_normalize_choices(choices)

  selected <- sft_resolve_choice_selection(
    selected = binding$selected,
    choices = choices,
    input = input,
    context = context,
    field = field,
    prefix = prefix,
    values = values,
    current = current
  )

  sft_update_choices_input(
    session = session,
    input_type = field$input_type,
    input_id = paste0(prefix, field$id),
    choices = choices,
    selected = selected,
    update_args = binding$update_args
  )
}

sft_run_value_binding <- function(binding,
                                  field,
                                  input,
                                  session,
                                  prefix,
                                  context) {
  values <- sft_current_input_values(
    form = context$form,
    input = input,
    prefix = prefix
  )

  value <- sft_call_input_binding_fun(
    fun = binding$value,
    input = input,
    context = context,
    field = field,
    prefix = prefix,
    values = values,
    current = input[[paste0(prefix, field$id)]]
  )

  sft_update_value_input(
    session = session,
    input_type = field$input_type,
    input_id = paste0(prefix, field$id),
    value = value,
    update_args = binding$update_args
  )
}

sft_binding_is_visible <- function(binding,
                                    field,
                                    input,
                                    prefix,
                                    context,
                                    values = NULL) {
  if (is.null(values)) {
    values <- sft_current_input_values(
      form = context$form,
      input = input,
      prefix = prefix
    )
  }

  isTRUE(sft_call_input_binding_fun(
    fun = binding$visible,
    input = input,
    context = context,
    field = field,
    prefix = prefix,
    values = values,
    current = values[[field$id]]
  ))
}

sft_run_visibility_binding <- function(binding,
                                       field,
                                       input,
                                       session,
                                       prefix,
                                       context) {
  visible <- sft_binding_is_visible(
    binding = binding,
    field = field,
    input = input,
    prefix = prefix,
    context = context
  )

  shinyjs::toggle(
    id = paste0("sft_field_container_", prefix, field$id),
    condition = visible
  )
}

# Re-evaluate visibility predicates server-side and drop the values of fields
# that are currently hidden, so a "pop-up" field never persists a stale value.
sft_drop_hidden_field_values <- function(input_bindings, form, values) {
  if (is.null(input_bindings) || length(input_bindings) == 0L) {
    return(values)
  }

  for (binding in input_bindings) {
    if (!inherits(binding, "sft_input_binding") ||
        !identical(binding$type, "visibility")) {
      next
    }

    field <- sft_find_input_field(form, binding$field)

    visible <- sft_binding_is_visible(
      binding = binding,
      field = field,
      input = NULL,
      prefix = "",
      context = list(form = form),
      values = values
    )

    if (!visible) {
      values[[binding$field]] <- NULL
    }
  }

  values
}

sft_register_one_input_binding <- function(binding,
                                           form,
                                           input,
                                           session,
                                           context) {
  field <- sft_find_input_field(form, binding$field)

  for (prefix in c("add_", "edit_")) {
    open_input_id <- if (identical(prefix, "add_")) "open_add" else "open_edit"

    local({
      local_binding <- binding
      local_field <- field
      local_prefix <- prefix
      local_open_input_id <- open_input_id

      shiny::observeEvent(
        sft_binding_event_key(
          input = input,
          prefix = local_prefix,
          depends_on = local_binding$depends_on,
          open_input_id = local_open_input_id
        ),
        {
          tryCatch(
            {
              run_binding <- switch(
                local_binding$type,
                choices = sft_run_choice_binding,
                value = sft_run_value_binding,
                visibility = sft_run_visibility_binding,
                stop("Unsupported input binding type: ", local_binding$type, ".", call. = FALSE)
              )

              run_binding(
                binding = local_binding,
                field = local_field,
                input = input,
                session = session,
                prefix = local_prefix,
                context = context()
              )
            },
            error = function(err) {
              shiny::showNotification(
                conditionMessage(err),
                type = "error",
                duration = 8
              )
            }
          )
        },
        ignoreInit = TRUE
      )
    })
  }

  invisible(TRUE)
}

sft_register_input_bindings <- function(input_bindings,
                                        form,
                                        input,
                                        session,
                                        context) {
  input_bindings <- sft_validate_input_bindings(input_bindings, form)

  for (binding in input_bindings) {
    sft_register_one_input_binding(
      binding = binding,
      form = form,
      input = input,
      session = session,
      context = context
    )
  }

  invisible(input_bindings)
}
