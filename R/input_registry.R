# Runtime registry of user-defined input types. Populated by register_input()
# and consulted, after the built-ins, by every input_type-keyed helper in this
# file (render, value placement, DB encode/decode, table display) and by the
# dynamic-update helpers in input_bindings.R. The environment is created once
# when the namespace loads and is shared across the R session.
.sft_input_registry <- new.env(parent = emptyenv())

#' Register a custom form input
#'
#' Teaches shinyformtools about an additional Shiny input function (for example
#' one from \pkg{shinyWidgets}) so it can be used as an `input_type` in
#' [form_field()] exactly like a built-in. The widget is rendered in the add and
#' edit forms, its value is read back, stored, restored and shown in the records
#' table, and -- when an `update_fun` is supplied -- it participates in dynamic
#' choices/values.
#'
#' Call `register_input()` before the [form_field()] that uses the registered
#' `input_type` (a field is validated against the registry at definition time).
#' Re-registering the same `name` overwrites the previous entry; built-in input
#' names cannot be overridden.
#'
#' By default the input is treated as a single-valued text input: its value is
#' stored verbatim and read back unchanged. For multi-valued inputs set
#' `multiple = TRUE` to store the value as a JSON array and decode it back to a
#' vector. For full control over how the value is serialized, supply `encode`
#' (value -> stored scalar), `decode` (stored value -> input value) and `format`
#' (stored value -> table display string).
#'
#' @param name Input type name, used as the `input_type` string in
#'   [form_field()]. Must not collide with a built-in input type.
#' @param fun The input UI function, for example `shinyWidgets::knobInput`. It is
#'   called with `inputId`, `label` and the field's `args`.
#' @param value_arg Name of the argument that carries the current value when the
#'   input is rendered or updated, typically `"value"` or `"selected"`. Use
#'   `NULL` if the input takes no value argument (the value is then left to
#'   `args`).
#' @param multiple Logical. When `TRUE`, the value is a vector: it is stored as a
#'   JSON array and decoded back to a vector. Ignored when `encode`/`decode` are
#'   supplied.
#' @param update_fun Optional update function (for example
#'   `shinyWidgets::updateKnobInput`) used for dynamic values and, if it accepts
#'   a `choices` argument, dynamic choices. It is called with `session`,
#'   `inputId` and the value/choices arguments.
#' @param encode Optional function mapping an input value to the scalar stored in
#'   the database. Overrides the default text/JSON handling.
#' @param decode Optional function mapping a stored value back to the value
#'   handed to the input when a record is edited. Overrides the default.
#' @param format Optional function mapping a stored value to the string shown in
#'   the records and versions tables. Overrides the default.
#'
#' @return Invisibly, `name`.
#' @examples
#' \dontrun{
#' # A shinyWidgets knob as a numeric input.
#' register_input(
#'   "knobInput",
#'   fun = shinyWidgets::knobInput,
#'   value_arg = "value",
#'   update_fun = shinyWidgets::updateKnobInput
#' )
#'
#' vol <- form_field(
#'   id = "vol", label = "Volume", input_type = "knobInput",
#'   args = list(min = 0, max = 100), db_type = "REAL"
#' )
#' }
#' @export
register_input <- function(name,
                           fun,
                           value_arg = "value",
                           multiple = FALSE,
                           update_fun = NULL,
                           encode = NULL,
                           decode = NULL,
                           format = NULL) {
  if (!sft_is_scalar_character(name)) {
    stop("name must be a non-empty character scalar.", call. = FALSE)
  }

  if (name %in% sft_supported_input_types()) {
    stop(
      "Cannot register '",
      name,
      "': it is a built-in input type.",
      call. = FALSE
    )
  }

  if (!is.function(fun)) {
    stop("fun must be a function.", call. = FALSE)
  }

  if (!is.null(value_arg) && !sft_is_scalar_character(value_arg)) {
    stop("value_arg must be NULL or a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_logical(multiple)) {
    stop("multiple must be TRUE or FALSE.", call. = FALSE)
  }

  for (hook in list(
    list(name = "update_fun", value = update_fun),
    list(name = "encode", value = encode),
    list(name = "decode", value = decode),
    list(name = "format", value = format)
  )) {
    if (!is.null(hook$value) && !is.function(hook$value)) {
      stop(hook$name, " must be NULL or a function.", call. = FALSE)
    }
  }

  assign(
    name,
    list(
      name = name,
      fun = fun,
      value_arg = value_arg,
      multiple = isTRUE(multiple),
      update_fun = update_fun,
      encode = encode,
      decode = decode,
      format = format
    ),
    envir = .sft_input_registry
  )

  invisible(name)
}

# Look up a registered custom input by its input_type name, or NULL.
sft_registered_input <- function(input_type) {
  if (!is.character(input_type) || length(input_type) != 1L || is.na(input_type)) {
    return(NULL)
  }

  if (!exists(input_type, envir = .sft_input_registry, inherits = FALSE)) {
    return(NULL)
  }

  get(input_type, envir = .sft_input_registry, inherits = FALSE)
}

sft_is_registered_input <- function(input_type) {
  !is.null(sft_registered_input(input_type))
}

sft_supported_input_types <- function() {
  c(
    "textInput",
    "passwordInput",
    "textAreaInput",
    "numericInput",
    "selectInput",
    "selectizeInput",
    "sliderInput",
    "dateInput",
    "dateRangeInput",
    "checkboxInput",
    "checkboxGroupInput",
    "radioButtons",
    "sliderTextInput",
    "multiInput",
    "timeInput",
    "ibanInput"
  )
}

sft_input_function <- function(input_type) {
  builtin <- switch(
    input_type,
    textInput = shiny::textInput,
    passwordInput = shiny::passwordInput,
    textAreaInput = shiny::textAreaInput,
    numericInput = shiny::numericInput,
    selectInput = shiny::selectInput,
    selectizeInput = shiny::selectizeInput,
    sliderInput = shiny::sliderInput,
    dateInput = shiny::dateInput,
    dateRangeInput = shiny::dateRangeInput,
    checkboxInput = shiny::checkboxInput,
    checkboxGroupInput = shiny::checkboxGroupInput,
    radioButtons = shiny::radioButtons,
    sliderTextInput = shinyWidgets::sliderTextInput,
    multiInput = shinyWidgets::multiInput,
    timeInput = shinyTime::timeInput,
    ibanInput = IBANInput,
    NULL
  )

  if (!is.null(builtin)) {
    return(builtin)
  }

  reg <- sft_registered_input(input_type)

  if (!is.null(reg)) {
    return(reg$fun)
  }

  stop(
    "Unsupported input_type: ",
    input_type,
    ". Supported types are: ",
    paste(sft_supported_input_types(), collapse = ", "),
    ".",
    call. = FALSE
  )
}

sft_validate_input_type <- function(input_type) {
  if (!input_type %in% sft_supported_input_types() &&
      !sft_is_registered_input(input_type)) {
    stop(
      "Unsupported input_type: ",
      input_type,
      ". Supported types are: ",
      paste(sft_supported_input_types(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(input_type)
}

sft_input_value_argument <- function(input_type) {
  if (input_type %in% c(
    "selectInput",
    "selectizeInput",
    "checkboxGroupInput",
    "radioButtons",
    "sliderTextInput",
    "multiInput"
  )) {
    return("selected")
  }

  if (identical(input_type, "dateRangeInput")) {
    return(NULL)
  }

  reg <- sft_registered_input(input_type)

  if (!is.null(reg)) {
    return(reg$value_arg)
  }

  "value"
}

sft_input_value_args <- function(input_type, value) {
  if (is.null(value) || length(value) == 0L) {
    return(list())
  }

  if (length(value) == 1L && is.na(value)) {
    return(list())
  }

  if (identical(input_type, "dateRangeInput")) {
    value <- as.Date(value)

    out <- list()

    if (length(value) >= 1L && !is.na(value[1L])) {
      out$start <- value[1L]
    }

    if (length(value) >= 2L && !is.na(value[2L])) {
      out$end <- value[2L]
    }

    return(out)
  }

  value_arg <- sft_input_value_argument(input_type)

  if (is.null(value_arg)) {
    return(list())
  }

  stats::setNames(list(value), value_arg)
}

sft_parse_json_vector <- function(value) {
  if (!is.character(value) || length(value) != 1L || !grepl("^\\s*\\[", value)) {
    return(value)
  }

  tryCatch(
    jsonlite::fromJSON(value),
    error = function(err) value
  )
}

sft_field_db_value <- function(field, value) {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }

  if (identical(field$input_type, "checkboxInput")) {
    if (length(value) == 1L && is.na(value)) {
      return(NA_integer_)
    }

    return(as.integer(isTRUE(value)))
  }

  if (identical(field$input_type, "dateInput")) {
    if (length(value) == 1L && is.na(value)) {
      return(NA_character_)
    }

    return(as.character(as.Date(value)))
  }

  if (identical(field$input_type, "dateRangeInput")) {
    if (all(is.na(value))) {
      return(NA_character_)
    }

    return(as.character(sft_as_json(as.character(as.Date(value)))))
  }

  if (identical(field$input_type, "timeInput")) {
    if (length(value) == 1L && is.na(value)) {
      return(NA_character_)
    }

    if (inherits(value, "POSIXt")) {
      return(format(value, "%H:%M:%S"))
    }

    return(as.character(value))
  }

  if (identical(field$input_type, "ibanInput")) {
    if (length(value) == 1L && is.na(value)) {
      return(NA_character_)
    }

    return(sft_normalize_iban(value))
  }

  if (field$input_type %in% c("checkboxGroupInput", "multiInput")) {
    if (all(is.na(value))) {
      return(NA_character_)
    }

    return(as.character(sft_as_json(as.character(value))))
  }

  if (field$input_type %in% c("selectInput", "selectizeInput")) {
    if (length(value) > 1L) {
      return(as.character(sft_as_json(as.character(value))))
    }
  }

  if (identical(field$input_type, "sliderInput") && length(value) > 1L) {
    return(as.character(sft_as_json(as.numeric(value))))
  }

  reg <- sft_registered_input(field$input_type)

  if (!is.null(reg)) {
    if (length(value) == 1L && is.na(value)) {
      return(NA_character_)
    }

    if (!is.null(reg$encode)) {
      encoded <- reg$encode(value)

      if (is.null(encoded) || length(encoded) == 0L) {
        return(NA_character_)
      }

      return(as.character(encoded))
    }

    if (isTRUE(reg$multiple)) {
      if (all(is.na(value))) {
        return(NA_character_)
      }

      return(as.character(sft_as_json(as.character(value))))
    }
  }

  sft_clean_db_value(value)
}

sft_prepare_input_args <- function(field, value = NULL) {
  local_args <- field$args

  local_args$input_type <- NULL
  local_args$db_val <- NULL

  if (identical(field$input_type, "multiInput")) {
    local_args$multiple <- NULL
  }

  if (!is.null(value)) {
    value_args <- sft_input_value_args(
      input_type = field$input_type,
      value = sft_ui_value(field, value)
    )

    for (arg_name in names(value_args)) {
      local_args[[arg_name]] <- value_args[[arg_name]]
    }
  }

  local_args
}

sft_ui_value <- function(field, value) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }

  if (length(value) == 1L && is.na(value)) {
    return(NULL)
  }

  if (identical(field$input_type, "numericInput")) {
    return(as.numeric(value))
  }

  if (identical(field$input_type, "sliderInput")) {
    parsed <- sft_parse_json_vector(value)
    return(as.numeric(parsed))
  }

  if (identical(field$input_type, "checkboxInput")) {
    return(isTRUE(value) || identical(value, 1L) || identical(value, "1") || identical(value, "TRUE"))
  }

  if (identical(field$input_type, "dateInput")) {
    return(as.Date(value))
  }

  if (identical(field$input_type, "dateRangeInput")) {
    parsed <- sft_parse_json_vector(value)
    return(as.Date(parsed))
  }

  if (identical(field$input_type, "timeInput")) {
    if (inherits(value, "POSIXt")) {
      return(value)
    }

    parsed <- tryCatch(
      as.POSIXct(value, format = "%H:%M:%S"),
      error = function(err) NA
    )

    if (is.na(parsed)) {
      parsed <- tryCatch(
        as.POSIXct(value),
        error = function(err) NA
      )
    }

    if (is.na(parsed)) {
      return(Sys.time())
    }

    return(parsed)
  }

  if (field$input_type %in% c("selectInput", "selectizeInput", "multiInput", "checkboxGroupInput")) {
    return(sft_parse_json_vector(value))
  }

  if (identical(field$input_type, "ibanInput")) {
    return(sft_format_iban(value))
  }

  reg <- sft_registered_input(field$input_type)

  if (!is.null(reg)) {
    if (!is.null(reg$decode)) {
      return(reg$decode(value))
    }

    if (isTRUE(reg$multiple)) {
      return(sft_parse_json_vector(value))
    }
  }

  value
}

sft_field_display_separator <- function(field, value = NULL) {
  if (field$input_type %in% c("dateRangeInput", "sliderInput")) {
    return(" - ")
  }

  "; "
}

sft_format_json_vector_value <- function(value, sep = "; ") {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }

  if (length(value) != 1L) {
    value <- as.character(value)
    value <- value[!is.na(value)]
    return(paste(value, collapse = sep))
  }

  if (is.na(value) || !nzchar(as.character(value))) {
    return(NA_character_)
  }

  parsed <- sft_parse_json_vector(as.character(value))

  if (identical(parsed, value)) {
    return(as.character(value))
  }

  parsed <- as.character(parsed)
  parsed <- parsed[!is.na(parsed)]

  if (length(parsed) == 0L) {
    return(NA_character_)
  }

  paste(parsed, collapse = sep)
}

sft_format_field_display_value <- function(field, value, sep = NULL) {
  sep <- sep %||% sft_field_display_separator(field, value)

  if (field$input_type %in% c(
    "checkboxGroupInput",
    "multiInput",
    "dateRangeInput"
  )) {
    return(sft_format_json_vector_value(value, sep = sep))
  }

  if (field$input_type %in% c("selectInput", "selectizeInput", "sliderInput")) {
    value_chr <- as.character(value)

    if (length(value_chr) == 1L && !is.na(value_chr) && grepl("^\\s*\\[", value_chr)) {
      return(sft_format_json_vector_value(value_chr, sep = sep))
    }
  }

  if (identical(field$input_type, "ibanInput")) {
    return(sft_format_iban(value))
  }

  reg <- sft_registered_input(field$input_type)

  if (!is.null(reg)) {
    if (!is.null(reg$format)) {
      return(reg$format(value))
    }

    if (isTRUE(reg$multiple)) {
      return(sft_format_json_vector_value(value, sep = sep))
    }
  }

  value
}
