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
    "multiInput",
    "timeInput",
    "ibanInput"
  )
}

sft_input_function <- function(input_type) {
  switch(
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
    multiInput = shinyWidgets::multiInput,
    timeInput = shinyTime::timeInput,
    ibanInput = IBANInput,
    stop(
      "Unsupported input_type: ",
      input_type,
      ". Supported types are: ",
      paste(sft_supported_input_types(), collapse = ", "),
      ".",
      call. = FALSE
    )
  )
}

sft_validate_input_type <- function(input_type) {
  if (!input_type %in% sft_supported_input_types()) {
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
    "multiInput"
  )) {
    return("selected")
  }

  if (identical(input_type, "dateRangeInput")) {
    return(NULL)
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

  value
}
