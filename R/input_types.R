# The input-type table. One spec per input type describes everything the
# package needs to know about it; every helper that used to switch on
# `input_type` (render, value placement, DB encode/decode, table display,
# dynamic updates, default column type, the conflict view) looks the spec up
# with sft_input_spec() instead. Adding a built-in is one entry here; a custom
# input registered with register_input() is normalised into the same shape, so
# built-ins and custom inputs take exactly the same code paths.
#
# A spec is a list with:
#   fun            the Shiny input function
#   value_arg      argument carrying the value ("value", "selected") or NULL
#   value_args     optional function(value) -> named list, for inputs whose
#                  value spans several arguments (dateRangeInput: start/end)
#   db_type        default column type of a field of this type
#   encode         function(value) -> scalar stored in the database
#   decode         function(value) -> value handed to the input (stored value
#                  is never NULL/NA here; sft_ui_value() guards that)
#   format         function(value, sep) -> string shown in the tables
#   sep            separator used when a multi-value is displayed
#   prepare_args   optional function(args) -> args, last-minute argument scrub
#   update_value   update function used for dynamic values, or NULL
#   update_choices update function used for dynamic choices, or NULL. For
#                  choice inputs both are the same function: it takes
#                  `selected` with or without `choices`
#   empty          what the conflict view pushes into the input for an empty
#                  stored value; NULL for choice inputs, which
#                  sft_update_value_input() turns into "nothing selected"

.sft_input_type_cache <- new.env(parent = emptyenv())

# ---- building blocks shared by several rows ---------------------------------

sft_is_single_na <- function(value) {
  length(value) == 1L && is.na(value)
}

sft_encode_json_strings <- function(value) {
  if (all(is.na(value))) {
    return(NA_character_)
  }

  as.character(sft_as_json_array(as.character(value)))
}

# select-like inputs are single-valued unless `multiple = TRUE` was passed in
# the field's args: a vector becomes a JSON array, a scalar is stored verbatim.
sft_encode_json_if_multiple <- function(value) {
  if (length(value) > 1L) {
    return(as.character(sft_as_json_array(as.character(value))))
  }

  sft_clean_db_value(value)
}

sft_format_json_if_array <- function(value, sep) {
  value_chr <- as.character(value)

  if (length(value_chr) == 1L && !is.na(value_chr) && grepl("^\\s*\\[", value_chr)) {
    return(sft_format_json_vector_value(value_chr, sep = sep))
  }

  value
}

sft_decode_time <- function(value) {
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

  # A stored value nothing can parse yields NULL, i.e. the input keeps its
  # current state. It used to return Sys.time(), which quietly turned an
  # unreadable stored time into "now" on the next save.
  if (is.na(parsed)) {
    return(NULL)
  }

  parsed
}

# A spec with the defaults of a plain single-valued text input; rows override
# what differs.
sft_input_spec_row <- function(fun,
                               value_arg = "value",
                               value_args = NULL,
                               db_type = "TEXT",
                               encode = sft_clean_db_value,
                               decode = identity,
                               format = function(value, sep) value,
                               sep = "; ",
                               prepare_args = NULL,
                               update_value = NULL,
                               update_choices = NULL,
                               empty = "") {
  list(
    fun = fun,
    value_arg = value_arg,
    value_args = value_args,
    db_type = db_type,
    encode = encode,
    decode = decode,
    format = format,
    sep = sep,
    prepare_args = prepare_args,
    update_value = update_value,
    update_choices = update_choices,
    empty = empty
  )
}

# ---- the built-in rows -------------------------------------------------------

sft_builtin_input_specs <- function() {
  cached <- .sft_input_type_cache$builtin

  if (!is.null(cached)) {
    return(cached)
  }

  row <- sft_input_spec_row

  multi_choice <- function(fun, update) {
    row(
      fun = fun,
      value_arg = "selected",
      encode = sft_encode_json_strings,
      decode = sft_parse_json_vector,
      format = function(value, sep) sft_format_json_vector_value(value, sep = sep),
      update_value = update,
      update_choices = update,
      empty = NULL
    )
  }

  single_or_multi_choice <- function(fun, update) {
    row(
      fun = fun,
      value_arg = "selected",
      encode = sft_encode_json_if_multiple,
      decode = sft_parse_json_vector,
      format = sft_format_json_if_array,
      update_value = update,
      update_choices = update,
      empty = NULL
    )
  }

  specs <- list(
    textInput = row(shiny::textInput, update_value = shiny::updateTextInput),
    passwordInput = row(shiny::passwordInput, update_value = shiny::updateTextInput),
    textAreaInput = row(shiny::textAreaInput, update_value = shiny::updateTextAreaInput),

    numericInput = row(
      shiny::numericInput,
      db_type = "REAL",
      decode = as.numeric,
      update_value = shiny::updateNumericInput
    ),

    selectInput = single_or_multi_choice(shiny::selectInput, shiny::updateSelectInput),
    selectizeInput = single_or_multi_choice(shiny::selectizeInput, shiny::updateSelectizeInput),

    sliderInput = row(
      shiny::sliderInput,
      db_type = "REAL",
      encode = function(value) {
        if (length(value) > 1L) {
          return(as.character(sft_as_json_array(as.numeric(value))))
        }

        sft_clean_db_value(value)
      },
      decode = function(value) as.numeric(sft_parse_json_vector(value)),
      format = sft_format_json_if_array,
      sep = " - ",
      update_value = shiny::updateSliderInput
    ),

    dateInput = row(
      shiny::dateInput,
      encode = function(value) {
        if (sft_is_single_na(value)) NA_character_ else as.character(as.Date(value))
      },
      decode = as.Date,
      update_value = shiny::updateDateInput
    ),

    dateRangeInput = row(
      shiny::dateRangeInput,
      value_arg = NULL,
      value_args = function(value) {
        value <- as.Date(value)
        out <- list()

        if (length(value) >= 1L && !is.na(value[1L])) {
          out$start <- value[1L]
        }

        if (length(value) >= 2L && !is.na(value[2L])) {
          out$end <- value[2L]
        }

        out
      },
      encode = function(value) {
        if (all(is.na(value))) {
          return(NA_character_)
        }

        as.character(sft_as_json_array(as.character(as.Date(value))))
      },
      decode = function(value) as.Date(sft_parse_json_vector(value)),
      format = function(value, sep) sft_format_json_vector_value(value, sep = sep),
      sep = " - ",
      update_value = shiny::updateDateRangeInput
    ),

    checkboxInput = row(
      shiny::checkboxInput,
      db_type = "INTEGER",
      encode = function(value) {
        if (sft_is_single_na(value)) NA_integer_ else as.integer(isTRUE(value))
      },
      decode = function(value) {
        isTRUE(value) || identical(value, 1L) || identical(value, "1") || identical(value, "TRUE")
      },
      update_value = shiny::updateCheckboxInput,
      empty = FALSE
    ),

    checkboxGroupInput = multi_choice(shiny::checkboxGroupInput, shiny::updateCheckboxGroupInput),

    radioButtons = row(
      shiny::radioButtons,
      value_arg = "selected",
      update_value = shiny::updateRadioButtons,
      update_choices = shiny::updateRadioButtons,
      empty = NULL
    ),

    # One handle stores the label verbatim; a two-handle range is a JSON array,
    # decoded back to a vector and shown as "from - to".
    sliderTextInput = row(
      shinyWidgets::sliderTextInput,
      value_arg = "selected",
      encode = sft_encode_json_if_multiple,
      decode = sft_parse_json_vector,
      format = sft_format_json_if_array,
      sep = " - ",
      update_value = shinyWidgets::updateSliderTextInput,
      update_choices = shinyWidgets::updateSliderTextInput,
      empty = NULL
    ),

    multiInput = utils::modifyList(
      multi_choice(shinyWidgets::multiInput, shinyWidgets::updateMultiInput),
      list(
        # multiInput() has no `multiple` argument; a field copied from a
        # selectInput definition must not pass it through.
        prepare_args = function(args) {
          args$multiple <- NULL
          args
        }
      )
    ),

    timeInput = row(
      shinyTime::timeInput,
      encode = function(value) {
        if (sft_is_single_na(value)) {
          return(NA_character_)
        }

        if (inherits(value, "POSIXt")) {
          return(format(value, "%H:%M:%S"))
        }

        as.character(value)
      },
      decode = sft_decode_time,
      update_value = shinyTime::updateTimeInput
    ),

    ibanInput = row(
      ibanInput,
      encode = function(value) {
        if (sft_is_single_na(value)) NA_character_ else sft_normalize_iban(value)
      },
      decode = sft_format_iban,
      format = function(value, sep) sft_format_iban(value),
      update_value = updateIbanInput
    )
  )

  assign("builtin", specs, envir = .sft_input_type_cache)
  specs
}

# ---- registered inputs, normalised into the same shape -----------------------

sft_registered_input_spec <- function(reg) {
  encode <- function(value) {
    if (sft_is_single_na(value)) {
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
      return(sft_encode_json_strings(value))
    }

    sft_clean_db_value(value)
  }

  decode <- function(value) {
    if (!is.null(reg$decode)) {
      return(reg$decode(value))
    }

    if (isTRUE(reg$multiple)) {
      return(sft_parse_json_vector(value))
    }

    value
  }

  format <- function(value, sep) {
    if (!is.null(reg$format)) {
      return(reg$format(value))
    }

    if (isTRUE(reg$multiple)) {
      return(sft_format_json_vector_value(value, sep = sep))
    }

    value
  }

  sft_input_spec_row(
    fun = reg$fun,
    value_arg = reg$value_arg,
    db_type = reg$db_type %||% "TEXT",
    encode = encode,
    decode = decode,
    format = format,
    update_value = reg$update_fun,
    update_choices = reg$update_fun,
    empty = if (identical(reg$value_arg, "selected")) NULL else ""
  )
}

# The spec of an input type: a built-in row, else a registered input, else NULL.
sft_input_spec <- function(input_type) {
  if (!is.character(input_type) || length(input_type) != 1L || is.na(input_type)) {
    return(NULL)
  }

  builtin <- sft_builtin_input_specs()[[input_type]]

  if (!is.null(builtin)) {
    return(builtin)
  }

  reg <- sft_registered_input(input_type)

  if (!is.null(reg)) {
    return(sft_registered_input_spec(reg))
  }

  NULL
}
