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
#   clear          optional function(session, inputId) that empties the input
#                  when an empty value is pushed (shinygridtools' grid clears
#                  every cell)

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

# A single value for a text column. A number from a script is stored as its
# plain digits on every backend: bound as a number, SQLite and DuckDB wrote
# "7.0" and MariaDB "7", and an upsert key then found the record on one
# backend only. TRUE / FALSE become "1" / "0", as SQLite and MariaDB stored
# them (DuckDB wrote "true").
# A number as text without losing a digit: whole numbers up to 2^53 as plain
# digits, anything else with the fewest significant digits (15 to 17) that
# read back as the same number. 15 digits alone turned 1234567890123456 into
# 1.23456789012346e+15, and two account numbers into one upsert key.
sft_number_text <- function(value) {
  if (value == round(value) && abs(value) < 2^53) {
    return(formatC(value + 0, format = "f", digits = 0, big.mark = ""))
  }

  # Plain decimals in the usual range ("0.0001", not "1e-04"), as the table
  # showed them before.
  scientific <- abs(value) >= 1e15 || abs(value) < 1e-15
  for (digits in 15:17) {
    text <- format(value, digits = digits, trim = TRUE, scientific = scientific)
    if (identical(as.numeric(text), value)) {
      return(text)
    }
  }

  text
}

# Every spelling under which a number may sit in a text column: the current
# one, and what earlier versions stored (0.7.0: 15 digits; up to 0.5.3 the
# database's own text for a bound number, "7.0" and "1.0e-05" on SQLite,
# "1e15" on MariaDB). Upsert keys match any of them.
sft_number_spellings <- function(value) {
  current <- sft_number_text(value)
  sqlite <- sprintf("%.15g", value)
  if (!grepl(".", sqlite, fixed = TRUE)) {
    sqlite <- if (grepl("e", sqlite, fixed = TRUE)) sub("e", ".0e", sqlite, fixed = TRUE) else paste0(sqlite, ".0")
  }
  unique(c(
    current,
    if (!grepl("[.e]", current)) paste0(current, ".0"),
    format(value, digits = 15, trim = TRUE, scientific = abs(value) >= 1e15),
    sqlite,
    gsub("e+", "e", format(value, digits = 15, trim = TRUE), fixed = TRUE),
    format(value, digits = 15, trim = TRUE, scientific = FALSE)
  ))
}

sft_encode_text <- function(value) {
  if (inherits(value, "integer64")) {
    value <- as.numeric(value)
  }

  if (length(value) == 1L && !is.na(value) && (is.numeric(value) || is.logical(value)) &&
      !inherits(value, c("Date", "POSIXt", "factor"))) {
    if (is.logical(value)) {
      return(as.character(as.integer(value)))
    }
    if (is.finite(value)) {
      return(sft_number_text(value))
    }
  }

  sft_clean_db_value(value)
}

# select-like inputs are single-valued unless `multiple = TRUE` was passed in
# the field's args: a vector becomes a JSON array, a scalar is stored verbatim.
sft_encode_json_if_multiple <- function(value) {
  if (length(value) > 1L) {
    return(as.character(sft_as_json_array(as.character(value))))
  }

  sft_encode_text(value)
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

  # "10:30" written by a script: without this the dialog showed 00:00:00 and
  # the next save stored it.
  if (is.na(parsed)) {
    parsed <- tryCatch(
      as.POSIXct(value, format = "%H:%M"),
      error = function(err) NA
    )
  }

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

# A number as people write it: digits with an optional sign, decimal point
# and exponent. as.numeric() alone also took "0x1A" (26) and "Inf".
sft_is_decimal_text <- function(text) {
  grepl("^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$", trimws(text))
}

# A number field's value for the database. Text from a script or an import is
# read as a number, and blank text is empty. Text that is no number is refused
# by validation before it gets here (sft_number_issues).
sft_encode_number <- function(value) {
  if (is.character(value) && length(value) == 1L) {
    value <- trimws(value)
    if (is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }
    number <- if (sft_is_decimal_text(value)) suppressWarnings(as.numeric(value)) else NA_real_
    if (!is.na(number)) {
      value <- number
    }
  }

  sft_clean_db_value(value)
}

# A spec with the defaults of a plain single-valued text input; rows override
# what differs.
sft_input_spec_row <- function(fun,
                               value_arg = "value",
                               value_args = NULL,
                               db_type = "TEXT",
                               encode = sft_encode_text,
                               decode = identity,
                               format = function(value, sep) value,
                               sep = "; ",
                               prepare_args = NULL,
                               update_value = NULL,
                               update_choices = NULL,
                               empty = "",
                               clear = NULL,
                               blank = "auto",
                               validate = NULL,
                               server = NULL) {
  # Arguments that render the input EMPTY, for a record whose stored value is
  # empty. Without them the edit dialog showed the input's own default (today
  # for a date, the first choice of radio buttons, args$value of a number),
  # and saving wrote that into a field the user never touched. "auto": choice
  # inputs select nothing, text inputs are "". NULL: the type has no empty
  # state (a slider) and keeps its default.
  if (identical(blank, "auto")) {
    blank <- if (identical(value_arg, "selected")) {
      list(selected = character(0))
    } else if (identical(value_arg, "value") && identical(empty, "")) {
      list(value = "")
    }
  }

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
    empty = empty,
    clear = clear,
    blank = blank,
    # Extension hooks (see register_input()): validate(form, fields, record,
    # conn, current_id) returns issues; server(input, output, session,
    # state, fields) runs inside form_server().
    validate = validate,
    server = server
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
      encode = sft_encode_number,
      decode = as.numeric,
      update_value = shiny::updateNumericInput,
      blank = list(value = NA)
    ),

    selectInput = single_or_multi_choice(shiny::selectInput, shiny::updateSelectInput),
    selectizeInput = single_or_multi_choice(shiny::selectizeInput, shiny::updateSelectizeInput),

    sliderInput = row(
      shiny::sliderInput,
      db_type = "REAL",
      encode = function(value) {
        if (length(value) > 1L) {
          value <- suppressWarnings(as.numeric(value))
          if (all(is.na(value))) {
            return(NA_character_)
          }
          # A missing end is JSON null; the default wrote the string "NA".
          return(as.character(jsonlite::toJSON(value, na = "null", digits = NA)))
        }

        sft_encode_number(value)
      },
      decode = function(value) as.numeric(sft_parse_json_vector(value)),
      format = sft_format_json_if_array,
      sep = " - ",
      update_value = shiny::updateSliderInput,
      blank = NULL
    ),

    dateInput = row(
      shiny::dateInput,
      encode = function(value) {
        if (sft_is_single_na(value)) {
          return(NA_character_)
        }
        # The calendar date of a time as the caller sees it; as.Date() takes
        # the UTC date, so 00:30 in Berlin became the day before.
        if (inherits(value, "POSIXt")) {
          return(format(value, "%Y-%m-%d"))
        }
        as.character(as.Date(value))
      },
      decode = as.Date,
      update_value = shiny::updateDateInput,
      blank = list(value = NA)
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
      update_value = shiny::updateDateRangeInput,
      blank = list(start = NA, end = NA)
    ),

    checkboxInput = row(
      shiny::checkboxInput,
      db_type = "INTEGER",
      # One notion of "true" in both directions (sft_truthy: TRUE, a non-zero
      # number, "1" / "true" / "yes"). Encoding used to be as.integer(isTRUE(x)),
      # so a script that inserted 1, "1" or "TRUE" - an import from a table -
      # silently stored 0. The checkbox itself always delivers TRUE / FALSE.
      encode = function(value) {
        if (sft_is_single_na(value)) NA_integer_ else as.integer(sft_truthy(value))
      },
      decode = function(value) sft_truthy(value),
      update_value = shiny::updateCheckboxInput,
      empty = FALSE,
      blank = list(value = FALSE)
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
      empty = NULL,
      blank = NULL
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
      update_value = shinyTime::updateTimeInput,
      # shinyTime has no empty state that is safe to render; keeps its default.
      blank = NULL
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
    empty = if (is.null(reg$empty)) {
      if (identical(reg$value_arg, "selected")) NULL else ""
    } else {
      reg$empty
    },
    sep = reg$sep %||% "; ",
    prepare_args = reg$prepare_args,
    clear = reg$clear,
    blank = reg$blank %||% "auto",
    validate = reg$validate,
    server = reg$server
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
