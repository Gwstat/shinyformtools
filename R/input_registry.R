# Runtime registry of user-defined input types, populated by register_input().
# A registered input is normalised into the same spec shape as a built-in
# (input_types.R), so the helpers below never distinguish the two: they look
# the spec up with sft_input_spec() and use it. The environment is created
# once when the namespace loads and is shared across the R session.
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
#' @param db_type Optional default column type for fields of this input type,
#'   for example `"REAL"` for a numeric widget. `NULL` means `"TEXT"`. A
#'   `db_type` given to [form_field()] still wins.
#'
#' @return Invisibly, `name`.
#' @examples
#' \dontrun{
#' # A shinyWidgets knob as a numeric input.
#' register_input(
#'   "knobInput",
#'   fun = shinyWidgets::knobInput,
#'   value_arg = "value",
#'   update_fun = shinyWidgets::updateKnobInput,
#'   decode = as.numeric,
#'   db_type = "REAL"
#' )
#'
#' vol <- form_field(
#'   id = "vol", label = "Volume", input_type = "knobInput",
#'   args = list(min = 0, max = 100)
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
                           format = NULL,
                           db_type = NULL) {
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

  if (!is.null(db_type) && !sft_is_scalar_character(db_type)) {
    stop("db_type must be NULL or a non-empty character scalar.", call. = FALSE)
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
      format = format,
      db_type = db_type
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

# ---- helpers keyed by input type ---------------------------------------------
# Each one looks the spec up (input_types.R) and uses it; none of them knows a
# type by name.

sft_supported_input_types <- function() {
  names(sft_builtin_input_specs())
}

sft_unsupported_input_type <- function(input_type) {
  stop(
    "Unsupported input_type: ",
    input_type,
    ". Supported types are: ",
    paste(sft_supported_input_types(), collapse = ", "),
    ".",
    call. = FALSE
  )
}

sft_input_function <- function(input_type) {
  spec <- sft_input_spec(input_type)

  if (is.null(spec)) {
    sft_unsupported_input_type(input_type)
  }

  spec$fun
}

sft_validate_input_type <- function(input_type) {
  if (is.null(sft_input_spec(input_type))) {
    sft_unsupported_input_type(input_type)
  }

  invisible(input_type)
}

# Default column type of a field with this input type.
sft_default_db_type <- function(input_type) {
  spec <- sft_input_spec(input_type)

  if (is.null(spec)) {
    return("TEXT")
  }

  spec$db_type
}

sft_input_value_argument <- function(input_type) {
  spec <- sft_input_spec(input_type)

  if (is.null(spec)) {
    return("value")
  }

  spec$value_arg
}

sft_input_value_args <- function(input_type, value) {
  if (is.null(value) || length(value) == 0L) {
    return(list())
  }

  if (length(value) == 1L && is.na(value)) {
    return(list())
  }

  spec <- sft_input_spec(input_type)

  if (!is.null(spec) && is.function(spec$value_args)) {
    return(spec$value_args(value))
  }

  value_arg <- sft_input_value_argument(input_type)

  if (is.null(value_arg)) {
    return(list())
  }

  stats::setNames(list(value), value_arg)
}

sft_parse_json_vector <- function(value) {
  if (!is.character(value) || length(value) != 1L) {
    return(value)
  }

  # "[...]" arrays are the canonical multi-value encoding. Bare JSON string
  # scalars ("\"a\"") exist in databases written before single selections were
  # forced to arrays, so parse those too.
  if (!grepl("^\\s*\\[", value) && !grepl("^\\s*\"", value)) {
    return(value)
  }

  tryCatch(
    jsonlite::fromJSON(value),
    error = function(err) value
  )
}

# Input value -> scalar stored in the database.
sft_field_db_value <- function(field, value) {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }

  spec <- sft_input_spec(field$input_type)

  if (is.null(spec)) {
    return(sft_clean_db_value(value))
  }

  spec$encode(value)
}

sft_prepare_input_args <- function(field, value = NULL) {
  local_args <- field$args

  local_args$input_type <- NULL
  local_args$db_val <- NULL

  spec <- sft_input_spec(field$input_type)

  if (!is.null(spec) && is.function(spec$prepare_args)) {
    local_args <- spec$prepare_args(local_args)
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

# Stored value -> value handed to the input. Empty stored values are NULL for
# every type, so a decode function never sees NULL or NA.
sft_ui_value <- function(field, value) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }

  if (length(value) == 1L && is.na(value)) {
    return(NULL)
  }

  spec <- sft_input_spec(field$input_type)

  if (is.null(spec)) {
    return(value)
  }

  spec$decode(value)
}

sft_field_display_separator <- function(field, value = NULL) {
  spec <- sft_input_spec(field$input_type)

  if (is.null(spec)) {
    return("; ")
  }

  spec$sep
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

# Stored value -> string shown in the records and versions tables.
sft_format_field_display_value <- function(field, value, sep = NULL) {
  sep <- sep %||% sft_field_display_separator(field, value)
  spec <- sft_input_spec(field$input_type)

  if (is.null(spec)) {
    return(value)
  }

  spec$format(value, sep)
}
