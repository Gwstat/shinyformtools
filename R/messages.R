sft_default_messages <- function() {
  list(
    mandatory_missing = "Mandatory fields missing: {fields}.",
    mandatory_empty = "Mandatory fields are empty: {fields}.",
    unique = "The value for '{label}' is already taken.",
    conditional_required = "Conditional mandatory fields missing: {fields}.",
    validation_rule_failed = "Validation rule '{rule}' failed.",
    no_active_fields_for_update = "No active form fields were provided to update."
  )
}

sft_interpolate_text <- function(text, values = list()) {
  if (is.function(text)) {
    return(text(values))
  }

  if (is.null(text)) {
    return(NA_character_)
  }

  out <- as.character(text)

  for (name in names(values)) {
    out <- gsub(
      pattern = paste0("{", name, "}"),
      replacement = as.character(values[[name]]),
      x = out,
      fixed = TRUE
    )
  }

  out
}

sft_form_messages <- function(form = NULL, messages = list()) {
  defaults <- sft_default_messages()

  option_messages <- getOption("shinyformtools.messages", list())

  if (!is.list(option_messages)) {
    option_messages <- list()
  }

  form_messages <- if (!is.null(form) && !is.null(form$messages)) {
    form$messages
  } else {
    list()
  }

  # English default <- global option (use_german) <- form() messages <- call arg.
  utils::modifyList(
    utils::modifyList(
      utils::modifyList(defaults, option_messages),
      form_messages
    ),
    messages
  )
}

sft_message <- function(form, key, values = list(), messages = list()) {
  resolved <- sft_form_messages(
    form = form,
    messages = messages
  )

  sft_interpolate_text(
    text = resolved[[key]],
    values = values
  )
}
