test_that("register_input validates its arguments", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  ok <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)

  expect_error(register_input(name = c("a", "b"), fun = ok), "non-empty character")
  expect_error(register_input("textInput", fun = ok), "built-in input type")
  expect_error(register_input("widget", fun = "not a function"), "fun must be a function")
  expect_error(register_input("widget", fun = ok, value_arg = 1), "value_arg")
  expect_error(register_input("widget", fun = ok, multiple = "yes"), "multiple must be")
  expect_error(register_input("widget", fun = ok, encode = "x"), "encode must be")

  expect_identical(register_input("widget", fun = ok), "widget")
  expect_true(sft_is_registered_input("widget"))
})

test_that("a registered input is accepted by form_field and resolves to its fun", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  knob <- function(inputId, label, ...) shiny::numericInput(inputId, label, value = 0)
  register_input("knobInput", fun = knob, value_arg = "value")

  field <- form_field(
    id = "vol", label = "Volume", input_type = "knobInput",
    args = list(min = 0, max = 100)
  )

  expect_identical(field$input_type, "knobInput")
  expect_identical(sft_input_function("knobInput"), knob)
  expect_identical(sft_input_value_argument("knobInput"), "value")

  # An unregistered input type is still rejected at definition time.
  expect_error(
    form_field(id = "x", input_type = "notRegistered"),
    "Unsupported input_type"
  )
})

test_that("single-valued registered input round-trips verbatim", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  txt <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)
  register_input("custom", fun = txt)
  field <- form_field(id = "f", label = "F", input_type = "custom")

  expect_identical(sft_field_db_value(field, "hello"), "hello")
  expect_identical(sft_ui_value(field, "hello"), "hello")
  expect_true(is.na(sft_field_db_value(field, NA_character_)))
})

test_that("multiple = TRUE stores and restores a JSON array", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  txt <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)
  register_input("tags", fun = txt, value_arg = "selected", multiple = TRUE)
  field <- form_field(id = "t", label = "T", input_type = "tags")

  encoded <- sft_field_db_value(field, c("a", "b"))
  expect_identical(encoded, "[\"a\",\"b\"]")
  expect_identical(sft_ui_value(field, encoded), c("a", "b"))
  expect_identical(sft_format_field_display_value(field, encoded), "a; b")
})

test_that("encode / decode / format hooks override the defaults", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  txt <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)
  register_input(
    "scaled",
    fun = txt,
    encode = function(value) as.character(as.numeric(value) * 10),
    decode = function(value) as.numeric(value) / 10,
    format = function(value) paste0(value, " (x10)")
  )
  field <- form_field(id = "s", label = "S", input_type = "scaled", db_type = "REAL")

  encoded <- sft_field_db_value(field, 4)
  expect_identical(encoded, "40")
  expect_identical(sft_ui_value(field, encoded), 4)
  expect_identical(sft_format_field_display_value(field, encoded), "40 (x10)")
})

test_that("dynamic update helpers fall back to a registered update_fun", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  calls <- new.env()
  txt <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)
  fake_update <- function(session, inputId, ...) {
    calls$args <- list(...)
    invisible(NULL)
  }
  register_input("dynWidget", fun = txt, update_fun = fake_update)

  sft_update_value_input(
    session = NULL, input_type = "dynWidget",
    input_id = "dynWidget", value = "42"
  )
  expect_identical(calls$args$value, "42")

  sft_update_choices_input(
    session = NULL, input_type = "dynWidget",
    input_id = "dynWidget", choices = c("a", "b"), selected = "a"
  )
  expect_identical(calls$args$choices, c("a", "b"))
  expect_identical(calls$args$selected, "a")

  # No update_fun -> the helpful error is still raised for an unknown type.
  register_input("noUpdate", fun = txt)
  expect_error(
    sft_update_value_input(NULL, "noUpdate", "noUpdate", "x"),
    "not supported"
  )
})

test_that("a single selection of a multi-value field round-trips as a JSON array", {
  field <- form_field(
    id = "tags", label = "Tags", input_type = "checkboxGroupInput",
    args = list(choices = c("a", "b"))
  )

  encoded <- sft_field_db_value(field, "a")
  expect_identical(encoded, "[\"a\"]")
  expect_identical(sft_ui_value(field, encoded), "a")
  expect_identical(sft_format_json_vector_value(encoded), "a")
})

test_that("registered multiple inputs encode single selections as arrays too", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  txt <- function(inputId, label, ...) shiny::textInput(inputId, label, ...)
  register_input("tags_single", fun = txt, value_arg = "selected", multiple = TRUE)
  field <- form_field(id = "t", label = "T", input_type = "tags_single")

  encoded <- sft_field_db_value(field, "a")
  expect_identical(encoded, "[\"a\"]")
  expect_identical(sft_ui_value(field, encoded), "a")
})

test_that("legacy scalar-encoded single values still decode", {
  # Databases written before single selections were forced to arrays store a
  # bare JSON string scalar, quotes included.
  field <- form_field(
    id = "tags", label = "Tags", input_type = "checkboxGroupInput",
    args = list(choices = c("a", "b"))
  )

  expect_identical(sft_ui_value(field, "\"a\""), "a")
  # Plain (non-JSON) stored values are untouched.
  expect_identical(sft_ui_value(field, "plain"), "plain")
})

test_that("an unparseable stored time never becomes the current time", {
  field <- form_field(id = "at", label = "At", input_type = "timeInput")

  parsed <- sft_ui_value(field, "08:30:00")
  expect_s3_class(parsed, "POSIXct")
  expect_identical(format(parsed, "%H:%M:%S"), "08:30:00")

  # NULL leaves the input as it is; Sys.time() would silently be saved as the
  # record's value on the next submit.
  expect_null(sft_ui_value(field, "not a time"))
  expect_null(sft_ui_value(field, NA_character_))
})

# ---- the input-type table ---------------------------------------------------

test_that("every built-in input type is one complete row of the table", {
  specs <- sft_builtin_input_specs()

  expect_identical(sft_supported_input_types(), names(specs))
  expect_false(anyDuplicated(names(specs)) > 0L)

  for (input_type in names(specs)) {
    spec <- specs[[input_type]]

    expect_true(is.function(spec$fun), info = input_type)
    expect_true(is.function(spec$encode), info = input_type)
    expect_true(is.function(spec$decode), info = input_type)
    expect_true(is.function(spec$format), info = input_type)
    expect_true(spec$db_type %in% c("TEXT", "REAL", "INTEGER"), info = input_type)
    expect_true(is.null(spec$value_arg) || spec$value_arg %in% c("value", "selected"), info = input_type)
    # Every built-in can be driven dynamically one way or the other.
    expect_true(is.function(spec$update_value) || is.function(spec$update_choices), info = input_type)

    # The spec is what the type-keyed helpers return.
    expect_identical(sft_input_function(input_type), spec$fun)
    expect_identical(sft_default_db_type(input_type), spec$db_type)
  }
})

test_that("a stored value survives decode -> encode unchanged for every built-in", {
  samples <- list(
    textInput = "hello",
    passwordInput = "secret",
    textAreaInput = "two\nlines",
    numericInput = 4.5,
    selectInput = "a",
    selectizeInput = "[\"a\",\"b\"]",
    sliderInput = "[1,5]",
    dateInput = "2026-09-21",
    dateRangeInput = "[\"2026-01-01\",\"2026-12-31\"]",
    checkboxInput = 1L,
    checkboxGroupInput = "[\"a\"]",
    radioButtons = "b",
    sliderTextInput = "[\"low\",\"high\"]",
    multiInput = "[\"a\",\"c\"]",
    timeInput = "08:30:00",
    ibanInput = "DE89370400440532013000"
  )

  # A new built-in has to be added here too, or this test says so.
  expect_setequal(names(samples), sft_supported_input_types())

  for (input_type in names(samples)) {
    field <- form_field(id = "f", label = "F", input_type = input_type)
    stored <- samples[[input_type]]

    expect_equal(
      sft_field_db_value(field, sft_ui_value(field, stored)),
      stored,
      info = input_type
    )
  }
})

test_that("a two-handle sliderTextInput round-trips and displays as a range", {
  field <- form_field(
    id = "level", label = "Level", input_type = "sliderTextInput",
    args = list(choices = c("low", "mid", "high"))
  )

  stored <- sft_field_db_value(field, c("low", "high"))
  expect_identical(stored, "[\"low\",\"high\"]")
  # Decoded back to a vector: it used to come back as the raw JSON string, so
  # the edit dialog could not select the stored range.
  expect_identical(sft_ui_value(field, stored), c("low", "high"))
  expect_identical(sft_format_field_display_value(field, stored), "low - high")

  # One handle is stored and shown verbatim.
  expect_identical(sft_field_db_value(field, "mid"), "mid")
  expect_identical(sft_ui_value(field, "mid"), "mid")
  expect_identical(sft_format_field_display_value(field, "mid"), "mid")

  expect_identical(sft_input_value_argument("sliderTextInput"), "selected")
  expect_true(is.function(sft_input_spec("sliderTextInput")$update_choices))
})

test_that("register_input(db_type = ) sets the default column type of its fields", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  dial <- function(inputId, label, ...) shiny::numericInput(inputId, label, value = 0)

  expect_error(register_input("dial", fun = dial, db_type = 1), "db_type must be")

  register_input("dial", fun = dial, decode = as.numeric, db_type = "REAL")
  expect_identical(form_field(id = "v", label = "V", input_type = "dial")$db_type, "REAL")
  # An explicit db_type on the field still wins.
  expect_identical(
    form_field(id = "v", label = "V", input_type = "dial", db_type = "INTEGER")$db_type,
    "INTEGER"
  )

  register_input("plain", fun = dial)
  expect_identical(form_field(id = "p", label = "P", input_type = "plain")$db_type, "TEXT")
})

test_that("dynamic values work for choice inputs, and an empty value clears the selection", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  for (input_type in c("selectInput", "selectizeInput", "radioButtons",
                       "checkboxGroupInput", "multiInput", "sliderTextInput")) {
    spec <- sft_input_spec(input_type)
    expect_true(is.function(spec$update_value), info = input_type)
    expect_identical(spec$update_value, spec$update_choices, info = input_type)
  }

  # It used to stop with "Dynamic values are not supported for input type
  # selectInput yet".
  session <- shiny::MockShinySession$new()
  expect_no_error(sft_update_value_input(session, "selectInput", "x", "a"))
  expect_no_error(sft_update_value_input(session, "checkboxGroupInput", "x", c("a", "b")))

  calls <- new.env()
  pick <- function(inputId, label, ...) shiny::selectInput(inputId, label, choices = c("a", "b"))
  fake_update <- function(session, inputId, ...) {
    calls$args <- list(...)
    invisible(NULL)
  }
  register_input("picker", fun = pick, value_arg = "selected", update_fun = fake_update)

  sft_update_value_input(NULL, "picker", "picker", "b")
  expect_identical(calls$args$selected, "b")

  sft_update_value_input(NULL, "picker", "picker", NULL)
  expect_identical(calls$args$selected, character(0))

  # A value input is not touched by an empty value.
  register_input("plainText", fun = pick, update_fun = fake_update)
  calls$args <- "untouched"
  sft_update_value_input(NULL, "plainText", "plainText", NULL)
  expect_identical(calls$args, list())
})

test_that("the conflict view pushes stored values, and empties, through the type's update function", {
  withr::defer(rm(list = ls(.sft_input_registry), envir = .sft_input_registry))

  calls <- new.env()
  widget <- function(inputId, label, ...) shiny::textInput(inputId, label)
  fake_update <- function(session, inputId, ...) {
    calls$id <- inputId
    calls$args <- list(...)
    invisible(NULL)
  }
  register_input("cPick", fun = widget, value_arg = "selected", multiple = TRUE, update_fun = fake_update)
  register_input("cText", fun = widget, update_fun = fake_update)

  pick <- form_field(id = "tags", label = "Tags", input_type = "cPick")
  text <- form_field(id = "note", label = "Note", input_type = "cText")

  sft_conflict_set_input(NULL, pick, "[\"a\",\"b\"]")
  expect_identical(calls$id, "edit_tags")
  expect_identical(calls$args$selected, c("a", "b"))

  # An empty stored value clears a choice input and blanks a text input.
  sft_conflict_set_input(NULL, pick, NA_character_)
  expect_identical(calls$args$selected, character(0))

  sft_conflict_set_input(NULL, text, NA_character_)
  expect_identical(calls$args$value, "")

  # Built-in empties: a checkbox falls back to FALSE.
  expect_identical(sft_input_spec("checkboxInput")$empty, FALSE)
  expect_null(sft_input_spec("selectInput")$empty)

  # An input without an update function is left alone instead of erroring.
  register_input("cStatic", fun = widget)
  expect_no_error(
    sft_conflict_set_input(NULL, form_field(id = "s", label = "S", input_type = "cStatic"), "x")
  )
})
