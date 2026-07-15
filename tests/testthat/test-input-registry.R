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
