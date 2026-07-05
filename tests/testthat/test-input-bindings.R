test_that("dynamic choice bindings validate fields and dependencies", {
  form <- form(
    form_id = "binding_test",
    fields = list(
      form_field("street", input_type = "selectizeInput"),
      form_field("house_no", input_type = "selectizeInput"),
      form_field("zip")
    )
  )

  binding <- dynamic_choices(
    field = "house_no",
    depends_on = "street",
    choices = function() c("1", "2")
  )

  out <- sft_validate_input_bindings(list(binding), form)
  expect_length(out, 1L)
  expect_s3_class(out[[1L]], "sft_input_binding")

  expect_error(
    sft_validate_input_bindings(
      list(dynamic_choices("unknown", choices = function() character())),
      form
    ),
    "Unknown input binding field"
  )

  expect_error(
    sft_validate_input_bindings(
      list(dynamic_choices("house_no", depends_on = "unknown", choices = function() character())),
      form
    ),
    "Unknown input binding dependency"
  )
})

test_that("dynamic choice helpers normalize choices and preserve valid values", {
  choices <- sft_normalize_choices(
    data.frame(
      value = c("p1", "p2"),
      label = c("Ada", "Grace"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(unname(choices), c("p1", "p2"))
  expect_equal(names(choices), c("Ada", "Grace"))

  selected <- sft_resolve_choice_selection(
    selected = "preserve",
    choices = choices,
    input = NULL,
    context = list(),
    field = list(),
    prefix = "add_",
    values = list(),
    current = "p2"
  )

  expect_equal(selected, "p2")

  selected <- sft_resolve_choice_selection(
    selected = "preserve",
    choices = choices,
    input = NULL,
    context = list(),
    field = list(),
    prefix = "add_",
    values = list(),
    current = "p3"
  )

  expect_equal(selected, "p1")
})

test_that("dynamic value binding stores its configuration", {
  binding <- dynamic_value(
    field = "zip",
    depends_on = c("street", "house_no"),
    value = function(values) "39104"
  )

  expect_s3_class(binding, "sft_input_binding")
  expect_equal(binding$type, "value")
  expect_equal(binding$field, "zip")
  expect_equal(binding$depends_on, c("street", "house_no"))
})

test_that("dynamic visibility binding stores its configuration and validates", {
  form <- form(
    form_id = "visibility_test",
    fields = list(
      form_field("marital_status", input_type = "selectInput"),
      form_field("spouse_name")
    )
  )

  binding <- dynamic_visibility(
    field = "spouse_name",
    depends_on = "marital_status",
    visible = function(values) identical(values$marital_status, "Married")
  )

  expect_s3_class(binding, "sft_input_binding")
  expect_equal(binding$type, "visibility")
  expect_equal(binding$field, "spouse_name")

  out <- sft_validate_input_bindings(list(binding), form)
  expect_length(out, 1L)

  expect_error(
    sft_validate_input_bindings(
      list(dynamic_visibility("unknown", visible = function() TRUE)),
      form
    ),
    "Unknown input binding field"
  )
})

test_that("hidden fields are dropped from values on save", {
  form <- form(
    form_id = "visibility_drop",
    fields = list(
      form_field("marital_status", input_type = "selectInput"),
      form_field("spouse_name")
    )
  )

  bindings <- list(
    dynamic_visibility(
      field = "spouse_name",
      depends_on = "marital_status",
      visible = function(values) identical(values$marital_status, "Married")
    )
  )

  # Hidden: spouse_name dropped because marital_status is not "Married".
  hidden <- sft_drop_hidden_field_values(
    bindings, form,
    list(marital_status = "Single", spouse_name = "stale value")
  )
  expect_false("spouse_name" %in% names(hidden))
  expect_equal(hidden$marital_status, "Single")

  # Visible: spouse_name kept when the predicate is TRUE.
  shown <- sft_drop_hidden_field_values(
    bindings, form,
    list(marital_status = "Married", spouse_name = "Ada")
  )
  expect_equal(shown$spouse_name, "Ada")

  # No visibility bindings: values pass through unchanged.
  passthrough <- sft_drop_hidden_field_values(
    list(), form,
    list(marital_status = "Single", spouse_name = "kept")
  )
  expect_equal(passthrough$spouse_name, "kept")
})

test_that("date range dynamic value updates use start and end arguments", {
  args <- sft_input_value_args(
    input_type = "dateRangeInput",
    value = as.Date(c("2026-06-01", "2026-06-05"))
  )

  expect_false("value" %in% names(args))
  expect_equal(args$start, as.Date("2026-06-01"))
  expect_equal(args$end, as.Date("2026-06-05"))
})
