test_that("IBAN helpers normalize, format and validate IBANs", {
  expect_equal(sft_normalize_iban("de89 3704 0044 0532 0130 00"), "DE89370400440532013000")
  expect_equal(sft_format_iban("DE89370400440532013000"), "DE89 3704 0044 0532 0130 00")
  expect_true(is_valid_iban("DE89 3704 0044 0532 0130 00"))
  expect_false(is_valid_iban("DE00 0000 0000 0000 0000 00"))
})

test_that("ibanInput is part of the form input registry", {
  expect_true("ibanInput" %in% sft_supported_input_types())

  field <- form_field(
    id = "iban",
    label = "IBAN",
    input_type = "ibanInput"
  )

  expect_equal(sft_field_db_value(field, "DE89 3704 0044 0532 0130 00"), "DE89370400440532013000")
  expect_equal(sft_ui_value(field, "DE89370400440532013000"), "DE89 3704 0044 0532 0130 00")
})
