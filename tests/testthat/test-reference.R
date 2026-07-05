testthat::test_that("reference choices use sft_easy_id as stable default value", {
  data <- data.frame(
    sft_easy_id = c("1-ABCD", "2-EFGH"),
    name = c("Ada Lovelace", "Grace Hopper"),
    phone = c("0391 111", "0391 222"),
    stringsAsFactors = FALSE
  )

  choices <- reference_choices(
    data = data,
    label = "name",
    extra = "phone",
    include_empty = TRUE
  )

  testthat::expect_equal(unname(choices), c("", "1-ABCD", "2-EFGH"))
  testthat::expect_equal(names(choices), c("", "Ada Lovelace · 0391 111", "Grace Hopper · 0391 222"))
})

testthat::test_that("reference choices validate missing columns", {
  data <- data.frame(
    sft_easy_id = "1-ABCD",
    name = "Ada Lovelace",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    reference_choices(data, value = "missing"),
    "Reference value column not found"
  )

  testthat::expect_error(
    reference_choices(data, label = "missing"),
    "Reference label column not found"
  )

  testthat::expect_error(
    reference_choices(data, extra = "phone"),
    "Reference extra column"
  )
})
