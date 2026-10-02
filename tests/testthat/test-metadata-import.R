testthat::test_that("sft_form_from_metadata maps legacy input rows to fields", {
  inputs <- data.frame(
    Fragebogen = c("basis", "basis", "other", "basis"),
    Auswahl = c(TRUE, TRUE, TRUE, FALSE),
    Id = c("name", "topics", "other_name", "ignored"),
    label = c("Name", "Themen", "Andere", "Ignoriert"),
    inputObj = c("textInput", "checkboxGroupInput", "textInput", "textInput"),
    args_json = c("{}", '{"choices":["A","B"]}', "{}", "{}"),
    mandatory = c(TRUE, FALSE, FALSE, FALSE),
    toggleunique = c(TRUE, FALSE, FALSE, FALSE),
    tab = c(0L, 1L, 0L, 0L),
    col = c(0L, 0L, 0L, 0L),
    pos = c(1L, 2L, 1L, 1L)
  )

  settings <- data.frame(
    questionaire = "basis",
    form_id = "basis_form",
    form_name = "Basisformular",
    table_name = "basis_table",
    version = 2L
  )

  form <- form_from_metadata(
    inputs = inputs,
    settings = settings,
    questionnaire = "basis"
  )

  testthat::expect_s3_class(form, "sft_form")
  testthat::expect_equal(form$form_id, "basis_form")
  testthat::expect_equal(form$form_name, "Basisformular")
  testthat::expect_equal(form$table_name, "basis_table")
  testthat::expect_equal(form$version, 2L)
  testthat::expect_equal(field_ids(form), c("name", "topics"))
  testthat::expect_true(form$fields[[1L]]$mandatory)
  testthat::expect_true(form$fields[[1L]]$unique)
  testthat::expect_equal(form$fields[[2L]]$args$choices, c("A", "B"))
})

testthat::test_that("sft_form_from_metadata supports renamed_from", {
  inputs <- data.frame(
    Id = "first_name",
    label = "Vorname",
    inputObj = "textInput",
    renamed_from = "name"
  )

  form <- form_from_metadata(inputs = inputs, form_id = "rename_test")

  testthat::expect_equal(form$fields[[1L]]$id, "first_name")
  testthat::expect_equal(form$fields[[1L]]$db_column, "name")
})

testthat::test_that("sft_form_from_metadata rejects legacy R argument strings", {
  inputs <- data.frame(
    Id = "choice",
    label = "Auswahl",
    inputObj = "selectInput",
    args = "choices = c('A', 'B')"
  )

  testthat::expect_error(
    form_from_metadata(inputs = inputs, form_id = "bad_args"),
    "Legacy R argument strings are intentionally not evaluated"
  )
})
