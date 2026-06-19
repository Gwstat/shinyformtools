testthat::test_that("sft_field creates a valid field", {
  field <- form_field(
    id = "name",
    label = "Name",
    input_type = "textInput",
    mandatory = TRUE
  )

  testthat::expect_s3_class(field, "sft_field")
  testthat::expect_equal(field$id, "name")
  testthat::expect_equal(field$db_column, "name")
  testthat::expect_true(field$mandatory)
})

testthat::test_that("sft_field rejects reserved field ids", {
  testthat::expect_error(
    form_field(
      id = "sft_name",
      label = "Name"
    ),
    "reserved prefix"
  )
})

testthat::test_that("sft_form creates a valid form", {
  fields <- list(
    form_field(
      id = "name",
      label = "Name"
    ),
    form_field(
      id = "email",
      label = "E-Mail",
      unique = TRUE
    )
  )

  form <- form(
    form_id = "simple",
    table_name = "simple",
    fields = fields
  )

  testthat::expect_s3_class(form, "sft_form")
  testthat::expect_equal(field_ids(form), c("name", "email"))
  testthat::expect_equal(db_columns(form), c("name", "email"))
})

testthat::test_that("sft_form rejects duplicated field ids", {
  fields <- list(
    form_field(
      id = "name",
      label = "Name"
    ),
    form_field(
      id = "name",
      label = "Name again"
    )
  )

  testthat::expect_error(
    form(
      form_id = "simple",
      fields = fields
    ),
    "field ids must be unique"
  )
})

testthat::test_that("sft_form rejects duplicated database columns", {
  fields <- list(
    form_field(
      id = "name",
      label = "Name",
      db_column = "person"
    ),
    form_field(
      id = "email",
      label = "E-Mail",
      db_column = "person"
    )
  )

  testthat::expect_error(
    form(
      form_id = "simple",
      fields = fields
    ),
    "database columns must be unique"
  )
})

testthat::test_that("non-input fields are valid but not database columns", {
  fields <- list(
    form_field(
      id = "birth_year",
      label = "Geburtsjahr",
      input_type = "numericInput"
    ),
    html_field(
      id = "hint",
      html = "<strong>Hinweis</strong>"
    ),
    output_field(
      id = "age_preview",
      output_type = "text",
      label = "Alter"
    )
  )

  form <- form(
    form_id = "person",
    fields = fields
  )

  testthat::expect_equal(
    field_ids(form),
    c("birth_year", "hint", "age_preview")
  )

  testthat::expect_equal(
    db_columns(form),
    "birth_year"
  )
})

testthat::test_that("sft_form stores layout labels and messages", {
  fields <- list(
    form_field(
      id = "name",
      label = "Name",
      tab = 0L,
      tab_label = "Basis",
      slide = 0L,
      slide_label = "Person"
    ),
    form_field(
      id = "email",
      label = "E-Mail",
      tab = 1L,
      slide = 1L
    )
  )

  form <- form(
    form_id = "layout_test",
    fields = fields,
    tab_labels = c("Basisdaten", "Kontakt"),
    slide_labels = c("Start", "Weitere Angaben"),
    messages = list(
      unique = "Schon vergeben: {label}"
    )
  )

  testthat::expect_equal(form$fields[[1]]$tab_label, "Basis")
  testthat::expect_equal(form$fields[[1]]$slide_label, "Person")
  testthat::expect_equal(form$tab_labels[[2]], "Kontakt")
  testthat::expect_equal(
    sft_message(
      form = form,
      key = "unique",
      values = list(label = "E-Mail")
    ),
    "Schon vergeben: E-Mail"
  )
})


testthat::test_that("sft_form accepts static and dynamic header and footer", {
  fields <- list(
    form_field(
      id = "name",
      label = "Name"
    )
  )

  form <- form(
    form_id = "header_footer_test",
    fields = fields,
    header = "<strong>Header</strong>",
    footer = function(ns, prefix) {
      shiny::tags$small(paste0(prefix, "Footer"))
    }
  )

  testthat::expect_equal(form$header, "<strong>Header</strong>")
  testthat::expect_true(is.function(form$footer))
})

testthat::test_that("sft_field supports explicit field renaming by reusing the old database column", {
  field <- form_field(
    id = "first_name",
    label = "Vorname",
    renamed_from = "name"
  )

  testthat::expect_equal(field$id, "first_name")
  testthat::expect_equal(field$db_column, "name")
  testthat::expect_equal(field$renamed_from, "name")
})

testthat::test_that("sft_form rejects renaming from a field still present in the current schema", {
  testthat::expect_error(
    form(
      form_id = "rename_invalid",
      fields = list(
        form_field(id = "name", label = "Name"),
        form_field(id = "first_name", label = "Vorname", renamed_from = "name")
      )
    ),
    "renamed_from must refer to previous field ids"
  )
})

testthat::test_that("input registry validates supported input types", {
  testthat::expect_silent(
    form_field(
      id = "password",
      label = "Passwort",
      input_type = "passwordInput"
    )
  )

  testthat::expect_silent(
    form_field(
      id = "period",
      label = "Zeitraum",
      input_type = "dateRangeInput"
    )
  )

  testthat::expect_error(
    form_field(
      id = "bad",
      label = "Bad",
      input_type = "doesNotExist"
    ),
    "Unsupported input_type"
  )
})

testthat::test_that("input values are stored and restored consistently for vector-like inputs", {
  date_field <- form_field(
    id = "period",
    label = "Zeitraum",
    input_type = "dateRangeInput"
  )

  stored_dates <- sft_field_db_value(
    field = date_field,
    value = as.Date(c("2026-01-01", "2026-01-31"))
  )

  testthat::expect_equal(
    sft_ui_value(date_field, stored_dates),
    as.Date(c("2026-01-01", "2026-01-31"))
  )

  multi_field <- form_field(
    id = "topics",
    label = "Themen",
    input_type = "checkboxGroupInput",
    args = list(choices = c("A", "B", "C"))
  )

  stored_multi <- sft_field_db_value(
    field = multi_field,
    value = c("A", "C")
  )

  testthat::expect_equal(
    sft_ui_value(multi_field, stored_multi),
    c("A", "C")
  )
})

testthat::test_that("dateRangeInput uses start/end instead of unsupported value argument", {
  field <- form_field(
    id = "period",
    label = "Zeitraum",
    input_type = "dateRangeInput",
    args = list(
      start = as.Date("2026-01-01"),
      end = as.Date("2026-01-31")
    )
  )

  stored <- sft_field_db_value(
    field = field,
    value = as.Date(c("2026-06-01", "2026-06-05"))
  )

  args <- sft_prepare_input_args(field, value = stored)

  testthat::expect_false("value" %in% names(args))
  testthat::expect_equal(args$start, as.Date("2026-06-01"))
  testthat::expect_equal(args$end, as.Date("2026-06-05"))
})

testthat::test_that("vector-like input values are displayed as semicolon-separated text", {
  form <- form(
    form_id = "display_vectors",
    fields = list(
      form_field(
        id = "topics",
        label = "Themen",
        input_type = "checkboxGroupInput",
        args = list(choices = c("A", "B", "C"))
      ),
      form_field(
        id = "period",
        label = "Zeitraum",
        input_type = "dateRangeInput"
      )
    )
  )

  data <- data.frame(
    topics = sft_field_db_value(form$fields[[1L]], c("A", "C")),
    period = sft_field_db_value(form$fields[[2L]], as.Date(c("2026-01-01", "2026-01-31"))),
    stringsAsFactors = FALSE
  )

  out <- sft_format_field_display_columns(data, form = form)

  testthat::expect_equal(out$topics, "A; C")
  testthat::expect_equal(out$period, "2026-01-01 - 2026-01-31")
})
