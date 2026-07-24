# Table-style presets: resolution, CSS scoping, and form_ui() wiring.

test_that("sft_resolve_table_style resolves argument, option and default", {
  # No argument, no option: classic.
  withr::with_options(list(shinyformtools.table_style = NULL), {
    expect_identical(sft_resolve_table_style(NULL), "classic")
  })

  # The global option fills in when the argument is NULL.
  withr::with_options(list(shinyformtools.table_style = "clean"), {
    expect_identical(sft_resolve_table_style(NULL), "clean")
    # An explicit argument overrides the option.
    expect_identical(sft_resolve_table_style("compact"), "compact")
  })

  expect_error(sft_resolve_table_style("fancy"), "table_style")
  withr::with_options(list(shinyformtools.table_style = "fancy"), {
    expect_error(sft_resolve_table_style(NULL), "table_style")
  })
})

test_that("classic emits no CSS; the other presets scope to the module tables", {
  ns <- shiny::NS("contacts")

  expect_null(sft_table_style_css(ns, "classic"))

  for (style in setdiff(sft_table_styles(), "classic")) {
    tag <- sft_table_style_css(ns, style)
    expect_s3_class(tag, "shiny.tag")
    expect_identical(tag$name, "style")

    css <- as.character(tag)
    # Every rule is anchored on a namespaced output id, so host-app tables
    # (and other form modules) are untouched.
    for (output_id in sft_table_style_output_ids()) {
      expect_true(grepl(paste0("#contacts-", output_id), css, fixed = TRUE))
    }
    expect_false(grepl("\ntable.dataTable", css, fixed = TRUE))
  }
})

test_that("form_ui includes the preset CSS only when a preset is active", {
  withr::with_options(list(shinyformtools.table_style = NULL), {
    default_html <- as.character(form_ui("contacts"))
    expect_false(grepl("sft table style", default_html, fixed = TRUE))

    clean_html <- as.character(form_ui("contacts", table_style = "clean"))
    expect_true(grepl("sft table style: clean", clean_html, fixed = TRUE))
    expect_true(grepl("#contacts-records", clean_html, fixed = TRUE))
  })

  # The global option styles a form that does not pass the argument, and a
  # per-form argument still wins.
  withr::with_options(list(shinyformtools.table_style = "publication"), {
    option_html <- as.character(form_ui("contacts"))
    expect_true(grepl("sft table style: publication", option_html, fixed = TRUE))

    override_html <- as.character(form_ui("contacts", table_style = "classic"))
    expect_false(grepl("sft table style", override_html, fixed = TRUE))
  })
})
