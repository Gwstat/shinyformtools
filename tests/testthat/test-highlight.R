test_that("sft_norm_value canonicalises across input/stored types", {
  expect_identical(sft_norm_value(NULL), "")
  expect_identical(sft_norm_value(NA), "")
  expect_identical(sft_norm_value(character()), "")
  expect_identical(sft_norm_value("  hi  "), "hi")

  # numeric vs character forms collapse to the same string
  expect_identical(sft_norm_value(5), sft_norm_value("5"))
  expect_identical(sft_norm_value(5.0), sft_norm_value("5"))

  # logical vs 0/1 storage
  expect_identical(sft_norm_value(TRUE), sft_norm_value(1L))
  expect_identical(sft_norm_value(FALSE), sft_norm_value(0L))

  # dates round-trip against their ISO string
  expect_identical(sft_norm_value(as.Date("2020-01-02")), "2020-01-02")
})

test_that("sft_values_differ ignores type-only differences", {
  expect_false(sft_values_differ(5, "5"))
  expect_false(sft_values_differ(TRUE, 1L))
  expect_false(sft_values_differ(as.Date("2020-01-02"), "2020-01-02"))
  expect_false(sft_values_differ(NULL, ""))
  expect_false(sft_values_differ("  x ", "x"))

  expect_true(sft_values_differ("a", "b"))
  expect_true(sft_values_differ(5, 6))
  expect_true(sft_values_differ(TRUE, FALSE))
})

test_that("sft_resolve_highlight_fields handles NULL, vectors, and reactives", {
  expect_identical(sft_resolve_highlight_fields(NULL), character())
  expect_identical(sft_resolve_highlight_fields(c("a", "b")), c("a", "b"))
  expect_identical(
    sft_resolve_highlight_fields(function() c("x")),
    "x"
  )
  # empty result is preserved (means "clear")
  expect_identical(sft_resolve_highlight_fields(function() character()), character())
})

test_that("sft_highlight_container_ids namespaces both add and edit containers", {
  ns <- shiny::NS("mod")

  ids <- sft_highlight_container_ids(ns, c("age", "iban"), c("add_", "edit_"))

  expect_setequal(
    ids,
    c(
      "mod-sft_field_container_add_age",
      "mod-sft_field_container_add_iban",
      "mod-sft_field_container_edit_age",
      "mod-sft_field_container_edit_iban"
    )
  )

  expect_identical(
    sft_highlight_container_ids(ns, character(), c("add_", "edit_")),
    character()
  )

  # The result must be unnamed: a named vector serialises to a JSON object in
  # the custom message and the client handler iterates it as an array, so names
  # would silently disable the glow (regression guard).
  expect_null(names(ids))
  expect_null(names(sft_highlight_container_ids(ns, "age", c("add_", "edit_"))))
})

test_that("render_field wraps each field in a sft-field-container", {
  field <- form_field(id = "name", label = "Name")

  html <- as.character(render_field(field, prefix = "add_"))

  expect_true(grepl("sft-field-container", html, fixed = TRUE))
  expect_true(grepl("sft_field_container_add_name", html, fixed = TRUE))
})

test_that("highlight observers run without error in form_server", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")

  contacts <- form(
    form_id = "contacts",
    table_name = "contacts",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "age", label = "Age", input_type = "numericInput")
    )
  )

  highlight <- shiny::reactiveVal(c("name"))

  shiny::testServer(
    form_server,
    args = list(
      id = "contacts",
      form = contacts,
      highlight_fields = highlight,
      show_changed = TRUE
    ),
    {
      session$flushReact()

      # changing the highlight set re-runs the observer cleanly
      highlight(character())
      session$flushReact()

      expect_true(TRUE)
    }
  )
})
