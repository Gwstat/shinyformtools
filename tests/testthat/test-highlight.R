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

  # Unnamed so the ids paste cleanly into a CSS selector list.
  expect_null(names(ids))
  expect_null(names(sft_highlight_container_ids(ns, "age", c("add_", "edit_"))))
})

test_that("sft_field_tab_positions maps fields to their 1-based tab positions", {
  multi_tab <- form(
    form_id = "tabbed",
    table_name = "tabbed",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "a", label = "A", tab = 1L),
      form_field(id = "b", label = "B", tab = 2L),
      form_field(id = "c", label = "C", tab = 2L)
    )
  )

  expect_identical(sft_field_tab_positions(multi_tab, "a"), 1L)
  expect_identical(sft_field_tab_positions(multi_tab, "c"), 2L)
  expect_identical(sft_field_tab_positions(multi_tab, c("a", "c")), c(1L, 2L))
  expect_identical(sft_field_tab_positions(multi_tab, character()), integer())

  # A single-tab form renders no tabset, so there is nothing to glow.
  single_tab <- form(
    form_id = "flat",
    table_name = "flat",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(form_field(id = "a", label = "A"))
  )
  expect_identical(sft_field_tab_positions(single_tab, "a"), integer())
})

test_that("sft_highlight_style_block emits selectors per channel and clears when empty", {
  ns <- shiny::NS("mod")

  multi_tab <- form(
    form_id = "tabbed",
    table_name = "tabbed",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "a", label = "A", tab = 1L),
      form_field(id = "b", label = "B", tab = 2L)
    )
  )

  html <- as.character(
    sft_highlight_style_block(
      ns = ns,
      form = multi_tab,
      highlight_field_ids = "a",
      changed_field_ids = "b",
      highlight_color = "#ff0000",
      changed_color = "#0000ff"
    )
  )

  # Highlight channel targets both add and edit containers for field "a".
  expect_true(grepl("#mod-sft_field_container_add_a", html, fixed = TRUE))
  expect_true(grepl("#mod-sft_field_container_edit_a", html, fixed = TRUE))
  # Changed channel targets only the edit container for field "b".
  expect_true(grepl("#mod-sft_field_container_edit_b", html, fixed = TRUE))
  expect_false(grepl("#mod-sft_field_container_add_b", html, fixed = TRUE))
  # The glow targets the input control, not the container/label/row.
  expect_true(grepl("#mod-sft_field_container_add_a .form-control", html, fixed = TRUE))
  expect_true(grepl("#mod-sft_field_container_add_a .selectize-input", html, fixed = TRUE))
  # Owning tabs glow (a -> position 1, b -> position 2).
  expect_true(grepl(".nav-tabs > li:nth-child(1) > a", html, fixed = TRUE))
  expect_true(grepl(".nav-tabs > li:nth-child(2) > a", html, fixed = TRUE))
  # Server-controlled colours are written inline.
  expect_true(grepl("#ff0000", html, fixed = TRUE))
  expect_true(grepl("#0000ff", html, fixed = TRUE))

  # Nothing highlighted -> an empty stylesheet, which clears any prior glow.
  empty <- as.character(sft_highlight_style_block(ns = ns, form = multi_tab))
  expect_false(grepl("box-shadow", empty, fixed = TRUE))

  # highlight_tab = FALSE suppresses the tab rules but keeps the field glow.
  no_tabs <- as.character(
    sft_highlight_style_block(
      ns = ns, form = multi_tab,
      highlight_field_ids = "a", highlight_tab = FALSE
    )
  )
  expect_true(grepl("#mod-sft_field_container_add_a", no_tabs, fixed = TRUE))
  expect_false(grepl("nav-tabs", no_tabs, fixed = TRUE))
})

test_that("render_field wraps each field in a sft-field-container", {
  field <- form_field(id = "name", label = "Name")

  html <- as.character(render_field(field, prefix = "add_"))

  expect_true(grepl("sft-field-container", html, fixed = TRUE))
  expect_true(grepl("sft_field_container_add_name", html, fixed = TRUE))
})

test_that("sft_changed_since_creation_ids flags only fields edited since add", {
  db_path <- tempfile(fileext = ".sqlite")

  people <- form(
    form_id = "people",
    table_name = "people",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "city", label = "City"),
      form_field(id = "age", label = "Age", input_type = "numericInput")
    )
  )

  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(people, conn = conn, user = "t")

  rec <- insert_record(
    people, list(name = "Ada", city = "London", age = 36),
    conn = conn, user = "t"
  )
  rid <- rec$sft_id[1]

  # Before any edit, nothing has changed since creation.
  row0 <- fetch_records(people, conn = conn)
  row0 <- row0[row0$sft_id == rid, , drop = FALSE]
  expect_identical(sft_changed_since_creation_ids(conn, people, row0), character())

  # Edit one field; only that field is "changed since creation".
  update_record(
    people, list(name = "Ada", city = "Cambridge", age = 36),
    record_id = rid, conn = conn, user = "t"
  )
  row1 <- fetch_records(people, conn = conn)
  row1 <- row1[row1$sft_id == rid, , drop = FALSE]
  expect_identical(sft_changed_since_creation_ids(conn, people, row1), "city")

  # A NULL row (no edit dialog open) or NULL connection yields nothing.
  expect_identical(sft_changed_since_creation_ids(conn, people, NULL), character())
  expect_identical(sft_changed_since_creation_ids(NULL, people, row1), character())
})

test_that("form_server renders a reactive highlight stylesheet", {
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

      style <- as.character(output$sft_highlight_style$html)
      expect_true(grepl("sft_field_container_add_name", style, fixed = TRUE))
      expect_true(grepl("sft_field_container_edit_name", style, fixed = TRUE))

      # clearing the highlight set empties the stylesheet
      highlight(character())
      session$flushReact()

      cleared <- as.character(output$sft_highlight_style$html)
      expect_false(grepl("box-shadow", cleared, fixed = TRUE))
    }
  )
})
