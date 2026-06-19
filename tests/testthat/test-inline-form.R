sft_inline_test_form <- function(db_path) {
  form(
    form_id = "inline_form",
    table_name = "inline_form",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "note", label = "Note")
    )
  )
}

testthat::test_that("form_ui adds the inline slot only when form_layout = inline", {
  inline_html <- as.character(form_ui("t", form_layout = "inline"))
  modal_html <- as.character(form_ui("t"))

  testthat::expect_true(grepl("sft_inline_form", inline_html, fixed = TRUE))
  testthat::expect_false(grepl("sft_inline_form", modal_html, fixed = TRUE))
})

testthat::test_that("form_ui/form_server reject an unknown form_layout", {
  testthat::expect_error(form_ui("t", form_layout = "sidebar"))
})

testthat::test_that("inline add opens, cancels, and submits into the table", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- sft_inline_test_form(db_path)
  init_db(form, conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn, form_layout = "inline"),
    {
      # Nothing open initially.
      testthat::expect_null(inline_active())

      # Open the add form inline: the panel renders the add_ inputs.
      session$setInputs(open_add = 1)
      testthat::expect_identical(inline_active(), "add")
      add_html <- paste(as.character(output$sft_inline_form), collapse = " ")
      testthat::expect_true(grepl("add_name", add_html, fixed = TRUE))

      # Cancel closes the panel.
      session$setInputs(sft_inline_cancel = 1)
      testthat::expect_null(inline_active())

      # Re-open, fill the form and submit: record inserted and panel closes.
      session$setInputs(open_add = 1)
      session$setInputs(add_name = "Inline Ada", add_note = "via panel")
      session$setInputs(submit_add = 1)
      testthat::expect_null(inline_active())
    }
  )

  rows <- fetch_records(form, conn = conn)
  testthat::expect_true("Inline Ada" %in% rows$name)
})

testthat::test_that("inline edit opens for a selected record and saves", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- sft_inline_test_form(db_path)
  init_db(form, conn = conn)
  rec <- insert_record(form, list(name = "Ada", note = "orig"), conn = conn)
  rid <- rec$sft_id[1]

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn, form_layout = "inline"),
    {
      session$setInputs(records_rows_selected = 1L)
      session$setInputs(open_edit = 1)
      testthat::expect_identical(inline_active(), "edit")

      html <- paste(as.character(output$sft_inline_form), collapse = " ")
      testthat::expect_true(grepl("edit_name", html, fixed = TRUE))

      session$setInputs(edit_name = "Ada Edited", edit_note = "changed")
      session$setInputs(submit_edit = 1)
      testthat::expect_null(inline_active())
    }
  )

  after <- fetch_records(form, conn = conn)
  testthat::expect_equal(after$name[after$sft_id == rid], "Ada Edited")
})

testthat::test_that("modal layout never populates the inline panel", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- sft_inline_test_form(db_path)
  init_db(form, conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn),
    {
      session$setInputs(open_add = 1)
      # Modal layout shows a dialog instead; inline state stays empty.
      testthat::expect_null(inline_active())
    }
  )
})

testthat::test_that("inline add respects can_add = FALSE", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- sft_inline_test_form(db_path)
  init_db(form, conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = form, conn = conn, form_layout = "inline", can_add = FALSE),
    {
      session$setInputs(open_add = 1)
      # Permission denied: the panel never opens.
      testthat::expect_null(inline_active())
    }
  )
})
