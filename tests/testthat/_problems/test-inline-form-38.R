# Extracted from test-inline-form.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
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
