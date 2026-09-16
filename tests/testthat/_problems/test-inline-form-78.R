# Extracted from test-inline-form.R:78

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
