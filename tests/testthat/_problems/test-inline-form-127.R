# Extracted from test-inline-form.R:127

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
    args = list(form = form, conn = conn, form_layout = "inline",
                permissions = list(can_add = FALSE)),
    {
      session$setInputs(open_add = 1)
      # Permission denied: the panel never opens.
      testthat::expect_null(inline_active())
    }
  )
