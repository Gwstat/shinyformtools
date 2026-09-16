# Extracted from test-deprecation.R:148

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
sft_test_deprecation_form <- function() {
  form(
    form_id = "deprecation",
    table_name = "deprecation",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(form_field(id = "name", label = "Name"))
  )
}

# test -------------------------------------------------------------------------
skip_if_not_installed("DT")
contacts <- sft_test_deprecation_form()
html <- as.character(form_ui("deprecation", form_layout = "inline"))
expect_match(html, "deprecation-sft_form_layout")
expect_match(html, 'value="inline"')
shiny::testServer(
    form_server,
    args = list(id = "deprecation", form = contacts),
    {
      session$flushReact()
      session$setInputs(sft_form_layout = "inline")
      session$setInputs(open_add = 1L)
      expect_identical(inline_active(), "add")
    }
  )
