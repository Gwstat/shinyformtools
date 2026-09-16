# Extracted from test-deprecation.R:80

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
sft_reset_deprecation_warnings()
withr::defer(sft_reset_deprecation_warnings())
contacts <- sft_test_deprecation_form()
suppressWarnings(
    shiny::testServer(
      form_server,
      args = list(
        id = "deprecation",
        form = contacts,
        permissions = list(can_add = TRUE),
        can_add = FALSE
      ),
      expect_true(can_add)
    )
  )
