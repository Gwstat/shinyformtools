# Extracted from test-deprecation.R:117

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
shiny::testServer(
    form_server,
    args = list(
      id = "deprecation",
      form = contacts,
      permissions = list(user = "alice", can_delete = FALSE)
    ),
    {
      expect_identical(user, "alice")
      expect_false(can_delete)
    }
  )
