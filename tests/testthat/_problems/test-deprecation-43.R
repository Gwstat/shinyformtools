# Extracted from test-deprecation.R:43

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
warnings <- capture_warnings(
    shiny::testServer(
      form_server,
      args = list(
        id = "deprecation",
        form = contacts,
        can_add = FALSE,
        table_filter = "top",
        table_columns = "name",
        highlight_color = "#123456"
      ),
      {
        expect_false(can_add)
        expect_identical(table_filter, "top")
        expect_identical(table_columns, "name")
        expect_identical(highlight_color, "#123456")
        # Untouched settings keep their defaults.
        expect_true(can_edit)
        expect_identical(default_column_view, "Standard")
      }
    )
  )
