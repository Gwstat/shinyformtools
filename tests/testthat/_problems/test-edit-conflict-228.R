# Extracted from test-edit-conflict.R:228

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
sft_test_conflict_form <- function(db_path) {
  form(
    form_id = "contacts",
    table_name = "contacts",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "age", label = "Age", input_type = "numericInput")
    )
  )
}

# test -------------------------------------------------------------------------
skip_if_not_installed("DT")
db_path <- tempfile(fileext = ".sqlite")
contacts <- sft_test_conflict_form(db_path)
conn <- db_connect(db_path)
on.exit(db_disconnect(conn), add = TRUE)
init_db(contacts, conn = conn, user = "alice")
added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
record_id <- added$sft_id[1]
shiny::testServer(
    form_server,
    args = list(
      id = "contacts",
      form = contacts,
      conn = conn,
      user = function() "alice"
    ),
    {
      session$flushReact()

      row <- fetch_records(contacts, conn = conn)
      row <- row[row$sft_id == record_id, , drop = FALSE]
      current_edit_row(row)
      edit_conflict_baseline(row)

      # Another user saves while the dialog is open.
      update_record(contacts, list(name = "Bob"), record_id = record_id,
                    conn = conn, user = "bob")

      session$setInputs(edit_name = "Alice-Edit", edit_age = 30)
      session$setInputs(submit_edit = 1)

      # The save was rejected: conflict state set, nothing written.
      expect_false(is.null(edit_conflict()))
      expect_identical(edit_conflict()$columns, "name")

      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Bob")

      # The conflict view renders with the field, both values and the writer.
      html <- as.character(output$sft_edit_conflict_ui$html)
      expect_true(grepl("sft-edit-conflict", html, fixed = TRUE))
      expect_true(grepl("Ada", html, fixed = TRUE))
      expect_true(grepl("Bob", html, fixed = TRUE))
      expect_true(grepl("sft_conflict_accept", html, fixed = TRUE))
      # It also hides the regular form body while it is shown.
      expect_true(grepl("sft_edit_form_main", html, fixed = TRUE))
      expect_true(grepl("display: none", html, fixed = TRUE))

      # "Keep my entries": baseline moves to the current row, view closes,
      # and the next save deliberately writes the user's values.
      session$setInputs(sft_conflict_keep = 1)
      expect_null(edit_conflict())
      expect_identical(edit_conflict_baseline()$name[1], "Bob")

      session$setInputs(submit_edit = 2)
      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Alice-Edit")
    }
  )
