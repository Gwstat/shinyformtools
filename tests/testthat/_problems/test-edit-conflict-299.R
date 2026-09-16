# Extracted from test-edit-conflict.R:299

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
      user = function() "alice",
      conflict_check = FALSE
    ),
    {
      session$flushReact()

      row <- fetch_records(contacts, conn = conn)
      row <- row[row$sft_id == record_id, , drop = FALSE]
      current_edit_row(row)
      edit_conflict_baseline(row)

      update_record(contacts, list(name = "Bob"), record_id = record_id,
                    conn = conn, user = "bob")

      session$setInputs(edit_name = "Alice-Edit", edit_age = 30)
      session$setInputs(submit_edit = 1)

      # No conflict view: the stale edit overwrites (previous behaviour).
      expect_null(edit_conflict())
      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Alice-Edit")
    }
  )
