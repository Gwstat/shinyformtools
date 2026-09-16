# Extracted from test-reconnect.R:150

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
sft_test_reconnect_form <- function(db_path) {
  form(
    form_id = "reconnect",
    table_name = "reconnect",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )
}

# test -------------------------------------------------------------------------
skip_if_not_installed("DT")
db_path <- tempfile(fileext = ".sqlite")
contacts <- sft_test_reconnect_form(db_path)
outer_conn <- db_connect(contacts$db)
on.exit(db_disconnect(outer_conn), add = TRUE)
init_db(contacts, conn = outer_conn, user = "alice")
insert_record(contacts, list(name = "Ada"), conn = outer_conn, user = "alice")
shiny::testServer(
    form_server,
    args = list(
      id = "reconnect",
      form = contacts,
      conn = outer_conn,
      user = function() "alice"
    ),
    {
      session$flushReact()

      # owns_connection is FALSE, so the guard never runs: conn stays the exact
      # object the caller passed in, alive or not.
      expect_false(owns_connection)
      expect_identical(state$handle, outer_conn)

      state$records()
      expect_identical(state$handle, outer_conn)
    }
  )
