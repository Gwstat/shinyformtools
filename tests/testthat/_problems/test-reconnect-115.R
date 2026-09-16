# Extracted from test-reconnect.R:115

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
seed_conn <- db_connect(contacts$db)
init_db(contacts, conn = seed_conn, user = "alice")
insert_record(contacts, list(name = "Ada"), conn = seed_conn, user = "alice")
db_disconnect(seed_conn)
shiny::testServer(
    form_server,
    args = list(
      id = "reconnect",
      form = contacts,
      user = function() "alice"
    ),
    {
      session$flushReact()
      expect_true(DBI::dbIsValid(conn))

      # Drop the connection, then force the records reactive to re-run (a cached
      # reactive would not otherwise): the guard must reconnect for the fetch to
      # return data instead of throwing.
      DBI::dbDisconnect(conn)
      refresh_tick(refresh_tick() + 1L)

      rows <- records()
      expect_true("Ada" %in% rows$name)
    }
  )
