# Extracted from test-reconnect.R:78

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
      # conn omitted: the module opens and OWNS its connection - the only case
      # the reconnect guard acts on.
      user = function() "alice"
    ),
    {
      session$flushReact()
      expect_true(DBI::dbIsValid(conn))

      # Simulate wait_timeout, then drive a real submit through the add flow.
      DBI::dbDisconnect(conn)
      session$setInputs(add_name = "Grace")
      session$setInputs(submit_add = 1)

      # The insert succeeded although the connection had been dropped, so
      # run_mutation() reconnected before writing.
      verify_conn <- db_connect(contacts$db)
      on.exit(db_disconnect(verify_conn), add = TRUE)
      stored <- fetch_records(contacts, conn = verify_conn)
      expect_true(all(c("Ada", "Grace") %in% stored$name))
    }
  )
