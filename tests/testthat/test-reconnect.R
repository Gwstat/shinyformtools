# The module heals a self-opened connection that died mid-session (e.g. MariaDB
# wait_timeout on a long-idle Shiny session). Simulated on SQLite by closing the
# module's own connection from underneath it - the probe query then fails and
# sft_live_connection reopens from form$db.

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

test_that("sft_live_connection returns the same connection while it is alive", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_reconnect_form(db_path)
  conn <- db_connect(contacts$db)
  on.exit(db_disconnect(conn), add = TRUE)

  # A live connection is returned unchanged (same external pointer).
  same <- sft_live_connection(conn, contacts$db)
  expect_identical(same, conn)
})

test_that("sft_live_connection reopens a dead connection from the config", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_reconnect_form(db_path)

  init_db(contacts, user = "alice")

  conn <- db_connect(contacts$db)
  DBI::dbDisconnect(conn)
  expect_false(DBI::dbIsValid(conn))

  fresh <- sft_live_connection(conn, contacts$db)
  on.exit(db_disconnect(fresh), add = TRUE)

  expect_true(DBI::dbIsValid(fresh))
  # The fresh connection reaches the same database (the schema is there).
  expect_true("sft_forms" %in% DBI::dbListTables(fresh))
})

# These two integration tests assert on OBSERVABLE outcomes (a write lands, a
# read returns data) after the module's connection is dropped, not on the
# module's private `conn` object. Under testServer the guard's `conn <<-`
# reassignment updates the module frame, but the test expression resolves `conn`
# through a scope that does not reliably reflect it - so inspecting `conn`
# directly is not a valid probe. The outcomes below can only hold if the guard
# reconnected: an insert or fetch on a dead connection throws. Each test kills
# the connection before any reconnect has happened, so at that point the test's
# `conn` and the module's sole connection are the same object and the kill bites.

test_that("the write path reconnects a module-owned connection the server dropped", {
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
})

test_that("the read path reconnects a module-owned connection the server dropped", {
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
})

test_that("a caller-supplied connection is never silently replaced", {
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
      expect_identical(conn, outer_conn)

      records()
      expect_identical(conn, outer_conn)
    }
  )
})
