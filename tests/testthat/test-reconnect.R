# The module heals a self-opened connection that died mid-session (e.g. MariaDB
# wait_timeout on a long-idle Shiny session). Simulated on SQLite by closing the
# module's own connection from underneath it - the probe query then fails and
# sft_live_connection reopens from form$db.

sft_test_reconnect_form <- function(db_path) {
  test_form_name("reconnect", db = db_sqlite(db_path))
}

test_that("sft_live_connection returns the same connection while it is alive", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_reconnect_form(db_path)
  conn <- local_test_conn(contacts$db)

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
# read returns data) after the module's connection is dropped. The module
# keeps its handle in `state$handle` and every observer reads it through the
# `state$conn()` accessor, so the tests kill `state$handle` directly and then
# assert on observable outcomes: an insert or fetch on a dead connection
# throws, so data written / read proves the guard reconnected.

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
      expect_true(DBI::dbIsValid(state$handle))

      # Simulate wait_timeout, then drive a real submit through the add flow.
      DBI::dbDisconnect(state$handle)
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
      expect_true(DBI::dbIsValid(state$handle))

      # Drop the connection, then force the records reactive to re-run (a cached
      # reactive would not otherwise): the guard must reconnect for the fetch to
      # return data instead of throwing.
      DBI::dbDisconnect(state$handle)
      state$refresh()

      rows <- state$records()
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
      expect_false(state$owns_connection)
      expect_identical(state$handle, outer_conn)

      state$records()
      expect_identical(state$handle, outer_conn)
    }
  )
})

# The registrars (audit table, deleted records, versions, column views, conflict
# attribution, highlight) do not hold the connection handle: they receive the
# `live_conn()` accessor. Before that, `conn` reached them by value, and after
# the guard had replaced the module's connection they kept using the dead one.
#
# Outputs only re-run on a flush, so the test renders the deleted-records table
# once (forcing the registrar's first use of the connection), kills the
# connection, invalidates via refresh_tick and flushes. With the handle passed
# by value that render died with "Invalid or closed connection".
test_that("a registrar-driven read works after the module's connection was dropped", {
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
      invisible(output$deleted_records)

      DBI::dbDisconnect(state$handle)
      state$refresh()
      session$flushReact()

      expect_no_error(output$deleted_records)

      # The healed connection is the one the module now hands out.
      expect_true(DBI::dbIsValid(session$returned$connection()))
    }
  )
})
