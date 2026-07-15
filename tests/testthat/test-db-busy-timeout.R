# SQLite waits zero milliseconds for another writer's lock by default and raises
# "database is locked" straight away. db_connect() sets a busy timeout so a
# collision becomes a short wait instead of a failed write.

testthat::test_that("db_connect gives sqlite connections a busy timeout", {
  path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_sqlite(path))
  on.exit(db_disconnect(conn), add = TRUE)

  timeout <- DBI::dbGetQuery(conn, "PRAGMA busy_timeout")

  testthat::expect_equal(timeout[[1]][1], sft_sqlite_busy_timeout_ms)
})

testthat::test_that("a busy timeout makes a blocked write wait instead of failing at once", {
  # Proves the mechanism the default relies on, using a short timeout so the
  # suite does not sit through the real one. The blocking connection holds a
  # write lock, exactly as another worker mid-transaction would.
  path <- tempfile(fileext = ".sqlite")

  setup <- db_connect(db_sqlite(path))
  DBI::dbExecute(setup, "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)")
  db_disconnect(setup)

  blocker <- db_connect(db_sqlite(path))
  on.exit(db_disconnect(blocker), add = TRUE)
  DBI::dbExecute(blocker, "BEGIN IMMEDIATE")

  attempt <- function(timeout_ms) {
    conn <- db_connect(db_sqlite(path))
    on.exit(db_disconnect(conn), add = TRUE)
    DBI::dbExecute(conn, paste("PRAGMA busy_timeout =", timeout_ms))

    started <- proc.time()[["elapsed"]]
    err <- tryCatch(
      {
        DBI::dbExecute(conn, "INSERT INTO t (v) VALUES ('x')")
        NA_character_
      },
      error = function(e) conditionMessage(e)
    )

    list(seconds = proc.time()[["elapsed"]] - started, error = err)
  }

  impatient <- attempt(0)
  patient <- attempt(400)

  # Both still fail - the lock is never released here - but only one waited.
  testthat::expect_match(impatient$error, "database is locked")
  testthat::expect_match(patient$error, "database is locked")

  testthat::expect_lt(impatient$seconds, 0.2)
  testthat::expect_gt(patient$seconds, 0.3)
})
