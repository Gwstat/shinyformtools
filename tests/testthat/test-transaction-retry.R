testthat::test_that("sft_is_retryable_conflict recognises backend constraint messages", {
  retryable <- c(
    "UNIQUE constraint failed: sft_audit_log.version_no",
    "PRIMARY KEY must be unique",
    "Duplicate entry '5' for key 'PRIMARY'",
    "Constraint Error: Duplicate key \"sft_id: 5\" violates primary key constraint",
    "violates unique constraint"
  )

  for (msg in retryable) {
    testthat::expect_true(
      sft_is_retryable_conflict(simpleError(msg)),
      info = msg
    )
  }

  not_retryable <- c(
    "NOT NULL constraint failed: t.name",
    "no such table: t",
    "forced audit failure"
  )

  for (msg in not_retryable) {
    testthat::expect_false(
      sft_is_retryable_conflict(simpleError(msg)),
      info = msg
    )
  }
})

testthat::test_that("sft_db_with_transaction retries a racing-writer conflict and both succeed", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  attempts <- 0L

  result <- sft_db_with_transaction(conn, {
    attempts <- attempts + 1L
    if (attempts < 3L) {
      stop("UNIQUE constraint failed: t.id")
    }
    "ok"
  })

  # The body is re-evaluated on each retry, so the conflict clears and the
  # writer ultimately succeeds rather than erroring out.
  testthat::expect_equal(result, "ok")
  testthat::expect_equal(attempts, 3L)
})

testthat::test_that("sft_db_with_transaction surfaces the conflict after exhausting retries", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  attempts <- 0L

  testthat::expect_error(
    sft_db_with_transaction(
      conn,
      {
        attempts <- attempts + 1L
        stop("UNIQUE constraint failed: t.id")
      },
      max_attempts = 3L
    ),
    "UNIQUE constraint failed"
  )

  testthat::expect_equal(attempts, 3L)
})

testthat::test_that("sft_db_with_transaction does not retry a non-conflict error", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  attempts <- 0L

  testthat::expect_error(
    sft_db_with_transaction(conn, {
      attempts <- attempts + 1L
      stop("some other failure")
    }),
    "some other failure"
  )

  # A non-retryable error surfaces on the first attempt.
  testthat::expect_equal(attempts, 1L)
})
