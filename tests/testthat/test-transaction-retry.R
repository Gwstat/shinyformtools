testthat::test_that("sft_is_retryable_conflict recognises backend constraint messages", {
  retryable <- c(
    "UNIQUE constraint failed: sft_audit_log.version_no",
    "PRIMARY KEY must be unique",
    "Duplicate entry '5' for key 'PRIMARY'",
    "Constraint Error: Duplicate key \"sft_id: 5\" violates primary key constraint",
    "violates unique constraint",
    # InnoDB lock errors say "try restarting transaction" - so we do.
    "Deadlock found when trying to get lock; try restarting transaction [1213]",
    "Lock wait timeout exceeded; try restarting transaction [1205]",
    # Observed live via RMariaDB when a write loses a row-lock race.
    "Record has changed since last read in table 'contacts' [1020]"
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
  conn <- local_test_conn(db_path)

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
  conn <- local_test_conn(db_path)

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
  conn <- local_test_conn(db_path)

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

# Colliding writers used to retry at once, in lockstep, and collide again: under
# contention that exhausted the five attempts (tools/load-test/write_contention.R).
# A retry now waits a random, growing moment first.
testthat::test_that("a retry backs off first, a clean run and a hard error never wait", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  waited <- integer()
  testthat::local_mocked_bindings(
    sft_retry_wait = function(attempt) {
      waited <<- c(waited, attempt)
      invisible(NULL)
    }
  )

  attempts <- 0L
  result <- sft_db_with_transaction(conn, {
    attempts <- attempts + 1L
    if (attempts < 3L) stop("UNIQUE constraint failed: t.id")
    "ok"
  })
  testthat::expect_equal(result, "ok")
  # One wait before each of the two retries, numbered by the attempt that failed.
  testthat::expect_identical(waited, 1:2)

  waited <- integer()
  testthat::expect_equal(sft_db_with_transaction(conn, "clean"), "clean")
  testthat::expect_error(sft_db_with_transaction(conn, stop("some other failure")), "some other failure")
  testthat::expect_length(waited, 0L)

  # The last failed attempt is not followed by a wait: there is no retry left.
  waited <- integer()
  testthat::expect_error(
    sft_db_with_transaction(conn, stop("UNIQUE constraint failed: t.id"), max_attempts = 3L),
    "UNIQUE constraint failed"
  )
  testthat::expect_identical(waited, 1:2)
})

testthat::test_that("the backoff ceiling doubles per attempt and stays small", {
  ceilings <- vapply(1:4, function(attempt) {
    max(replicate(200, {
      t0 <- Sys.time()
      sft_retry_wait(attempt)
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    })[1:3])
  }, numeric(1))

  # Generous upper bounds (scheduler noise): 10, 20, 40, 80 ms ceilings.
  testthat::expect_true(all(ceilings < c(0.05, 0.06, 0.08, 0.12)))
})
