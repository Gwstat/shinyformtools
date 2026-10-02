tx_forms <- function() {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  list(
    db = db,
    contacts = form(form_id = "contacts", table_name = "contacts", db = db,
                    fields = list(form_field(id = "name", label = "Name", unique = TRUE))),
    reasons = form(form_id = "reasons", table_name = "reasons", db = db,
                   fields = list(form_field(id = "contact", label = "Contact"),
                                 form_field(id = "reason", label = "Reason")))
  )
}

test_that("writes inside with_transaction commit together", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)
  init_db(f$reasons, conn = conn)
  DBI::dbExecute(conn, "CREATE TABLE side (x TEXT)")

  out <- with_transaction(conn, {
    rec <- insert_record(f$contacts, list(name = "Ada"), conn = conn, user = "u")
    insert_record(f$reasons, list(contact = "Ada", reason = "call"), conn = conn)
    DBI::dbExecute(conn, "INSERT INTO side VALUES ('x')")
    rec$sft_id
  })

  expect_identical(out, 1L)
  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 1L)
  expect_identical(nrow(fetch_records(f$reasons, conn = conn)), 1L)
  expect_identical(nrow(DBI::dbGetQuery(conn, "SELECT * FROM side")), 1L)
  expect_identical(nrow(fetch_audit_log(f$contacts, conn = conn)), 1L)
})

test_that("an error anywhere in the block rolls everything back", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)
  init_db(f$reasons, conn = conn)
  DBI::dbExecute(conn, "CREATE TABLE side (x TEXT)")

  expect_error(with_transaction(conn, {
    insert_record(f$contacts, list(name = "Ada"), conn = conn)
    DBI::dbExecute(conn, "INSERT INTO side VALUES ('x')")
    insert_record(f$reasons, list(contact = "Ada", reason = "call"), conn = conn)
    stop("mail server down")
  }), "mail server down")

  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 0L)
  expect_identical(nrow(fetch_records(f$reasons, conn = conn)), 0L)
  expect_identical(nrow(DBI::dbGetQuery(conn, "SELECT * FROM side")), 0L)
  expect_identical(nrow(fetch_audit_log(f$contacts, conn = conn)), 0L)

  # A validation error inside rolls back what came before it, and keeps its class.
  insert_record(f$contacts, list(name = "Bob"), conn = conn)
  err <- tryCatch(with_transaction(conn, {
    insert_record(f$reasons, list(contact = "Bob", reason = "x"), conn = conn)
    insert_record(f$contacts, list(name = "Bob"), conn = conn)
  }), error = function(e) e)
  expect_s3_class(err, "sft_validation_error")
  expect_identical(nrow(fetch_records(f$reasons, conn = conn)), 0L)
})

test_that("a conflict retries the whole block", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)
  runs <- 0L

  with_transaction(conn, {
    runs <- runs + 1L
    insert_record(f$contacts, list(name = paste0("n", runs)), conn = conn)
    if (runs == 1L) stop("database is locked")
  })

  expect_identical(runs, 2L)
  expect_identical(fetch_records(f$contacts, conn = conn)$name, "n2")
})

test_that("with_transaction nests, and a stale schema is refused inside", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)

  with_transaction(conn, {
    with_transaction(conn, insert_record(f$contacts, list(name = "a"), conn = conn))
    insert_record(f$contacts, list(name = "b"), conn = conn)
  })
  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 2L)

  expect_error(
    with_transaction(conn, insert_record(f$reasons, list(reason = "x"), conn = conn)),
    "init_db\\(\\) for the form before the block"
  )
  # Outside the block the schema is created as usual.
  expect_no_error(insert_record(f$reasons, list(reason = "x"), conn = conn))
})

test_that("a nested write that fails and is caught still rolls the whole block back", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)
  init_db(f$reasons, conn = conn)

  # The inner block wrote one row before failing; catching its error must not
  # let the outer block commit that half.
  expect_error(
    with_transaction(conn, {
      insert_record(f$contacts, list(name = "Ada"), conn = conn)
      try(with_transaction(conn, {
        insert_record(f$reasons, list(contact = "Ada", reason = "call"), conn = conn)
        stop("inner failed")
      }), silent = TRUE)
      "done"
    }),
    "inner failed"
  )
  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 0L)
  expect_identical(nrow(fetch_records(f$reasons, conn = conn)), 0L)

  # A caught validation error of a package writer, same rule.
  expect_error(
    with_transaction(conn, {
      insert_record(f$contacts, list(name = "Bo"), conn = conn)
      tryCatch(insert_record(f$contacts, list(name = "Bo"), conn = conn), error = function(e) NULL)
    }),
    class = "sft_validation_error"
  )
  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 0L)

  # The next block on the connection starts clean.
  with_transaction(conn, insert_record(f$contacts, list(name = "Cy"), conn = conn))
  expect_identical(fetch_records(f$contacts, conn = conn)$name, "Cy")
})

warn_form <- function(db) {
  form(form_id = "warned", table_name = "warned", db = db,
       fields = list(form_field(id = "name", label = "Name")),
       validation_rules = list(warning_if(
         id = "careful", condition = function(values) TRUE,
         fields = "name", message = "Careful."
       )))
}

for (backend in c("sqlite", "duckdb")) {
  test_that(paste("a caught warning does not leave the transaction open,", backend), {
    if (backend == "duckdb") {
      skip_if_not_installed("duckdb")
      db <- db_duckdb(tempfile(fileext = ".duckdb"))
    } else {
      db <- db_sqlite(tempfile(fileext = ".sqlite"))
    }
    f <- warn_form(db)
    conn <- local_test_conn(db)
    init_db(f, conn = conn)

    caught <- tryCatch(insert_record(f, list(name = "first"), conn = conn),
                       warning = function(w) w)
    expect_s3_class(caught, "warning")
    expect_match(conditionMessage(caught), "Careful")

    expect_warning(insert_record(f, list(name = "second"), conn = conn), "Careful")
    expect_setequal(fetch_records(f, conn = conn)$name, c("first", "second"))
  })
}

test_that("a block left through a caught condition is rolled back", {
  f <- tx_forms()
  conn <- local_test_conn(f$db)
  init_db(f$contacts, conn = conn)

  tryCatch(with_transaction(conn, {
    insert_record(f$contacts, list(name = "Ada"), conn = conn)
    message("leaving")
    insert_record(f$contacts, list(name = "Grace"), conn = conn)
  }), message = function(m) NULL)

  expect_identical(nrow(fetch_records(f$contacts, conn = conn)), 0L)
  insert_record(f$contacts, list(name = "Linus"), conn = conn)
  expect_identical(fetch_records(f$contacts, conn = conn)$name, "Linus")
})
