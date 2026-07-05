# SQL-injection regression tests.
#
# The package's guarantee is twofold:
#   1. Identifiers (form_id, table_name, field id / db_column) are validated at
#      definition time against a strict allowlist (^[A-Za-z][A-Za-z0-9_]*$), so
#      they cannot contain quotes, semicolons, or whitespace.
#   2. Every value reaches the database through a parameterized query (? + params)
#      and every identifier through sft_quote_identifier(); values are never
#      interpolated into SQL.
#
# These tests lock in both layers so a future change that interpolated a value
# (or accepted an unsafe identifier) would fail here.

testthat::test_that("malicious field values are stored verbatim and never executed", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "vault",
    table_name = "vault",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "note", label = "Note")
    )
  )

  # Classic injection payloads targeting the real table name, plus embedded
  # quotes that would break any string-concatenated SQL.
  payloads <- c(
    "x'); DROP TABLE vault;--",
    "Robert'); DROP TABLE sft_audit_log;--",
    "O'Brien said \"hi\"; DELETE FROM vault; --",
    "1 OR 1=1"
  )

  for (payload in payloads) {
    insert_record(
      form = form,
      record = list(name = payload, note = payload),
      conn = conn,
      user = "tester"
    )
  }

  rows <- fetch_records(form, conn = conn)

  # Every row survived: the DROP/DELETE payloads were data, not SQL.
  testthat::expect_equal(nrow(rows), length(payloads))
  # Values round-trip byte-for-byte.
  testthat::expect_setequal(rows$name, payloads)
  testthat::expect_setequal(rows$note, payloads)

  # The targeted tables still exist and the audit log was not wiped.
  tables <- DBI::dbListTables(conn)
  testthat::expect_true("vault" %in% tables)
  testthat::expect_true("sft_audit_log" %in% tables)
  audit <- DBI::dbGetQuery(conn, "SELECT * FROM sft_audit_log")
  testthat::expect_equal(nrow(audit), length(payloads))
})

testthat::test_that("the parameterized unique check handles values with quotes", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "people",
    table_name = "people",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "handle", label = "Handle", unique = TRUE)
    )
  )

  tricky <- "o'brien'); --"

  insert_record(form, record = list(name = "A", handle = tricky),
                conn = conn, user = "tester")

  # A duplicate of the exact (quote-laden) value is rejected by the unique check,
  # proving the check compares the real value rather than a mangled fragment.
  testthat::expect_error(
    insert_record(form, record = list(name = "B", handle = tricky),
                  conn = conn, user = "tester")
  )

  # A different value is still accepted.
  insert_record(form, record = list(name = "C", handle = "o'brien"),
                conn = conn, user = "tester")

  rows <- fetch_records(form, conn = conn)
  testthat::expect_equal(nrow(rows), 2L)
  testthat::expect_setequal(rows$handle, c(tricky, "o'brien"))
})

testthat::test_that("unsafe identifiers are rejected at definition time", {
  # Field id / db_column must match the allowlist - no quotes, semicolons, spaces.
  testthat::expect_error(
    form_field(id = "name; DROP TABLE x"),
    "letters, numbers"
  )
  testthat::expect_error(
    form_field(id = "ok", db_column = "evil\"col"),
    "letters, numbers"
  )

  # Table name and form id are validated the same way when the form is built.
  testthat::expect_error(
    form(
      form_id = "ok",
      table_name = "users; DROP TABLE x",
      db_path = tempfile(fileext = ".sqlite"),
      fields = list(form_field(id = "name", label = "Name"))
    ),
    "letters, numbers"
  )
})
