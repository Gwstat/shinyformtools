# Shared fixtures for the database tests. testthat sources helper*.R before the
# suite runs, so these are available to every test file. They factor out the
# mechanical setup only; tests still own any form whose definition is the
# subject of the test.

# A SQLite connection for one test, disconnected automatically when the calling
# test_that() block finishes (replacing the manual on.exit(db_disconnect())).
# `path` defaults to a fresh tempfile; pass the test's own db_path when the form
# is built with `db_path = db_path` so the form and connection share a file.
# withr is an imported dependency of testthat, so it is always available here.
local_test_conn <- function(path = tempfile(fileext = ".sqlite"),
                            .local_envir = parent.frame()) {
  conn <- db_connect(path)
  withr::defer(db_disconnect(conn), envir = .local_envir)
  conn
}

# The canonical minimal form used across the CRUD/schema/restore tests: a
# mandatory name plus a unique email. The db config is unused when CRUD calls
# pass `conn` explicitly (as the tests do); it only has to be valid.
test_form_basic <- function(form_id = "simple",
                            table_name = form_id,
                            db = db_sqlite(tempfile(fileext = ".sqlite"))) {
  form(
    form_id = form_id,
    table_name = table_name,
    db = db,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )
}

# The audit-log rows for a connection, oldest first.
test_audit_log <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM sft_audit_log ORDER BY log_id")
}

# Assert the number of records a form has (optionally including soft-deleted).
expect_record_count <- function(form, conn, n, include_deleted = FALSE) {
  records <- fetch_records(form, conn = conn, include_deleted = include_deleted)
  testthat::expect_equal(nrow(records), as.integer(n))
}
