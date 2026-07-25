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

# Connection details for the live MariaDB tests, or NULL when none are
# configured. Opt-in on purpose: the suite has to stay green on a machine with
# no server, and CRAN has none.
#
#   SFT_TEST_MARIADB_HOST      enables the tests (nothing else does)
#   SFT_TEST_MARIADB_PORT      defaults to 3306
#   SFT_TEST_MARIADB_USER      falls back to SFT_MARIADB_USER
#   SFT_TEST_MARIADB_PASSWORD  falls back to SFT_MARIADB_PASSWORD
#
# The account needs CREATE DATABASE, because every test builds its own throwaway
# database and drops it again - no test ever opens a database it did not create.
sft_test_mariadb_config <- function() {
  host <- Sys.getenv("SFT_TEST_MARIADB_HOST", unset = "")

  if (!nzchar(host)) {
    return(NULL)
  }

  fallback <- function(name) {
    value <- Sys.getenv(paste0("SFT_TEST_", name), unset = "")
    if (nzchar(value)) value else Sys.getenv(paste0("SFT_", name), unset = "")
  }

  list(
    host = host,
    port = as.integer(Sys.getenv("SFT_TEST_MARIADB_PORT", unset = "3306")),
    user = fallback("MARIADB_USER"),
    password = fallback("MARIADB_PASSWORD")
  )
}

# Open an admin connection to the configured server, or return the error.
sft_test_mariadb_connect <- function(config = sft_test_mariadb_config(),
                                     dbname = NULL) {
  tryCatch(
    DBI::dbConnect(
      RMariaDB::MariaDB(),
      dbname = dbname,
      host = config$host,
      port = config$port,
      username = config$user,
      password = config$password
    ),
    error = function(e) e
  )
}

# Skip unless a live server is BOTH configured and currently answering.
#
# The second half matters as much as the first: the test server is typically a
# container that is not permanently running, so a configured host that happens
# to be down has to skip like an unconfigured one. Checking only the environment
# variable would turn "my container is stopped" into a suite full of red
# connection errors.
skip_if_no_mariadb <- function() {
  testthat::skip_if_not_installed("RMariaDB")

  config <- sft_test_mariadb_config()

  if (is.null(config)) {
    testthat::skip("no live MariaDB configured (set SFT_TEST_MARIADB_HOST)")
  }

  conn <- sft_test_mariadb_connect(config)

  if (inherits(conn, "error")) {
    testthat::skip(paste0(
      "live MariaDB at ", config$host, ":", config$port,
      " is not reachable: ", conditionMessage(conn)
    ))
  }

  DBI::dbDisconnect(conn)

  invisible(TRUE)
}

# A throwaway database on the configured server, dropped when the calling
# test_that() block finishes. The name is unique per call so a crashed run never
# poisons the next one.
local_test_mariadb <- function(.local_envir = parent.frame()) {
  config <- sft_test_mariadb_config()

  dbname <- paste0(
    "zz_sft_test_",
    as.integer(Sys.time()),
    "_",
    sample.int(99999L, 1L)
  )

  admin <- sft_test_mariadb_connect(config)

  # The server can go away between skip_if_no_mariadb() and here, and the
  # account may lack CREATE DATABASE. Neither is a package failure.
  if (inherits(admin, "error")) {
    testthat::skip(paste("live MariaDB became unreachable:", conditionMessage(admin)))
  }

  created <- tryCatch(
    DBI::dbExecute(admin, paste0("CREATE DATABASE ", DBI::dbQuoteIdentifier(admin, dbname))),
    error = function(e) e
  )

  if (inherits(created, "error")) {
    DBI::dbDisconnect(admin)
    testthat::skip(paste(
      "cannot create a throwaway database (the test account needs CREATE",
      "DATABASE):", conditionMessage(created)
    ))
  }

  withr::defer(
    {
      DBI::dbExecute(
        admin,
        paste0("DROP DATABASE IF EXISTS ", DBI::dbQuoteIdentifier(admin, dbname))
      )
      DBI::dbDisconnect(admin)
    },
    envir = .local_envir
  )

  db_mariadb(
    dbname = dbname,
    host = config$host,
    port = config$port,
    user = config$user,
    password = config$password
  )
}

# The declared type of one column, as the server reports it.
test_column_type <- function(conn, table_name, column) {
  DBI::dbGetQuery(
    conn,
    "
    SELECT DATA_TYPE
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = ? AND COLUMN_NAME = ?
    ",
    params = list(table_name, column)
  )$DATA_TYPE
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
