# Compatibility of the MariaDB/MySQL path, both directions: older servers must
# be able to create the schema at all, and databases written by an older version
# of the package must keep working. Every expectation here was first observed
# live against MariaDB 11.8.6, MariaDB 10.3.39 and MySQL 8.4.10; sft_db_backend()
# dispatches on class names only, so a bare object with the right class
# exercises the SQL helpers without a server.

fake_mariadb_conn <- function() structure(list(), class = "MariaDBConnection")

testthat::test_that("unique TEXT support is read from the server version", {
  # MariaDB >= 10.4 indexes an unbounded TEXT column transparently (UNIQUE HASH).
  testthat::expect_true(sft_mariadb_supports_unique_text("10.4.0-MariaDB"))
  testthat::expect_true(sft_mariadb_supports_unique_text("11.8.6-MariaDB-ubu2404"))

  # Older MariaDB cannot: verified live on 10.3.39, error 1170.
  testthat::expect_false(
    sft_mariadb_supports_unique_text("10.3.39-MariaDB-1:10.3.39+maria~ubu2004")
  )
  testthat::expect_false(sft_mariadb_supports_unique_text("5.5.68-MariaDB"))

  # Real MySQL never can, at any version - verified live on 8.4.10.
  testthat::expect_false(sft_mariadb_supports_unique_text("8.4.10"))
  testthat::expect_false(sft_mariadb_supports_unique_text("8.0.36-0ubuntu0.22.04.1"))

  # Some MariaDB builds prefix the version so ancient clients keep connecting.
  testthat::expect_true(sft_mariadb_supports_unique_text("5.5.5-10.11.2-MariaDB"))
  testthat::expect_false(sft_mariadb_supports_unique_text("5.5.5-10.3.39-MariaDB"))

  # An unreadable version falls back to the answer that works everywhere.
  testthat::expect_false(sft_mariadb_supports_unique_text(""))
  testthat::expect_false(sft_mariadb_supports_unique_text(NULL))

  # Backends that are not MySQL-protocol never consult a version at all.
  conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
  on.exit(db_disconnect(conn), add = TRUE)
  testthat::expect_true(sft_supports_unique_text_index(conn))
})

testthat::test_that("a unique TEXT column becomes VARCHAR(255) only where it must", {
  unique_field <- form_field(id = "email", label = "Email", unique = TRUE)
  plain_field <- form_field(id = "note", label = "Note")
  conn <- fake_mariadb_conn()

  # The server cannot index TEXT: the column has to be indexable instead.
  testthat::expect_identical(
    sft_field_db_definition(unique_field, conn = conn, unique_text_ok = FALSE),
    "VARCHAR(255)"
  )

  # The server can: nothing changes, so no existing database is disturbed.
  testthat::expect_identical(
    sft_field_db_definition(unique_field, conn = conn, unique_text_ok = TRUE),
    "TEXT"
  )

  # Non-unique fields are never rewritten - they carry no index.
  testthat::expect_identical(
    sft_field_db_definition(plain_field, conn = conn, unique_text_ok = FALSE),
    "TEXT"
  )

  # Non-text types are left alone even when unique.
  numeric_unique <- form_field(
    id = "code", label = "Code", input_type = "numericInput", unique = TRUE
  )
  testthat::expect_identical(
    sft_field_db_definition(numeric_unique, conn = conn, unique_text_ok = FALSE),
    "REAL"
  )
})

testthat::test_that("the schema signature is unchanged by the VARCHAR mapping", {
  # This is the load-bearing part: the signature is computed with conn = NULL, so
  # it must stay backend-neutral. If the mapping leaked into it, every stored
  # schema_hash would change and every existing database would reconcile.
  contacts <- form(
    form_id = "sig_contacts", table_name = "sig_contacts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "email", label = "Email", unique = TRUE),
      form_field(id = "name", label = "Name")
    )
  )

  signature <- sft_schema_signature(contacts)
  testthat::expect_true(grepl("\"email\":\\s*\\[?\"TEXT\"", signature))
  testthat::expect_false(grepl("VARCHAR", signature))

  columns <- sft_expected_columns(contacts, conn = NULL)
  testthat::expect_identical(unname(columns[["email"]]), "TEXT")
})

testthat::test_that("CREATE TABLE pins utf8mb4 on MariaDB and nowhere else", {
  # MariaDB <= 10.5 defaults to latin1, which silently narrows what a column can
  # hold: verified live on 10.3, where CJK and emoji inserts were rejected with
  # error 1366 until the charset was pinned.
  testthat::expect_identical(
    sft_create_table_suffix(fake_mariadb_conn()),
    " DEFAULT CHARSET=utf8mb4"
  )

  conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
  on.exit(db_disconnect(conn), add = TRUE)

  testthat::expect_identical(sft_create_table_suffix(conn), "")

  # SQLite DDL stays byte-for-byte what it was.
  sql <- sft_create_table_sql(conn, "demo", c(sft_id = "INTEGER PRIMARY KEY"))
  testthat::expect_false(grepl("CHARSET", sql))
  testthat::expect_true(endsWith(sql, ")"))
})

testthat::test_that("a failed unique index does not misreport the cause", {
  index <- list(name = "uq_demo__email", columns = c("email", "sft_unique_slot"))
  demo_form <- list(table_name = "demo")

  key_length <- sft_unique_index_error_message(
    index, demo_form,
    "BLOB/TEXT column 'email' used in key specification without a key length [1170]"
  )
  testthat::expect_match(key_length, "cannot put a unique index on an unbounded TEXT")
  testthat::expect_false(grepl("duplicate values", key_length))

  duplicates <- sft_unique_index_error_message(
    index, demo_form, "Duplicate entry 'a@b.de' for key 'uq_demo__email' [1062]"
  )
  testthat::expect_match(duplicates, "duplicate values")

  # The original server error survives either way.
  testthat::expect_match(key_length, "1170")
  testthat::expect_match(duplicates, "1062")
})

testthat::test_that("widening long-text columns is a no-op off MariaDB", {
  # The payload columns are only capped on MariaDB; other backends must not be
  # touched, and the helper must tolerate a database with no system tables yet.
  conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
  on.exit(db_disconnect(conn), add = TRUE)

  testthat::expect_false(sft_widen_long_text_columns(conn))

  init_system_tables(conn)
  testthat::expect_false(sft_widen_long_text_columns(conn))
  testthat::expect_true(all(
    sft_system_table_names() %in% DBI::dbListTables(conn)
  ))
})

testthat::test_that("a conflict is recognised even when the server speaks German", {
  # Matching the English wording alone silently disabled the whole retry
  # machinery on a localized server. These are REAL messages captured from
  # MariaDB 11.8 with lc_messages = 'de_DE'.
  german <- c(
    "Doppelter Eintrag '1' für Schlüssel 'PRIMARY' [1062]",
    paste0(
      "Beim Warten auf eine Sperre wurde die zulässige Wartezeit ",
      "überschritten. Bitte versuchen Sie, die Transaktion neu zu ",
      "starten [1205]"
    )
  )

  for (msg in german) {
    testthat::expect_true(sft_is_retryable_conflict(simpleError(msg)))
  }

  # The English wording still works, so nothing regressed.
  testthat::expect_true(sft_is_retryable_conflict(
    simpleError("Duplicate entry '1' for key 'PRIMARY' [1062]")
  ))

  # And errors that are NOT conflicts must stay non-retryable in any language -
  # otherwise a typo in a query would be retried five times before failing.
  not_conflicts <- c(
    "Tabelle 'db.nope' existiert nicht [1146]",
    "Unbekanntes Tabellenfeld 'nope' in SELECT [1054]",
    "Table 'db.nope' doesn't exist [1146]",
    "Data too long for column 'config_json' at row 1 [1406]"
  )

  for (msg in not_conflicts) {
    testthat::expect_false(sft_is_retryable_conflict(simpleError(msg)))
  }
})

testthat::test_that("table lookup is exact unless the server folds identifiers", {
  conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
  on.exit(db_disconnect(conn), add = TRUE)

  DBI::dbExecute(conn, "CREATE TABLE MyStaff (a TEXT)")

  # SQLite is not asked about folding at all, and stores the name as written.
  testthat::expect_false(sft_folds_table_names(conn))
  testthat::expect_true(sft_table_exists(conn, "MyStaff"))
  testthat::expect_false(sft_table_exists(conn, "nowhere"))

  # Case-insensitive matching must NOT leak into a case-sensitive backend:
  # there, `mystaff` would be a different table.
  testthat::expect_false(sft_table_exists(conn, "mystaff"))

  testthat::expect_true(sft_tables_present(conn, c("MyStaff"), c("MyStaff", "x")))
  testthat::expect_false(sft_tables_present(conn, c("MyStaff", "gone"), c("MyStaff")))
})

testthat::test_that("dropping an index does not rely on IF EXISTS", {
  # MySQL rejects `DROP INDEX ... IF EXISTS` with a syntax error (1064, verified
  # live on 8.4.10), so the MariaDB branch must guard by lookup instead. On
  # SQLite the clause is valid and stays in use.
  conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
  on.exit(db_disconnect(conn), add = TRUE)

  DBI::dbExecute(conn, "CREATE TABLE demo (a TEXT, b TEXT)")
  DBI::dbExecute(conn, "CREATE INDEX demo_idx ON demo (a)")

  testthat::expect_true("demo_idx" %in% sft_list_index_names(conn, "demo"))
  sft_drop_index(conn, "demo", "demo_idx")
  testthat::expect_false("demo_idx" %in% sft_list_index_names(conn, "demo"))

  # Dropping a missing index stays silent on every backend.
  testthat::expect_true(sft_drop_index(conn, "demo", "never_existed"))
})
