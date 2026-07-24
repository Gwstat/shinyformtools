testthat::test_that("sft_form keeps backwards-compatible SQLite db config", {
  form <- form(
    form_id = "backend_test",
    db_path = "dev/backend_test.sqlite",
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      )
    )
  )

  testthat::expect_s3_class(form$db, "sft_db_config")
  testthat::expect_equal(form$db$type, "sqlite")
  testthat::expect_equal(form$db$path, "dev/backend_test.sqlite")
})

testthat::test_that("sft_db_sqlite creates persistent SQLite connection", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)

  testthat::expect_true(DBI::dbIsValid(conn))
  testthat::expect_equal(sft_db_backend(conn), "sqlite")
})

testthat::test_that("sft_db_mariadb stores connection metadata without requiring connection", {
  db <- db_mariadb(
    dbname = "shinyformtools",
    host = "127.0.0.1",
    port = 3306L,
    user = "sft",
    password = "secret"
  )

  testthat::expect_s3_class(db, "sft_db_config")
  testthat::expect_equal(db$type, "mariadb")
  testthat::expect_equal(sft_redact_db_config(db)$password, "<redacted>")
})

testthat::test_that("sft_db_duckdb stores connection metadata without requiring connection", {
  db <- db_duckdb(
    path = "dev/backend_test.duckdb",
    read_only = FALSE,
    config = list(threads = "1")
  )

  testthat::expect_s3_class(db, "sft_db_config")
  testthat::expect_equal(db$type, "duckdb")
  testthat::expect_equal(db$path, "dev/backend_test.duckdb")
  testthat::expect_false(db$read_only)
  testthat::expect_equal(db$config, list(threads = "1"))
})

testthat::test_that("backend-specific column types resolve per backend", {
  # sft_db_backend() dispatches on the connection's class names only, so a
  # bare object with the right class exercises the SQL-type helpers without a
  # live server.
  mariadb_conn <- structure(list(), class = "MariaDBConnection")
  sqlite_conn <- structure(list(), class = "SQLiteConnection")

  testthat::expect_identical(sft_db_backend(mariadb_conn), "mariadb")

  # MariaDB caps TEXT at 64KB; the JSON payload columns must be MEDIUMTEXT so
  # a large config_json / audit snapshot cannot overflow under strict mode.
  testthat::expect_identical(sft_long_text_definition(mariadb_conn), "MEDIUMTEXT")
  testthat::expect_identical(sft_long_text_definition(sqlite_conn), "TEXT")

  types <- sft_system_table_types(mariadb_conn)
  testthat::expect_identical(types$long_text, "MEDIUMTEXT")
  testthat::expect_identical(types$short_text, "VARCHAR(255)")

  # MariaDB's own text spellings normalise into the TEXT bucket, so drift
  # detection does not flag them as type changes.
  for (type in c("mediumtext", "LONGTEXT", "tinytext", "varchar(255)")) {
    testthat::expect_identical(sft_normalize_db_type_for_compare(type), "TEXT")
  }
})

testthat::test_that("sft_db_duckdb creates an optional DuckDB connection", {
  testthat::skip_if_not_installed("duckdb")

  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_connect(db_duckdb(db_path))
  on.exit(db_disconnect(conn), add = TRUE)

  testthat::expect_true(DBI::dbIsValid(conn))
  testthat::expect_equal(sft_db_backend(conn), "duckdb")
})
