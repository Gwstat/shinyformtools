range_form <- function(db) {
  form(
    form_id = "sl", table_name = "sl", db = db,
    fields = list(
      form_field(id = "name", label = "Name", unique = TRUE),
      form_field(id = "range", label = "Range", input_type = "sliderInput",
                 args = list(min = 0, max = 10, value = c(1, 2))),
      form_field(id = "level", label = "Level", input_type = "sliderInput",
                 args = list(min = 0, max = 10, value = 5))
    )
  )
}

check_range_round_trip <- function(db, conn) {
  f <- range_form(db)
  insert_record(f, list(name = "a", range = c(1.25, 7.5), level = 3), conn = conn)
  stored <- fetch_records(f, conn = conn)
  expect_identical(sft_ui_value(f$fields[[2]], stored$range), c(1.25, 7.5))
  expect_identical(as.numeric(stored$level), 3)
}

test_that("a slider range is stored on SQLite", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  check_range_round_trip(db, local_test_conn(db))
})

test_that("a slider range is stored on DuckDB, also where the column is still a number", {
  skip_if_not_installed("duckdb")
  db <- db_duckdb(tempfile(fileext = ".duckdb"))
  conn <- local_test_conn(db)
  check_range_round_trip(db, conn)

  # A database from an earlier version: the range column is a number, with a
  # unique index on the table.
  indexes <- DBI::dbGetQuery(conn, "SELECT index_name, sql FROM duckdb_indexes() WHERE table_name = 'sl'")
  for (name in indexes$index_name) DBI::dbExecute(conn, paste0('DROP INDEX "', name, '"'))
  DBI::dbExecute(conn, "UPDATE sl SET range = NULL")
  DBI::dbExecute(conn, "DELETE FROM sl")
  DBI::dbExecute(conn, "ALTER TABLE sl ALTER COLUMN range TYPE FLOAT")
  for (sql in indexes$sql) DBI::dbExecute(conn, sub(";\\s*$", "", sql))

  f <- range_form(db)
  expect_false(sft_schema_is_current(conn, f))
  insert_record(f, list(name = "b", range = c(2, 3), level = 1), conn = conn)
  expect_identical(sft_ui_value(f$fields[[2]], fetch_records(f, conn = conn)$range), c(2, 3))
  expect_true(sft_schema_is_current(conn, f))
})
