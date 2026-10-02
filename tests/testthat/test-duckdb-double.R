test_that("DuckDB stores numbers as DOUBLE and widens older FLOAT columns", {
  skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  amounts <- form(
    form_id = "a", table_name = "a", db = db_duckdb(path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "amount", label = "Amount", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(amounts$db)
  insert_record(amounts, list(name = "x", amount = 123456789), conn = conn)
  expect_identical(fetch_records(amounts, conn = conn)$amount, 123456789)
  insert_record(amounts, list(name = "y", amount = 0.1), conn = conn)
  expect_identical(fetch_records(amounts, conn = conn)$amount[2], 0.1)

  # A database from before: the column is FLOAT.
  DBI::dbExecute(conn, 'ALTER TABLE a ALTER COLUMN amount TYPE FLOAT')
  expect_false(sft_schema_is_current(conn, amounts))

  # The next CRUD call notices and widens it; the stored values stay.
  insert_record(amounts, list(name = "z", amount = 987654321), conn = conn)
  type <- DBI::dbGetQuery(conn, "SELECT data_type FROM information_schema.columns WHERE table_name = 'a' AND column_name = 'amount'")$data_type
  expect_identical(type, "DOUBLE")
  expect_identical(fetch_records(amounts, conn = conn)$amount[3], 987654321)
  expect_true(sft_schema_is_current(conn, amounts))
})

test_that("DuckDB widens FLOAT columns of a table with a unique index, once", {
  skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  people <- form(
    form_id = "p", table_name = "p", db = db_duckdb(path),
    fields = list(
      form_field(id = "email", label = "Email", unique = TRUE),
      form_field(id = "amount", label = "Amount", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(people$db)
  insert_record(people, list(email = "a@x", amount = 1), conn = conn)

  # Simulate the older schema: the column as FLOAT, the unique index in place.
  indexes <- DBI::dbGetQuery(conn, "SELECT index_name, sql FROM duckdb_indexes() WHERE table_name = 'p'")
  for (name in indexes$index_name) DBI::dbExecute(conn, paste0('DROP INDEX "', name, '"'))
  DBI::dbExecute(conn, "ALTER TABLE p ALTER COLUMN amount TYPE FLOAT")
  for (sql in indexes$sql) DBI::dbExecute(conn, sub(";\\s*$", "", sql))

  expect_no_warning(insert_record(people, list(email = "b@x", amount = 987654321), conn = conn))
  type <- DBI::dbGetQuery(conn, "SELECT data_type FROM information_schema.columns WHERE table_name = 'p' AND column_name = 'amount'")$data_type
  expect_identical(type, "DOUBLE")
  expect_identical(fetch_records(people, conn = conn)$amount[2], 987654321)
  expect_true(sft_schema_is_current(conn, people))
  # The unique index is back and works.
  expect_error(insert_record(people, list(email = "a@x", amount = 2), conn = conn), class = "sft_validation_error")
  expect_setequal(DBI::dbGetQuery(conn, "SELECT index_name FROM duckdb_indexes() WHERE table_name = 'p'")$index_name, indexes$index_name)
})

test_that("on DuckDB a field with empty values and deleted records can become unique", {
  skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  plain <- form(
    form_id = "u", table_name = "u", db = db_duckdb(path),
    fields = list(form_field(id = "name", label = "Name"), form_field(id = "email", label = "Email"))
  )
  conn <- local_test_conn(plain$db)
  insert_record(plain, list(name = "a", email = ""), conn = conn)
  insert_record(plain, list(name = "b", email = ""), conn = conn)
  insert_record(plain, list(name = "c", email = "c@x"), conn = conn)
  soft_delete_record(plain, record_id = 3, conn = conn)

  unique_email <- form(
    form_id = "u", table_name = "u", db = db_duckdb(path),
    fields = list(form_field(id = "name", label = "Name"), form_field(id = "email", label = "Email", unique = TRUE))
  )
  expect_identical(nrow(fetch_records(unique_email, conn = conn)), 2L)
  expect_true(sft_schema_is_current(conn, unique_email))
  # The deleted record's value is free again, a live duplicate is refused.
  expect_no_error(insert_record(unique_email, list(name = "d", email = "c@x"), conn = conn))
  expect_error(insert_record(unique_email, list(name = "e", email = "c@x"), conn = conn), class = "sft_validation_error")
})

test_that("old 4-byte number columns do not stop a form under schema_policy = 'manual'", {
  skip_if_not_installed("duckdb")
  db <- db_duckdb(tempfile(fileext = ".duckdb"))
  mk <- function(policy) form(form_id = "m", table_name = "m", db = db, schema_policy = policy,
    fields = list(form_field(id = "name", label = "Name"),
                  form_field(id = "n", label = "N", input_type = "numericInput")))
  conn <- db_connect(db); on.exit(db_disconnect(conn))
  init_db(mk("safe"), conn = conn)
  insert_record(mk("safe"), list(name = "a", n = 1), conn = conn)
  DBI::dbExecute(conn, "ALTER TABLE m ALTER COLUMN n TYPE REAL")

  f <- mk("manual")
  expect_identical(nrow(fetch_records(f, conn = conn)), 1L)
  expect_no_error(insert_record(f, list(name = "b", n = 2), conn = conn))
  # init_db() still widens them when someone runs it.
  init_db(f, conn = conn)
  expect_length(sft_duckdb_narrow_real_columns(conn, f), 0L)
})

test_that("an integer64 value is stored as its number on DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  f <- form(form_id = "i64", table_name = "i64", db = db_duckdb(tempfile(fileext = ".duckdb")), fields = list(
    form_field(id = "n", label = "N", input_type = "numericInput"),
    form_field(id = "t", label = "T")))
  conn <- db_connect(f$db); on.exit(db_disconnect(conn))
  init_db(f, conn = conn)
  insert_record(f, list(n = bit64::as.integer64(12), t = bit64::as.integer64(12)), conn = conn)
  r <- fetch_records(f, conn = conn)
  expect_identical(r$n, 12)
  expect_equal(as.numeric(r$t), 12)
})

test_that("a new unique field can be added to an existing DuckDB form", {
  skip_if_not_installed("duckdb")
  db <- db_duckdb(tempfile(fileext = ".duckdb"))
  v1 <- form(form_id = "people", table_name = "people", db = db,
             fields = list(form_field(id = "name", label = "Name")))
  v2 <- form(form_id = "people", table_name = "people", db = db,
             fields = list(form_field(id = "name", label = "Name"),
                           form_field(id = "code", label = "Code", unique = TRUE)))
  conn <- db_connect(db); on.exit(db_disconnect(conn))
  init_db(v1, conn = conn)
  insert_record(v1, list(name = "a"), conn = conn)
  soft_delete_record(v1, record_id = insert_record(v1, list(name = "b"), conn = conn)$sft_id, conn = conn)

  expect_identical(nrow(fetch_records(v2, conn = conn)), 1L)
  insert_record(v2, list(name = "c", code = "X"), conn = conn)
  expect_error(insert_record(v2, list(name = "d", code = "X"), conn = conn), class = "sft_validation_error")
  expect_true(sft_schema_is_current(conn, v2))
})

test_that("plan_migration and apply_migration widen an old range slider column like init_db", {
  skip_if_not_installed("duckdb")
  db <- db_duckdb(tempfile(fileext = ".duckdb"))
  f <- form(form_id = "spans", table_name = "spans", db = db, fields = list(
    form_field(id = "name", label = "Name"),
    form_field(id = "span", label = "Span", input_type = "sliderInput",
               args = list(min = 0, max = 10, value = c(2, 5)))
  ))
  conn <- local_test_conn(db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "a"), conn = conn)
  # As an older version created it: a number column.
  DBI::dbExecute(conn, "ALTER TABLE spans ALTER COLUMN span TYPE DOUBLE")

  plan <- plan_migration(f, conn)
  expect_false("type_warning" %in% plan$actions$action)
  apply_migration(f, conn, plan = plan)
  expect_identical(sft_range_slider_narrow_columns(conn, f), character())
  insert_record(f, list(name = "b", span = c(1, 4)), conn = conn)
  expect_identical(fetch_records(f, conn = conn)$span[2], "[1,4]")
})

test_that("an insert's audit row lists the same changed fields on DuckDB as on SQLite", {
  skip_if_not_installed("duckdb")
  changed <- function(db) {
    f <- form(form_id = "aud", table_name = "aud", db = db, fields = list(
      form_field(id = "name", label = "Name")
    ))
    conn <- db_connect(db)
    on.exit(db_disconnect(conn))
    init_db(f, conn = conn)
    insert_record(f, list(name = "a"), conn = conn)
    sort(jsonlite::fromJSON(fetch_audit_log(f, conn = conn)$changed_fields_json[1]))
  }
  expect_identical(changed(db_duckdb(tempfile(fileext = ".duckdb"))),
                   changed(db_sqlite(tempfile(fileext = ".sqlite"))))
})
