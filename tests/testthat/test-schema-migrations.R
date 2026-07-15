# sft_schema_migrations is the structural counterpart of the audit log: the only
# record of what the package did to a table's shape. fetch_schema_migrations()
# exposes it, and from_version says where each change came from.

migrations_form <- function(db, version, fields) {
  form(
    form_id = "hist",
    table_name = "hist",
    db = db,
    version = version,
    fields = fields
  )
}

testthat::test_that("fetch_schema_migrations returns the history oldest first", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  conn <- db_connect(db)
  on.exit(db_disconnect(conn), add = TRUE)

  v1 <- migrations_form(db, 1L, list(
    form_field("name", "Name"),
    form_field("email", "Email", unique = TRUE)
  ))
  init_db(v1, conn = conn, apply = TRUE, user = "alice")

  # v2 adds a column.
  v2 <- migrations_form(db, 2L, list(
    form_field("name", "Name"),
    form_field("email", "Email", unique = TRUE),
    form_field("city", "City")
  ))
  init_db(v2, conn = conn, apply = TRUE, user = "bob")

  history <- fetch_schema_migrations(v2, conn = conn)

  testthat::expect_equal(history$action, c("create_table", "add_column"))
  testthat::expect_equal(history$db_column, c("hist", "city"))
  testthat::expect_equal(history$applied_by, c("alice", "bob"))
})

testthat::test_that("from_version records where each change came from", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  conn <- db_connect(db)
  on.exit(db_disconnect(conn), add = TRUE)

  v1 <- migrations_form(db, 1L, list(form_field("name", "Name")))
  init_db(v1, conn = conn, apply = TRUE, user = "alice")

  v2 <- migrations_form(db, 2L, list(
    form_field("name", "Name"),
    form_field("city", "City")
  ))
  init_db(v2, conn = conn, apply = TRUE, user = "bob")

  v3 <- migrations_form(db, 3L, list(form_field("name", "Name")))
  init_db(v3, conn = conn, apply = TRUE, user = "carol")

  history <- fetch_schema_migrations(v3, conn = conn)

  # The first action has nothing to come from; the rest are real transitions.
  testthat::expect_true(is.na(history$from_version[1]))
  testthat::expect_equal(history$to_version[1], 1L)

  testthat::expect_equal(
    history$from_version[history$action == "add_column"], 1L
  )
  testthat::expect_equal(
    history$to_version[history$action == "add_column"], 2L
  )
  testthat::expect_equal(
    history$from_version[history$action == "retire_column"], 2L
  )
  testthat::expect_equal(
    history$to_version[history$action == "retire_column"], 3L
  )
})

testthat::test_that("retiring a field keeps its column, and the history says why", {
  # The point of the table: a retired column stays in the database (dropping it
  # would throw data away), so without this log it is an unexplained column.
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  conn <- db_connect(db)
  on.exit(db_disconnect(conn), add = TRUE)

  v1 <- migrations_form(db, 1L, list(
    form_field("name", "Name"),
    form_field("city", "City")
  ))
  init_db(v1, conn = conn, apply = TRUE, user = "alice")
  insert_record(v1, list(name = "Ada", city = "London"), conn = conn)

  v2 <- migrations_form(db, 2L, list(form_field("name", "Name")))
  init_db(v2, conn = conn, apply = TRUE, user = "bob")

  columns <- DBI::dbGetQuery(conn, "PRAGMA table_info(hist)")$name
  testthat::expect_true("city" %in% columns)

  retired <- DBI::dbGetQuery(conn, "SELECT city FROM hist")
  testthat::expect_equal(retired$city[1], "London")

  history <- fetch_schema_migrations(v2, conn = conn)
  testthat::expect_true("retire_column" %in% history$action)
  testthat::expect_equal(
    history$db_column[history$action == "retire_column"], "city"
  )
})

testthat::test_that("fetch_schema_migrations reports only the form's own history", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  conn <- db_connect(db)
  on.exit(db_disconnect(conn), add = TRUE)

  a <- form(form_id = "hist_a", table_name = "hist_a", db = db,
            fields = list(form_field("name", "Name")))
  b <- form(form_id = "hist_b", table_name = "hist_b", db = db,
            fields = list(form_field("name", "Name")))

  init_db(a, conn = conn, apply = TRUE)
  init_db(b, conn = conn, apply = TRUE)

  history <- fetch_schema_migrations(a, conn = conn)

  testthat::expect_equal(nrow(history), 1L)
  testthat::expect_true(all(history$form_id == "hist_a"))
})

testthat::test_that("fetch_schema_migrations rejects a non-form", {
  testthat::expect_error(
    fetch_schema_migrations("not a form"),
    "form must be a form object"
  )
})
