# Extracted from test-db-unique-index.R:59

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "people",
    table_name = "people",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )
first <- insert_record(
    form = form,
    record = list(name = "Ada", email = "ada@example.org"),
    conn = conn
  )
testthat::expect_error(
    insert_record(
      form = form,
      record = list(name = "Ada II", email = "ada@example.org"),
      conn = conn
    )
  )
soft_delete_record(
    form = form,
    record_id = first$sft_id[1],
    conn = conn
  )
reused <- insert_record(
    form = form,
    record = list(name = "Ada Reborn", email = "ada@example.org"),
    conn = conn
  )
testthat::expect_equal(reused$email, "ada@example.org")
live <- fetch_records(form = form, conn = conn, include_deleted = FALSE)
testthat::expect_equal(nrow(live), 1L)
testthat::expect_equal(live$name, "Ada Reborn")
testthat::expect_error(
    restore_record(
      form = form,
      record_id = first$sft_id[1],
      conn = conn
    ),
    "already held by an active record"
  )
