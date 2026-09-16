# Extracted from test-db-restore.R:172

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "restore_unique",
    table_name = "restore_unique",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )
original <- insert_record(
    form = form,
    record = list(name = "Ada", email = "ada@example.org"),
    conn = conn,
    user = "tester"
  )
soft_delete_record(
    form = form,
    record_id = original$sft_id[1],
    conn = conn,
    user = "tester"
  )
insert_record(
    form = form,
    record = list(name = "Grace", email = "ada@example.org"),
    conn = conn,
    user = "tester"
  )
testthat::expect_error(
    restore_record(
      form = form,
      record_id = original$sft_id[1],
      conn = conn,
      user = "tester"
    ),
    "already held by an active record"
  )
