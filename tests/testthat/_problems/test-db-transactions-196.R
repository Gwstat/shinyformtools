# Extracted from test-db-transactions.R:196

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "transaction_restore",
    table_name = "transaction_restore",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )
inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn,
    user = "tester"
  )
soft_delete_record(
    form = form,
    record_id = inserted$sft_id[1],
    conn = conn,
    user = "tester"
  )
DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_audit_restore
     BEFORE INSERT ON sft_audit_log
     BEGIN
       SELECT RAISE(ABORT, 'forced audit failure');
     END"
  )
testthat::expect_error(
    restore_record(
      form = form,
      record_id = inserted$sft_id[1],
      conn = conn,
      user = "tester"
    ),
    "forced audit failure"
  )
