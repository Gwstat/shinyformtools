# Extracted from test-db-restore.R:321

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      )
    )
  )
inserted <- insert_record(
    form = form,
    record = list(name = "Ada"),
    conn = conn,
    user = "tester"
  )
update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "tester"
  )
restored <- restore_record(
    form = form,
    record_id = inserted$sft_id[1],
    version_no = 1L,
    conn = conn,
    user = "tester"
  )
