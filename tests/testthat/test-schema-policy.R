# form(schema_policy = "manual") was accepted, stored and printed but never
# read: every CRUD call self-healed the schema regardless. It now switches the
# probe-gated reconciliation off; init_db() stays the explicit manual step.

sft_test_policy_form <- function(db_path, policy, version = 1L, extra = NULL) {
  form(
    form_id = "policy",
    table_name = "policy",
    db = db_sqlite(db_path),
    version = version,
    schema_policy = policy,
    fields = c(
      list(form_field(id = "name", label = "Name")),
      extra
    )
  )
}

test_that("manual policy refuses to touch a cold database until init_db() ran", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  manual <- sft_test_policy_form(db_path, "manual")

  expect_error(
    insert_record(manual, list(name = "Ada"), conn = conn),
    "schema_policy = \"manual\""
  )
  expect_false("policy" %in% DBI::dbListTables(conn))

  init_db(manual, conn = conn)
  inserted <- insert_record(manual, list(name = "Ada"), conn = conn)
  expect_identical(inserted$name, "Ada")
})

test_that("manual policy reports drift instead of migrating behind a CRUD call", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  v1 <- sft_test_policy_form(db_path, "manual")
  init_db(v1, conn = conn)
  insert_record(v1, list(name = "Ada"), conn = conn)

  v2 <- sft_test_policy_form(
    db_path, "manual", version = 2L,
    extra = list(form_field(id = "city", label = "City"))
  )

  expect_error(fetch_records(v2, conn = conn), "not current")
  expect_false("city" %in% DBI::dbListFields(conn, "policy"))

  # The explicit step works under the manual policy, and the probe goes green.
  init_db(v2, conn = conn)
  expect_true("city" %in% DBI::dbListFields(conn, "policy"))
  expect_identical(fetch_records(v2, conn = conn)$name, "Ada")
})

test_that("the default safe policy still self-heals", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  safe <- sft_test_policy_form(db_path, "safe")

  inserted <- insert_record(safe, list(name = "Ada"), conn = conn)
  expect_identical(inserted$name, "Ada")
})
