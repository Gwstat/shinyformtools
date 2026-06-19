testthat::test_that("unique slot allows reuse after soft-delete and blocks unsafe restore", {
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

  # While the value is held by an active record a duplicate is rejected.
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

  # After soft-delete the value is free again.
  reused <- insert_record(
    form = form,
    record = list(name = "Ada Reborn", email = "ada@example.org"),
    conn = conn
  )
  testthat::expect_equal(reused$email, "ada@example.org")

  live <- fetch_records(form = form, conn = conn, include_deleted = FALSE)
  testthat::expect_equal(nrow(live), 1L)
  testthat::expect_equal(live$name, "Ada Reborn")

  # Restoring the old record must fail: its unique value is now taken by a live
  # record, so the friendly pre-check rejects the reactivation before the
  # composite unique index would.
  testthat::expect_error(
    restore_record(
      form = form,
      record_id = first$sft_id[1],
      conn = conn
    ),
    "already held by an active record"
  )
})

testthat::test_that("empty values do not collide on a unique field", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "optional",
    table_name = "optional",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "code", label = "Code", unique = TRUE)
    )
  )

  insert_record(
    form = form,
    record = list(name = "A", code = ""),
    conn = conn
  )

  # A second empty unique value must be allowed (stored as NULL, distinct in the
  # index), matching the application-level check that exempts empty values.
  second <- insert_record(
    form = form,
    record = list(name = "B", code = ""),
    conn = conn
  )
  testthat::expect_equal(nrow(second), 1L)
})

testthat::test_that("changing unique = TRUE to FALSE drops the obsolete unique index", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "accounts",
    table_name = "accounts",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )

  insert_record(
    form = form_v1,
    record = list(name = "Ada", email = "ada@example.org"),
    conn = conn
  )

  testthat::expect_true(
    "uq_accounts__email" %in% sft_list_index_names(conn, "accounts")
  )

  # Same field, now unique = FALSE, new schema version.
  form_v2 <- form(
    form_id = "accounts",
    table_name = "accounts",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = FALSE)
    )
  )

  # A write under v2 reconciles the schema, drops the stale index, and the
  # duplicate is now allowed.
  dup <- insert_record(
    form = form_v2,
    record = list(name = "Ada II", email = "ada@example.org"),
    conn = conn
  )
  testthat::expect_equal(dup$email, "ada@example.org")

  testthat::expect_false(
    "uq_accounts__email" %in% sft_list_index_names(conn, "accounts")
  )

  live <- fetch_records(form = form_v2, conn = conn, include_deleted = FALSE)
  testthat::expect_equal(sum(live$email == "ada@example.org"), 2L)
})

testthat::test_that("the migration planner manages unique indexes as actions", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "members",
    table_name = "members",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE)
    )
  )
  init_db(form_v1, conn = conn)

  testthat::expect_false(
    any(startsWith(sft_list_index_names(conn, "members"), "uq_members__"))
  )

  # Adding a unique field must surface as a create_index action in the plan,
  # not as a side effect of init.
  form_v2 <- form(
    form_id = "members",
    table_name = "members",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE)
    )
  )

  plan_add <- plan_migration(conn, form_v2)
  testthat::expect_true("create_index" %in% plan_add$actions$action)

  apply_migration(conn, form_v2, plan = plan_add)
  testthat::expect_true(
    "uq_members__email" %in% sft_list_index_names(conn, "members")
  )

  # Removing uniqueness must surface as a drop_index action.
  form_v3 <- form(
    form_id = "members",
    table_name = "members",
    db_path = db_path,
    version = 3L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = FALSE)
    )
  )

  plan_drop <- plan_migration(conn, form_v3)
  testthat::expect_true("drop_index" %in% plan_drop$actions$action)

  apply_migration(conn, form_v3, plan = plan_drop)
  testthat::expect_false(
    "uq_members__email" %in% sft_list_index_names(conn, "members")
  )

  # Both index actions are recorded in the migration log.
  logged <- DBI::dbGetQuery(
    conn,
    "SELECT action, db_column FROM sft_schema_migrations ORDER BY migration_id"
  )
  testthat::expect_true("create_index" %in% logged$action)
  testthat::expect_true("drop_index" %in% logged$action)
  testthat::expect_true("uq_members__email" %in% logged$db_column)
})
