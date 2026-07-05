testthat::test_that("sft_init_db creates system tables and the main table", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  fields <- list(
    form_field(
      id = "name",
      label = "Name"
    ),
    form_field(
      id = "email",
      label = "E-Mail",
      unique = TRUE
    )
  )

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    fields = fields
  )

  init_db(form, conn = conn)

  tables <- DBI::dbListTables(conn)

  testthat::expect_true("sft_forms" %in% tables)
  testthat::expect_true("sft_fields" %in% tables)
  testthat::expect_true("sft_schema_migrations" %in% tables)
  testthat::expect_true("sft_audit_log" %in% tables)
  testthat::expect_true("simple" %in% tables)

  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(simple)")

  testthat::expect_true("sft_id" %in% table_info$name)
  testthat::expect_true("sft_uuid" %in% table_info$name)
  testthat::expect_true("sft_is_deleted" %in% table_info$name)
  testthat::expect_true("name" %in% table_info$name)
  testthat::expect_true("email" %in% table_info$name)
})

testthat::test_that("sft_plan_migration detects a missing table", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  init_system_tables(conn)

  fields <- list(
    form_field(
      id = "name",
      label = "Name"
    )
  )

  form <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    fields = fields
  )

  plan <- plan_migration(conn, form)

  testthat::expect_s3_class(plan, "sft_migration_plan")
  testthat::expect_equal(nrow(plan$actions), 1L)
  testthat::expect_equal(plan$actions$action, "create_table")
})

testthat::test_that("sft_apply_migration adds a new field column", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      )
    )
  )

  init_db(form_v1, conn = conn)

  form_v2 <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      ),
      form_field(
        id = "email",
        label = "E-Mail"
      )
    )
  )

  plan <- plan_migration(conn, form_v2)

  testthat::expect_true("add_column" %in% plan$actions$action)
  testthat::expect_true("email" %in% plan$actions$db_column)

  apply_migration(conn, form_v2, plan = plan)

  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(simple)")

  testthat::expect_true("email" %in% table_info$name)
})

testthat::test_that("sft_apply_migration retires removed fields without dropping columns", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      ),
      form_field(
        id = "email",
        label = "E-Mail"
      )
    )
  )

  init_db(form_v1, conn = conn)

  form_v2 <- form(
    form_id = "simple",
    table_name = "simple",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      )
    )
  )

  plan <- plan_migration(conn, form_v2)

  testthat::expect_true("retire_column" %in% plan$actions$action)
  testthat::expect_true("email" %in% plan$actions$db_column)

  apply_migration(conn, form_v2, plan = plan)

  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(simple)")

  testthat::expect_true("email" %in% table_info$name)

  field_meta <- DBI::dbGetQuery(
    conn,
    "
    SELECT status
    FROM sft_fields
    WHERE form_id = ? AND field_id = ?
    ",
    params = list("simple", "email")
  )

  testthat::expect_equal(field_meta$status, "retired")

  plan_after <- plan_migration(conn, form_v2)

  testthat::expect_false("retire_column" %in% plan_after$actions$action)
})
testthat::test_that("renamed fields reuse existing database columns without adding new columns", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "rename_test",
    table_name = "rename_test",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(
        id = "name",
        label = "Name"
      )
    )
  )

  init_db(form_v1, conn = conn)

  form_v2 <- form(
    form_id = "rename_test",
    table_name = "rename_test",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(
        id = "first_name",
        label = "Vorname",
        renamed_from = "name"
      )
    )
  )

  plan <- plan_migration(conn, form_v2)

  testthat::expect_equal(nrow(plan$actions), 0L)

  apply_migration(conn, form_v2, plan = plan)

  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(rename_test)")

  testthat::expect_true("name" %in% table_info$name)
  testthat::expect_false("first_name" %in% table_info$name)

  metadata <- DBI::dbGetQuery(
    conn,
    "
    SELECT field_id, db_column, status, renamed_from
    FROM sft_fields
    WHERE form_id = ?
    ORDER BY field_id
    ",
    params = list("rename_test")
  )

  testthat::expect_equal(
    metadata[metadata$field_id == "first_name", "db_column"],
    "name"
  )
  testthat::expect_equal(
    metadata[metadata$field_id == "first_name", "renamed_from"],
    "name"
  )
  testthat::expect_equal(
    metadata[metadata$field_id == "name", "status"],
    "retired"
  )
})

testthat::test_that("a failed migration rolls back on a transactional-DDL backend", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form_v1 <- form(
    form_id = "mig_rollback",
    table_name = "mig_rollback",
    db_path = db_path,
    version = 1L,
    fields = list(
      form_field(id = "name", label = "Name")
    )
  )

  init_db(form_v1, conn = conn, apply = TRUE)

  form_v2 <- form(
    form_id = "mig_rollback",
    table_name = "mig_rollback",
    db_path = db_path,
    version = 2L,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "city", label = "City")
    )
  )

  # The plan adds the `city` column. Force a failure when the action is logged,
  # which happens after the ALTER TABLE has already run within the transaction.
  DBI::dbExecute(
    conn,
    "CREATE TRIGGER fail_migration_log
     BEFORE INSERT ON sft_schema_migrations
     BEGIN
       SELECT RAISE(ABORT, 'forced migration failure');
     END"
  )

  testthat::expect_error(
    apply_migration(conn = conn, form = form_v2),
    "forced migration failure"
  )

  # The ALTER TABLE rolled back: the column is absent rather than left behind in
  # a half-migrated schema.
  info <- sft_table_info(conn, "mig_rollback")
  testthat::expect_false("city" %in% info$name)
})
