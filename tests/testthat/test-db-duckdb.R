testthat::test_that("DuckDB backend supports CRUD, audit and preferences", {
  testthat::skip_if_not_installed("duckdb")

  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_connect(db_duckdb(db_path))
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "duckdb_simple",
    table_name = "duckdb_simple",
    db = db_duckdb(db_path),
    fields = list(
      form_field(
        id = "name",
        label = "Name",
        mandatory = TRUE
      ),
      form_field(
        id = "email",
        label = "E-Mail",
        unique = TRUE
      )
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(
      name = "Ada",
      email = "ada@example.org"
    ),
    conn = conn,
    user = "duckdb_user"
  )

  testthat::expect_equal(inserted$sft_id[1], 1L)
  testthat::expect_equal(inserted$name[1], "Ada")
  testthat::expect_true(nzchar(inserted$sft_easy_id[1]))

  updated <- update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(name = "Ada Lovelace"),
    conn = conn,
    user = "duckdb_user"
  )

  testthat::expect_equal(updated$name[1], "Ada Lovelace")

  sft_set_column_view(
    conn = conn,
    form = form,
    user = "duckdb_user",
    view_name = "Kontakt",
    columns = c("name", "email")
  )

  testthat::expect_equal(
    sft_get_column_view(
      conn = conn,
      form = form,
      user = "duckdb_user",
      view_name = "Kontakt"
    ),
    c("name", "email")
  )

  audit <- fetch_audit_log(
    form = form,
    conn = conn,
    record_id = inserted$sft_id[1]
  )

  testthat::expect_equal(audit$action, c("insert", "update"))
  testthat::expect_equal(audit$log_id, c(1L, 2L))
})
