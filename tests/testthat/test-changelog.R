testthat::test_that("sft_audit_history and changelog_box read record history", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "changelog_test",
    table_name = "changelog_test",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name")
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

  context <- sft_form_context(
    form = form,
    conn = conn
  )

  history <- audit_history(
    context = context,
    record = inserted,
    limit = 1L
  )

  testthat::expect_equal(nrow(history), 1L)
  testthat::expect_equal(history$action, "update")

  box <- changelog_box(
    context = context,
    record = inserted,
    limit = 2L
  )

  testthat::expect_s3_class(box, "shiny.tag")
})
