test_that("numeric fields stay numeric in the records table", {
  scores <- form(
    form_id = "scores", table_name = "scores",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "score", label = "Score", input_type = "numericInput", args = list(value = 0))
    )
  )
  conn <- local_test_conn(scores$db)
  for (s in list(92, 100.5, NA)) insert_record(scores, list(name = "n", score = s), conn = conn)

  data <- records_datatable(fetch_records(scores, conn = conn), scores)$x$data

  # DT sorts and filters a numeric column as numbers only if it stays numeric.
  expect_type(data$Score, "double")
  expect_identical(data$Score, c(92, 100.5, NA))
  expect_type(data$Name, "character")
})
