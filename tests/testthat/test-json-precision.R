test_that("restore writes numbers back at full precision", {
  points <- form(
    form_id = "p", table_name = "p", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "lat", label = "Lat", input_type = "numericInput"),
      form_field(id = "range", label = "Range", input_type = "sliderInput",
                 args = list(min = 0, max = 10, value = c(1, 2)))
    )
  )
  conn <- local_test_conn(points$db)
  insert_record(points, list(lat = 48.137154, range = c(1.23456789, 2.5)), conn = conn)
  update_record(points, list(lat = 3.14159265), record_id = 1, conn = conn)
  soft_delete_record(points, record_id = 1, conn = conn)
  restore_record(points, record_id = 1, conn = conn)

  stored <- fetch_records(points, conn = conn)
  expect_identical(stored$lat, 3.14159265)
  expect_match(stored$range, "1.23456789", fixed = TRUE)
  expect_match(fetch_audit_log(points, conn = conn)$new_data_json[1], "48.137154", fixed = TRUE)
})
