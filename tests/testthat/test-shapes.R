testthat::skip_if_not_installed("sf")
testthat::skip_if_not_installed("geojsonsf")

make_district_shapes <- function() {
  poly_a <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  poly_b <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  poly_z <- sf::st_polygon(list(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5))))

  sf::st_sf(
    wkr = c("A", "B", "Z"), # Z has no matching record
    geometry = sf::st_sfc(poly_a, poly_b, poly_z, crs = 4326)
  )
}

make_district_form <- function(db_path) {
  form(
    form_id = "districts",
    table_name = "districts",
    db_path = db_path,
    fields = list(
      form_field(id = "district_id", label = "District", mandatory = TRUE),
      form_field(id = "electorate", label = "Electorate", input_type = "numericInput"),
      shape_field(id = "geometry", label = "Boundary", crs = 4326, encoding = "geojson")
    )
  )
}

testthat::test_that("sft_shape_field creates a stored, non-editable, non-input column", {
  field <- shape_field(id = "geometry", crs = 4326)

  testthat::expect_true(sft_is_shape_field(field))
  testthat::expect_false(sft_is_input_field(field))
  testthat::expect_true(sft_is_stored_field(field))
  testthat::expect_false(field$editable)
  testthat::expect_equal(field$db_type, "TEXT")
})

testthat::test_that("a shape field becomes a database column in the schema", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- make_district_form(db_path)
  init_db(form, conn = conn)

  info <- sft_table_info(conn, "districts")
  testthat::expect_true("geometry" %in% info$name)
})

testthat::test_that("sft_attach_shapes stores geometry that round-trips, and reports unmatched features", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- make_district_form(db_path)
  init_db(form, conn = conn)

  insert_record(form, list(district_id = "A", electorate = 100), conn = conn)
  insert_record(form, list(district_id = "B", electorate = 200), conn = conn)

  result <- attach_shapes(
    form = form,
    shapes = make_district_shapes(),
    key = c(district_id = "wkr"),
    conn = conn,
    user = "loader"
  )

  testthat::expect_equal(result$attached, 2L)
  testthat::expect_equal(result$unmatched, 1L)

  records <- fetch_records(form, conn = conn)
  records <- records[order(records$district_id), , drop = FALSE]

  testthat::expect_true("geometry" %in% names(records))
  testthat::expect_true(all(nzchar(records$geometry)))

  field <- Filter(sft_is_shape_field, form$fields)[[1L]]
  decoded <- decode_shape(records$geometry, field)

  testthat::expect_s3_class(decoded, "sfc")
  testthat::expect_equal(
    as.character(sf::st_geometry_type(decoded)),
    c("POLYGON", "POLYGON")
  )

  # An attach action is audited per updated record.
  audit <- DBI::dbGetQuery(
    conn,
    "SELECT action FROM sft_audit_log WHERE action = 'attach_shape'"
  )
  testthat::expect_equal(nrow(audit), 2L)
})

testthat::test_that("editing input fields leaves the fixed geometry untouched", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- make_district_form(db_path)
  init_db(form, conn = conn)

  inserted <- insert_record(form, list(district_id = "A", electorate = 100), conn = conn)
  attach_shapes(
    form = form,
    shapes = make_district_shapes(),
    key = c(district_id = "wkr"),
    conn = conn
  )

  before <- fetch_records(form, conn = conn)
  geometry_before <- before$geometry[before$district_id == "A"]

  update_record(
    form = form,
    record_id = inserted$sft_id[1],
    values = list(electorate = 999),
    conn = conn,
    user = "editor"
  )

  after <- fetch_records(form, conn = conn)

  testthat::expect_equal(after$electorate[after$district_id == "A"], 999)
  testthat::expect_equal(after$geometry[after$district_id == "A"], geometry_before)
})

testthat::test_that("shape columns are not offered as record-table columns", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- make_district_form(db_path)
  init_db(form, conn = conn)
  insert_record(form, list(district_id = "A", electorate = 100), conn = conn)
  attach_shapes(
    form = form,
    shapes = make_district_shapes(),
    key = c(district_id = "wkr"),
    conn = conn
  )

  data <- fetch_records(form, conn = conn)

  testthat::expect_false("geometry" %in% sft_allowed_record_columns(form, data))
  testthat::expect_false("geometry" %in% sft_default_record_columns(form, data))
})
