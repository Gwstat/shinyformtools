testthat::test_that("editable accepts a function and rejects bad values", {
  testthat::expect_error(
    form_field(id = "x", label = "X", editable = "yes"),
    "editable must be TRUE, FALSE, or a function"
  )

  fld <- form_field(id = "x", label = "X", editable = function(user) identical(user, "admin"))
  testthat::expect_true(is.function(fld$editable))
})

testthat::test_that("a function editable is only allowed on input fields", {
  testthat::expect_error(
    output_field(id = "o", output_type = "text"),
    NA
  )

  bad <- form_field(id = "o", label = "O", type = "text_output")
  bad$editable <- function(user) TRUE
  testthat::expect_error(sft_validate_field(bad), "Only input fields can have a function for editable")
})

testthat::test_that("sft_field_editable_for resolves per user and fails closed", {
  fn <- form_field(id = "x", label = "X", editable = function(user) identical(user, "admin"))
  testthat::expect_true(sft_field_editable_for(fn, "admin"))
  testthat::expect_false(sft_field_editable_for(fn, "bob"))

  static_true <- form_field(id = "y", label = "Y")
  static_false <- form_field(id = "z", label = "Z", editable = FALSE)
  testthat::expect_true(sft_field_editable_for(static_true, "anyone"))
  testthat::expect_false(sft_field_editable_for(static_false, "anyone"))

  boom <- form_field(id = "b", label = "B", editable = function(user) stop("boom"))
  testthat::expect_false(sft_field_editable_for(boom, "admin"))
})

testthat::test_that("sft_resolve_editable flattens functions to logicals", {
  form <- form(
    form_id = "resolve_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "secret", label = "Secret", editable = function(user) identical(user, "admin"))
    )
  )

  for_bob <- sft_resolve_editable(form, "bob")
  for_admin <- sft_resolve_editable(form, "admin")

  ed <- function(f, id) Filter(function(x) x$id == id, f$fields)[[1]]$editable
  testthat::expect_false(ed(for_bob, "secret"))
  testthat::expect_true(ed(for_admin, "secret"))
  # The static field is untouched, and the original form keeps its function.
  testthat::expect_true(ed(for_bob, "name"))
  testthat::expect_true(is.function(ed(form, "secret")))
})

testthat::test_that("sft_user_locked_input_fields lists only function-locked fields", {
  form <- form(
    form_id = "locked_test",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "fixed", label = "Fixed", editable = FALSE),
      form_field(id = "secret", label = "Secret", editable = function(user) identical(user, "admin"))
    )
  )

  testthat::expect_equal(sft_user_locked_input_fields(form, "bob"), "secret")
  testthat::expect_equal(sft_user_locked_input_fields(form, "admin"), character(0))
})

testthat::test_that("a form with a function editable initialises and stores editable = 1", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "store_test",
    table_name = "store_test",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "secret", label = "Secret", editable = function(user) identical(user, "admin"))
    )
  )

  testthat::expect_error(init_db(form, conn = conn, user = "system"), NA)

  stored <- DBI::dbGetQuery(
    conn,
    "SELECT field_id, editable FROM sft_fields WHERE form_id = 'store_test' AND field_id = 'secret'"
  )
  testthat::expect_equal(stored$editable, 1L)
})

testthat::test_that("update_record with a resolved form drops fields the user may not edit", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "enforce_test",
    table_name = "enforce_test",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "secret", label = "Secret", editable = function(user) identical(user, "admin"))
    )
  )

  rec <- insert_record(
    form = form,
    record = list(name = "Ada", secret = "original"),
    conn = conn,
    user = "system"
  )
  rid <- rec$sft_id[1]

  # A non-admin's attempt to change `secret` is dropped server-side; `name` saves.
  update_record(
    form = sft_resolve_editable(form, "bob"),
    record_id = rid,
    values = list(name = "Ada B.", secret = "hacked"),
    conn = conn,
    user = "bob"
  )

  after_bob <- fetch_records(form, conn = conn)
  testthat::expect_equal(after_bob$name, "Ada B.")
  testthat::expect_equal(after_bob$secret, "original")

  # An admin can change `secret`.
  update_record(
    form = sft_resolve_editable(form, "admin"),
    record_id = rid,
    values = list(secret = "updated"),
    conn = conn,
    user = "admin"
  )

  after_admin <- fetch_records(form, conn = conn)
  testthat::expect_equal(after_admin$secret, "updated")
})
