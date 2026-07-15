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

testthat::test_that("sft_static_locked_input_defaults maps locked fields to declared defaults", {
  f <- form(
    form_id = "locked_defaults",
    db_path = tempfile(fileext = ".sqlite"),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "role", label = "Role", editable = FALSE, args = list(value = "user")),
      form_field(id = "note", label = "Note", editable = FALSE),
      form_field(id = "flag", label = "Flag", editable = function(user) TRUE)
    )
  )

  defaults <- sft_static_locked_input_defaults(f)

  # Only statically locked fields appear; function-locked fields are handled
  # per user by sft_user_locked_input_fields.
  testthat::expect_setequal(names(defaults), c("role", "note"))
  testthat::expect_identical(defaults$role, "user")
  testthat::expect_null(defaults$note)
})

testthat::test_that("add ignores client values for statically non-editable fields", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  f <- form(
    form_id = "locked_add", table_name = "locked_add", db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "role", label = "Role", editable = FALSE, args = list(value = "user"))
    )
  )
  init_db(f, conn = conn)

  shiny::testServer(
    form_server,
    args = list(form = f, conn = conn),
    {
      # The role input renders disabled, but that is client-side only: simulate
      # a tampered client submitting a value for it anyway.
      session$setInputs(add_name = "Ada", add_role = "admin")
      session$setInputs(submit_add = 1L)
    }
  )

  records <- fetch_records(f, conn = conn)
  testthat::expect_identical(records$role[1], "user")
})

testthat::test_that("derived non-editable fields are recomputed server-side on add", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  f <- form(
    form_id = "derived_add", table_name = "derived_add", db_path = db_path,
    fields = list(
      form_field(id = "city", label = "City"),
      # The app_cascading_inputs pattern: a locked field whose value is derived
      # from another input by a dynamic_value binding.
      form_field(id = "zip", label = "ZIP", editable = FALSE)
    )
  )
  init_db(f, conn = conn)

  binding <- dynamic_value(
    field = "zip",
    depends_on = "city",
    value = function(values) if (identical(values$city, "Berlin")) "10115" else ""
  )

  shiny::testServer(
    form_server,
    args = list(form = f, conn = conn, input_bindings = list(binding)),
    {
      # A tampered client submits its own zip; the server recomputes it from
      # the binding instead.
      session$setInputs(add_city = "Berlin", add_zip = "99999")
      session$setInputs(submit_add = 1L)
    }
  )

  records <- fetch_records(f, conn = conn)
  testthat::expect_identical(records$zip[1], "10115")
})
