# A rights table modelled as a shinyformtools form: one rule grants permissions
# to a user across one or more forms (multi-select, stored as a JSON array).
# rights_permissions() resolves those rules into form_server()'s can_* functions.

testthat::test_that("permissions_form has user, forms and one field per permission", {
  pf <- permissions_form(form_ids = c("a", "b"), db_path = tempfile(fileext = ".sqlite"))
  ids <- vapply(pf$fields, function(f) f$id, character(1))

  testthat::expect_true(all(c("user", "forms") %in% ids))
  testthat::expect_true(all(sft_permission_fields() %in% ids))

  forms_field <- Filter(function(f) identical(f$id, "forms"), pf$fields)[[1L]]
  testthat::expect_true(isTRUE(forms_field$args$multiple))
})

testthat::test_that("a multi-form rule resolves for its forms and only its user", {
  db_path <- tempfile(fileext = ".sqlite")
  pf <- permissions_form(form_ids = c("locations", "people", "tasks"), db_path = db_path)
  conn <- db_connect(db_path)
  init_db(pf, conn = conn)
  on.exit(db_disconnect(conn), add = TRUE)

  insert_record(
    pf,
    list(user = "editor", forms = c("locations", "tasks"), can_edit = TRUE, can_add = TRUE),
    conn = conn, user = "tester"
  )
  rules <- fetch_records(pf, conn = conn)

  loc <- rights_permissions(rules, user = "editor", form_id = "locations")
  testthat::expect_true(loc$can_edit())
  testthat::expect_true(loc$can_add())
  testthat::expect_false(loc$can_delete()) # not granted -> default deny
  testthat::expect_identical(loc$user(), "editor")

  # A form not listed in the rule gets nothing.
  ppl <- rights_permissions(rules, user = "editor", form_id = "people")
  testthat::expect_false(ppl$can_edit())

  # A different user gets nothing either.
  other <- rights_permissions(rules, user = "viewer", form_id = "locations")
  testthat::expect_false(other$can_edit())
})

testthat::test_that("permissions OR across multiple rules for the same user", {
  db_path <- tempfile(fileext = ".sqlite")
  pf <- permissions_form(form_ids = c("locations", "people"), db_path = db_path)
  conn <- db_connect(db_path)
  init_db(pf, conn = conn)
  on.exit(db_disconnect(conn), add = TRUE)

  # One rule grants edit on locations; a single-form rule grants view on people.
  insert_record(pf, list(user = "u", forms = "locations", can_edit = TRUE),
                conn = conn, user = "tester")
  insert_record(pf, list(user = "u", forms = "people", can_view_record = TRUE),
                conn = conn, user = "tester")
  rules <- fetch_records(pf, conn = conn)

  testthat::expect_true(rights_permissions(rules, "u", "locations")$can_edit())
  testthat::expect_false(rights_permissions(rules, "u", "locations")$can_view_record())
  testthat::expect_true(rights_permissions(rules, "u", "people")$can_view_record())
})

testthat::test_that("superuser grants everything and rules may be reactive", {
  rules_fn <- function() data.frame(
    user = "u", forms = "locations", can_edit = 1L,
    sft_is_deleted = 0L, stringsAsFactors = FALSE
  )

  # superuser overrides the (empty for this form) rules
  su <- rights_permissions(rules_fn, user = "admin", form_id = "people", superuser = TRUE)
  testthat::expect_true(su$can_delete())

  # function/reactive rules are read each call
  live <- rights_permissions(rules_fn, user = "u", form_id = "locations")
  testthat::expect_true(live$can_edit())
})

testthat::test_that("deleted rules are ignored", {
  rules <- data.frame(
    user = c("u", "u"),
    forms = c("locations", "locations"),
    can_edit = c(1L, 1L),
    sft_is_deleted = c(1L, 0L), # first rule soft-deleted
    stringsAsFactors = FALSE
  )
  # Only the live rule counts; flip it and edit should drop.
  testthat::expect_true(rights_permissions(rules, "u", "locations")$can_edit())

  rules$sft_is_deleted <- c(1L, 1L)
  testthat::expect_false(rights_permissions(rules, "u", "locations")$can_edit())
})

testthat::test_that("permissions list matches form_server arguments", {
  perms <- rights_permissions(data.frame(), user = "u", form_id = "x")
  testthat::expect_true(all(names(perms) %in% names(formals(form_server))))
})

testthat::test_that("shinymanager_users reads a credentials data frame", {
  creds <- data.frame(user = c("b", "a", "a"), password = c("1", "2", "3"),
                      stringsAsFactors = FALSE)
  testthat::expect_identical(shinymanager_users(creds), c("a", "b"))
})
