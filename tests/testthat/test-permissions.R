# shinymanager_permissions() turns a shinymanager-style auth object into the
# can_* functions that form_server() expects, so several tables can share one
# login without hand-writing a closure per permission.

testthat::test_that("sft_truthy coerces shinymanager-style character flags", {
  testthat::expect_true(sft_truthy("TRUE"))
  testthat::expect_true(sft_truthy("true"))
  testthat::expect_true(sft_truthy("1"))
  testthat::expect_true(sft_truthy(TRUE))
  testthat::expect_false(sft_truthy("FALSE"))
  testthat::expect_false(sft_truthy(""))
  testthat::expect_false(sft_truthy(NA))
  testthat::expect_false(sft_truthy(NULL))
  testthat::expect_true(sft_truthy(NULL, default = TRUE))
})

testthat::test_that("permissions list matches the form_server arguments", {
  auth <- list(user = "editor", can_add = "TRUE")
  perms <- shinymanager_permissions(auth)

  server_args <- names(formals(form_server))
  testthat::expect_true(all(names(perms) %in% server_args))
  testthat::expect_setequal(
    setdiff(names(perms), "user"),
    sft_permission_fields()
  )
})

testthat::test_that("each permission reads its credential column truthily", {
  auth <- list(
    user = "editor",
    can_add = "TRUE",
    can_edit = "TRUE",
    can_delete = "FALSE",
    can_view_audit = "FALSE"
  )
  perms <- shinymanager_permissions(auth)

  testthat::expect_identical(perms$user(), "editor")
  testthat::expect_true(perms$can_add())
  testthat::expect_true(perms$can_edit())
  testthat::expect_false(perms$can_delete())
  testthat::expect_false(perms$can_view_audit())
  # A column the credentials do not define falls back to `default` (deny).
  testthat::expect_false(perms$can_restore())
})

testthat::test_that("default and mapping change how columns are resolved", {
  auth <- list(user = "admin", can_edit_locations = "TRUE")

  permissive <- shinymanager_permissions(auth, default = TRUE)
  testthat::expect_true(permissive$can_restore()) # absent column -> default TRUE

  mapped <- shinymanager_permissions(
    auth,
    mapping = c(can_edit = "can_edit_locations")
  )
  testthat::expect_true(mapped$can_edit()) # reads the remapped column
  testthat::expect_false(mapped$can_add()) # still reads the absent default column
})

testthat::test_that("permissions track a reactive auth object", {
  auth <- shiny::reactiveValues(user = "viewer", can_add = "FALSE")
  perms <- shinymanager_permissions(auth)

  shiny::isolate(testthat::expect_false(perms$can_add()))

  shiny::isolate(auth$can_add <- "TRUE")
  shiny::isolate(testthat::expect_true(perms$can_add()))
})

testthat::test_that("user falls back to user_default when the column is empty", {
  perms <- shinymanager_permissions(
    list(user = ""),
    user_default = "__anon__"
  )
  testthat::expect_identical(perms$user(), "__anon__")
})

testthat::test_that("editable_fields locks other inputs without mutating the form", {
  f <- form(
    form_id = "ef", table_name = "ef", db_path = tempfile(fileext = ".sqlite"),
    fields = list(form_field(id = "a", label = "A"), form_field(id = "b", label = "B"))
  )

  one <- as.character(render_form_fields(f, editable_fields = "a"))
  all <- as.character(render_form_fields(f))
  count <- function(s) lengths(regmatches(s, gregexpr("disabled", s)))

  testthat::expect_equal(count(one), 1L) # field b locked
  testthat::expect_equal(count(all), 0L) # nothing locked by default
  # The caller's form object is not mutated by the lock.
  testthat::expect_true(isTRUE(f$fields[[2L]]$editable))
})

testthat::test_that("form_server initializes with the view/reset gates off", {
  db_path <- tempfile(fileext = ".sqlite")
  f <- form(form_id = "vt", table_name = "vt", db_path = db_path,
            fields = list(form_field(id = "name", label = "Name")))
  conn <- db_connect(db_path)
  init_db(f, conn = conn)
  insert_record(f, list(name = "x"), conn = conn)
  on.exit(db_disconnect(conn), add = TRUE)

  # can_view_table / can_reset_table follow the same can_* + hide_forbidden path
  # as the other permissions (the records output and reset button are hidden);
  # here we assert the module wires up cleanly with them denied.
  app <- function(input, output, session) {
    form_server("f", form = f, conn = conn,
                can_view_table = FALSE, can_reset_table = FALSE)
  }
  shiny::testServer(app, {
    testthat_silent <- testthat::expect_silent(session$flushReact())
  })
})

testthat::test_that("sft_form_server accepts reactive can_* and hide_forbidden", {
  db_path <- tempfile(fileext = ".sqlite")
  form <- form(
    form_id = "perm_form",
    table_name = "perm_form",
    db_path = db_path,
    fields = list(form_field(id = "name", label = "Name"))
  )
  conn <- db_connect(db_path)
  init_db(form, conn = conn)
  insert_record(form, list(name = "x"), conn = conn)
  on.exit(db_disconnect(conn), add = TRUE)

  auth <- shiny::reactiveValues(user = "viewer", can_add = "FALSE", can_view_audit = "FALSE")
  perms <- shinymanager_permissions(auth)

  app <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      do.call(
        form_server,
        c(list(id = "f", form = form, conn = conn, hide_forbidden = TRUE), perms)
      )
    })
  }

  shiny::testServer(app, args = list(id = "wrap"), {
    session$flushReact()
    # The module wires up and the visibility observer runs without error even
    # though add and audit are denied for this user.
    testthat::expect_silent(session$flushReact())
  })
})
