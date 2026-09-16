# The 0.2 argument clean-up: form_server() takes four option bundles, the schema
# functions take (form, conn), and a few arguments were renamed. Every old
# spelling still works for one release and warns once per session.

sft_test_deprecation_form <- function() {
  form(
    form_id = "deprecation",
    table_name = "deprecation",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(form_field(id = "name", label = "Name"))
  )
}

test_that("a deprecation warning fires once per session per argument", {
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  expect_warning(sft_deprecate_warn("`old`", "`new`"), "old.*deprecated.*new")
  expect_no_warning(sft_deprecate_warn("`old`", "`new`"))
  expect_warning(sft_deprecate_warn("`other`", "`new`"), "other")
})

test_that("deprecated flat form_server arguments land in their bundles", {
  skip_if_not_installed("DT")
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  contacts <- sft_test_deprecation_form()

  # One warning per deprecated argument, each naming its new home.
  warnings <- capture_warnings(
    shiny::testServer(
      form_server,
      args = list(
        id = "deprecation",
        form = contacts,
        can_add = FALSE,
        table_filter = "top",
        table_columns = "name",
        highlight_color = "#123456"
      ),
      {
        expect_false(can_add)
        expect_identical(table_filter, "top")
        expect_identical(table_columns, "name")
        expect_identical(highlight_color, "#123456")
        # Untouched settings keep their defaults.
        expect_true(can_edit)
        expect_identical(default_column_view, "Standard")
      }
    )
  )

  expect_length(warnings, 4L)
  expect_match(warnings, "deprecated", all = TRUE)
  expect_true(any(grepl("permissions = list\\(can_add = \\)", warnings)))
  expect_true(any(grepl("table = list\\(filter = \\)", warnings)))
  expect_true(any(grepl("columns = list\\(visible = \\)", warnings)))
  expect_true(any(grepl("highlight = list\\(color = \\)", warnings)))
})

test_that("an explicit bundle entry wins over a deprecated argument", {
  skip_if_not_installed("DT")
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  contacts <- sft_test_deprecation_form()

  suppressWarnings(
    shiny::testServer(
      form_server,
      args = list(
        id = "deprecation",
        form = contacts,
        permissions = list(can_add = TRUE),
        can_add = FALSE
      ),
      expect_true(can_add)
    )
  )
})

test_that("a misspelt bundle key or an unknown argument is an error, not a no-op", {
  contacts <- sft_test_deprecation_form()

  expect_error(
    form_server("x", contacts, permissions = list(can_ad = FALSE)),
    "Unknown permissions entry: can_ad"
  )
  expect_error(
    form_server("x", contacts, table = list(clas = "x")),
    "Unknown table entry"
  )
  expect_error(
    form_server("x", contacts, bogus = 1),
    "unused argument in form_server\\(\\): bogus"
  )
  expect_error(
    form_server("x", contacts, permissions = "yes"),
    "permissions must be a named list"
  )
})

test_that("permissions$user supplies the user when the argument is NULL", {
  skip_if_not_installed("DT")
  contacts <- sft_test_deprecation_form()

  shiny::testServer(
    form_server,
    args = list(
      id = "deprecation",
      form = contacts,
      permissions = list(user = "alice", can_delete = FALSE)
    ),
    {
      expect_identical(user, "alice")
      expect_false(can_delete)
    }
  )

  shiny::testServer(
    form_server,
    args = list(
      id = "deprecation",
      form = contacts,
      user = "bob",
      permissions = list(user = "alice")
    ),
    expect_identical(user, "bob")
  )
})

test_that("form_server picks the layout up from the UI's hidden input", {
  skip_if_not_installed("DT")
  contacts <- sft_test_deprecation_form()

  html <- as.character(form_ui("deprecation", form_layout = "inline"))
  expect_match(html, "deprecation-sft_form_layout")
  expect_match(html, 'value="inline"')

  shiny::testServer(
    form_server,
    args = list(id = "deprecation", form = contacts),
    {
      session$flushReact()
      session$setInputs(sft_form_layout = "inline")
      session$setInputs(open_add = 1L)
      expect_identical(inline_active(), "add")
    }
  )

  # Without the marker (or with "modal") the add flow opens the dialog.
  shiny::testServer(
    form_server,
    args = list(id = "deprecation", form = contacts),
    {
      session$flushReact()
      session$setInputs(open_add = 1L)
      expect_null(inline_active())
    }
  )
})

test_that("form_ui accepts the two legacy flags with a warning", {
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  expect_warning(
    html <- as.character(form_ui("x", show_include_deleted = TRUE)),
    "show_include_deleted.*deprecated"
  )
  expect_match(html, "x-include_deleted")

  expect_warning(form_ui("x", show_versions = TRUE), "show_versions.*no effect")
  expect_error(form_ui("x", show_bogus = TRUE), "unused argument")
})

test_that("the schema functions accept the old (conn, form) order with a warning", {
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  contacts <- sft_test_deprecation_form()
  conn <- local_test_conn()

  expect_warning(
    plan <- plan_migration(conn, contacts),
    "plan_migration\\(conn, form\\).*deprecated.*plan_migration\\(form, conn\\)"
  )
  expect_s3_class(plan, "sft_migration_plan")

  expect_warning(inspection <- inspect_schema(conn, contacts), "inspect_schema")
  expect_false(inspection$table_exists)

  expect_warning(apply_migration(conn, contacts), "apply_migration")
  expect_true(inspect_schema(contacts, conn)$table_exists)

  # The new order warns about nothing.
  expect_no_warning(plan_migration(contacts, conn))
})

test_that("validate_record, shinymanager_users and the IBAN inputs keep their old names", {
  sft_reset_deprecation_warnings()
  withr::defer(sft_reset_deprecation_warnings())

  contacts <- sft_test_deprecation_form()
  conn <- local_test_conn()
  init_db(contacts, conn = conn)
  rec <- insert_record(contacts, list(name = "Ada"), conn = conn)

  expect_warning(
    validate_record(contacts, list(name = "Ada"), conn = conn, current_id = rec$sft_id[1]),
    "current_id.*record_id"
  )
  expect_no_warning(
    validate_record(contacts, list(name = "Ada"), conn = conn, record_id = rec$sft_id[1])
  )

  creds <- data.frame(user = c("b", "a"), stringsAsFactors = FALSE)
  expect_warning(users <- shinymanager_users(db = creds), "credentials")
  expect_identical(users, c("a", "b"))
  expect_identical(shinymanager_users(creds), c("a", "b"))

  expect_warning(old <- IBANInput("iban", "IBAN"), "ibanInput")
  expect_identical(as.character(old), as.character(ibanInput("iban", "IBAN")))
})
