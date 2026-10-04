# The helpers of the extension API, tested here on their own: shinygridtools
# and shinycarttools use them, but a break should show in this package.

helper_form <- function(db = db_sqlite(tempfile(fileext = ".sqlite"))) {
  form(
    form_id = "people", table_name = "people", db = db,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "team", label = "Team"),
      form_field(id = "tags", label = "Tags", input_type = "checkboxGroupInput",
                 args = list(choices = c("a", "b", "c"))),
      form_field(id = "salary", label = "Salary", input_type = "numericInput",
                 editable = function(user) identical(user, "admin"))
    )
  )
}

test_that("form helpers find fields, read values and encode them as stored", {
  f <- helper_form()
  expect_identical(vapply(form_fields(f), function(x) x$id, ""), c("name", "team", "tags", "salary"))
  expect_identical(find_field(f, "team")$label, "Team")
  expect_null(find_field(f, "nope"))
  expect_identical(vapply(key_fields(f, c("team", "name")), function(x) x$id, ""), c("team", "name"))
  expect_error(key_fields(f, c("name", "nope")))

  expect_identical(record_value(list(name = "Ada"), find_field(f, "name")), "Ada")
  expect_identical(record_value(data.frame(team = "Blue"), find_field(f, "team")), "Blue")
  expect_null(record_value(list(name = "Ada"), find_field(f, "team")))
  expect_identical(field_db_value(find_field(f, "tags"), c("a", "c")), "[\"a\",\"c\"]")

  expect_false(isTRUE(find_field(resolve_editable(f, "ada"), "salary")$editable))
  expect_true(isTRUE(find_field(resolve_editable(f, "admin"), "salary")$editable))
})

test_that("module_connection shares one connection per session and closes it", {
  f <- helper_form()
  given <- db_connect(f$db)
  on.exit(db_disconnect(given), add = TRUE)
  expect_identical(module_connection(NULL, f$db, conn = given)(), given)

  session <- shiny::MockShinySession$new()
  first <- module_connection(session, f$db)
  second <- module_connection(session, f$db)
  conn <- first()
  expect_true(DBI::dbIsValid(conn))
  expect_identical(second(), conn)
  expect_identical(first(probe = FALSE), conn)
  session$close()
  expect_false(DBI::dbIsValid(conn))
})

test_that("prepare_write brings the schema up to date before an own write", {
  f <- helper_form()
  conn <- local_test_conn(f$db)
  expect_false(table_exists(conn, "people"))
  expect_identical(prepare_write(f, conn), conn)
  expect_true(table_exists(conn, "people"))
})

test_that("after_transaction runs after a rollback, after_commit only after a commit", {
  f <- helper_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  ran <- character()
  try(with_transaction(conn, {
    after_commit(conn, function() ran <<- c(ran, "commit"))
    after_transaction(conn, function() ran <<- c(ran, "any"))
    stop("give up")
  }), silent = TRUE)
  expect_identical(ran, "any")

  ran <- character()
  with_transaction(conn, {
    after_commit(conn, function() ran <<- c(ran, "commit"))
    after_transaction(conn, function() ran <<- c(ran, "any"))
  })
  expect_setequal(ran, c("commit", "any"))
})

test_that("module permissions: values, functions, failures and editable fields", {
  expect_true(module_permission(list(), "can_edit"))
  expect_false(module_permission(list(can_edit = FALSE), "can_edit"))
  expect_true(module_permission(list(can_edit = function() TRUE), "can_edit"))
  # A permission function that fails denies instead of ending the session.
  expect_false(module_permission(list(can_edit = function() stop("boom")), "can_edit"))

  expect_null(module_editable_fields(list()))
  expect_identical(module_editable_fields(list(editable_fields = "name")), "name")
  expect_identical(module_editable_fields(list(editable_fields = function() c("name", "team"))), c("name", "team"))
})

test_that("apply_edit_rules recomputes derived fields and drops hidden ones", {
  f <- form(
    form_id = "items", table_name = "items", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "length", label = "Length", input_type = "numericInput", editable = FALSE),
      form_field(id = "note", label = "Note")
    )
  )
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "ab", length = 2, note = "x"), conn = conn)
  state_with <- function(bindings) {
    list(form = f, input_bindings = bindings, conn = function() conn,
         current_user = function() "u", display_context = function() list(form = f))
  }

  length_of_name <- dynamic_value("length", depends_on = "name", value = function(values) nchar(values$name))
  out <- apply_edit_rules(state_with(list(length_of_name)), 1L, list(name = "abcd"))
  expect_identical(out$values$length, 4L)
  # The locked derived field may be written for this edit.
  expect_true(isTRUE(find_field(out$form, "length")$editable))

  hide_note <- dynamic_visibility("note", depends_on = "name", visible = function(values) FALSE)
  out <- apply_edit_rules(state_with(list(hide_note)), 1L, list(name = "ab", note = "y"))
  expect_false("note" %in% names(out$values))

  # Without bindings the values pass through untouched.
  expect_identical(apply_edit_rules(state_with(list()), 1L, list(note = "z"))$values, list(note = "z"))
})

test_that("ui_labels layers overrides over the active language", {
  expect_identical(ui_labels(list(save = "OK"))$save, "OK")
  expect_identical(with_language(german(), ui_labels()$save), german_labels()$save)
})

test_that("the audit table lists the changed form fields, not the system columns", {
  f <- helper_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "Ada", team = "Blue"), conn = conn)
  update_record(f, list(team = "Red"), record_id = 1L, conn = conn)
  shown <- audit_datatable(fetch_audit_log(f, conn = conn))
  changed <- shown$x$data[[sft_table_labels()$audit_changed_fields]]
  expect_false(any(grepl("sft_|\\[", changed)))
  expect_true("team" %in% changed)
  expect_true(isTRUE(shown$x$options$scrollX))
})

test_that("form_from_excel reads the same form as form_from_metadata", {
  skip_if_not_installed("openxlsx")
  inputs <- data.frame(
    Id = c("name", "topics"), label = c("Name", "Topics"),
    inputObj = c("textInput", "checkboxGroupInput"),
    args_json = c("{}", '{"choices":["A","B"]}'), mandatory = c(TRUE, FALSE)
  )
  settings <- data.frame(form_id = "from_excel", form_name = "From Excel", table_name = "from_excel")
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Inputs = inputs, Einstellungen = settings), path)

  f <- form_from_excel(path)
  expect_identical(f$form_id, "from_excel")
  expect_identical(vapply(f$fields, function(x) x$id, ""), c("name", "topics"))
  expect_identical(f$fields[[2]]$args$choices, c("A", "B"))
  expect_error(form_from_excel(path, inputs_sheet = "Nope"), "Sheet not found")
})

test_that("updateIbanInput sends the IBAN formatted", {
  sent <- NULL
  local_mocked_bindings(updateTextInput = function(session, inputId, label = NULL, value = NULL, ...) {
    sent <<- list(id = inputId, value = value)
  }, .package = "shiny")
  updateIbanInput(NULL, "iban", value = "de89370400440532013000")
  expect_identical(sent$id, "iban")
  expect_match(sent$value, "^DE89 3704 0044")
})
