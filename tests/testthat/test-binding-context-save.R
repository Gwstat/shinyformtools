# Input-binding handlers get the context ?hook_context describes also when the
# server re-evaluates them on save, not only in the dialog.

ctx_form <- function(extra = list()) {
  form(form_id = "ctx", table_name = "ctx", db = db_sqlite(tempfile(fileext = ".sqlite")),
       fields = c(list(form_field(id = "name", label = "Name", mandatory = TRUE),
                       form_field(id = "city", label = "City")), extra))
}

ctx_add <- function(f, bindings, inputs) {
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  shiny::testServer(form_server, args = list(
    id = "a", form = f, conn = conn, user = "hooker", input_bindings = bindings,
    columns = list(persist = FALSE)
  ), {
    session$flushReact()
    session$setInputs(open_add = 1)
    do.call(session$setInputs, inputs)
    session$setInputs(submit_add = 1)
  })
  fetch_records(f, conn = conn)
}

test_that("a visibility predicate reading context on save sees the user and records", {
  local_mocked_bindings(showNotification = function(...) NULL, showModal = function(...) NULL,
                        removeModal = function(...) NULL, .package = "shiny")
  f <- ctx_form()
  stored <- ctx_add(f, list(dynamic_visibility(
    "city", visible = function(values, context) { context$records(); identical(context$user, "hooker") },
    depends_on = "name"
  )), list(add_name = "Ada", add_city = "Berlin"))
  expect_identical(stored$city, "Berlin")
})

test_that("a locked derived value reading context on save uses the user", {
  local_mocked_bindings(showNotification = function(...) NULL, showModal = function(...) NULL,
                        removeModal = function(...) NULL, .package = "shiny")
  f <- ctx_form(list(form_field(id = "code", label = "Code", editable = FALSE)))
  stored <- ctx_add(f, list(dynamic_value(
    "code", value = function(values, context) paste0(toupper(values$name), "-", context$user),
    depends_on = "name"
  )), list(add_name = "Ada", add_city = "B", add_code = "ADA-hooker"))
  expect_identical(stored$code, "ADA-hooker")
})
