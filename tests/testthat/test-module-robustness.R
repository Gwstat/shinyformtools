# The module keeps the session alive and refuses what the user cannot see.

robust_form <- function() {
  form(form_id = "robust", table_name = "robust", db = db_sqlite(tempfile(fileext = ".sqlite")),
       fields = list(form_field(id = "name", label = "Name"),
                     form_field(id = "note", label = "Note")))
}

test_that("a hidden records table gives no record away by row index", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "One", note = "secret"), conn = conn)
  local_mocked_bindings(showModal = function(...) NULL, .package = "shiny")

  shiny::testServer(form_server, args = list(
    id = "m", form = f, conn = conn, user = "bob",
    permissions = list(can_view_table = FALSE), columns = list(persist = FALSE)
  ), {
    session$flushReact()
    session$setInputs(records_rows_selected = 1L)
    expect_null(session$returned$selected_record())
    session$setInputs(open_edit = 1)
    expect_null(state$current_edit_row())
    session$setInputs(delete = 1)
    session$setInputs(confirm_delete = 1)
  })
  expect_identical(nrow(fetch_records(f, conn = conn)), 1L)
})

test_that("a permission function that fails denies instead of ending the session", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  shown <- character()
  local_mocked_bindings(showNotification = function(ui, ...) {
    shown <<- c(shown, as.character(ui))
    invisible(NULL)
  }, .package = "shiny")
  broken <- function() stop("rights table unreachable")

  for (hide in c(TRUE, FALSE)) {
    shiny::testServer(form_server, args = list(
      id = "m", form = f, conn = conn, user = "bob", columns = list(persist = FALSE),
      permissions = list(can_add = broken, can_edit = broken, hide_forbidden = hide)
    ), {
      session$flushReact()
      session$setInputs(add_name = "x", add_note = "y")
      session$setInputs(submit_add = 1)
      session$flushReact()
      expect_false(session$isEnded())
    })
  }
  expect_identical(nrow(fetch_records(f, conn = conn)), 0L)
  expect_true(any(grepl("rights table unreachable", shown)))
})

test_that("the records table renders again once the database is back", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "Ada"), conn = conn)
  db_state <- new.env()
  db_state$down <- TRUE
  real_fetch <- fetch_records
  local_mocked_bindings(fetch_records = function(...) {
    if (db_state$down) stop("Failed to connect: Too many connections")
    real_fetch(...)
  })
  local_mocked_bindings(replaceData = function(...) NULL, .package = "DT")
  local_mocked_bindings(showNotification = function(...) NULL, .package = "shiny")

  shiny::testServer(form_server, args = list(
    id = "m", form = f, conn = conn, user = "bob", columns = list(persist = FALSE)
  ), {
    session$flushReact()
    expect_error(output$records, "Too many connections")
    db_state$down <- FALSE
    session$returned$refresh()
    session$flushReact()
    expect_no_error(output$records)
    expect_match(output$records, "<table")
  })
})

test_that("with refresh_delay a row index means the row the browser shows", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  for (name in c("A", "B", "C")) insert_record(f, list(name = name), conn = conn)
  local_mocked_bindings(replaceData = function(...) NULL, .package = "DT")
  local_mocked_bindings(showNotification = function(...) NULL, .package = "shiny")

  shiny::testServer(form_server, args = list(
    id = "m", form = f, conn = conn, user = "bob", columns = list(persist = FALSE),
    table = list(refresh_delay = 1000)
  ), {
    session$flushReact()
    output$records
    session$elapse(1100)
    session$flushReact()
    # A first refresh shows at once and opens the throttle window.
    session$returned$refresh()
    session$flushReact()
    # Another session deletes B; this refresh falls into the window.
    soft_delete_record(f, record_id = 2L, conn = conn)
    session$returned$refresh()
    session$flushReact()
    # The browser still shows A, B, C: row 2 is B, which is gone.
    session$setInputs(records_rows_selected = 2L)
    expect_null(session$returned$selected_record())
    session$setInputs(records_rows_selected = 3L)
    expect_identical(session$returned$selected_record()$name, "C")
    session$elapse(1100)
    session$flushReact()
    # Now the browser shows A, C.
    session$setInputs(records_rows_selected = 2L)
    expect_identical(session$returned$selected_record()$name, "C")
  })
})

test_that("after a save the closed dialog's version table is not rebuilt", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "A"), conn = conn)
  local_mocked_bindings(showModal = function(...) NULL, removeModal = function(...) NULL,
                        showNotification = function(...) NULL, .package = "shiny")
  local_mocked_bindings(replaceData = function(...) NULL, .package = "DT")
  real_versions <- list_restorable_versions
  calls <- 0L
  local_mocked_bindings(list_restorable_versions = function(...) {
    calls <<- calls + 1L
    real_versions(...)
  })

  shiny::testServer(form_server, args = list(
    id = "m", form = f, conn = conn, user = "bob", columns = list(persist = FALSE)
  ), {
    session$flushReact()
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_edit = 1)
    expect_gt(nrow(state$restore_versions()), 0L)
    calls <<- 0L
    session$setInputs(edit_name = "B", edit_note = "")
    session$setInputs(submit_edit = 1)
    session$flushReact()
    state$restore_versions()
    expect_identical(fetch_records(f, conn = conn)$name, "B")
  })
  expect_identical(calls, 0L)
})

test_that("saved moves only on the module's own writes, so two forms can follow each other", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  local_mocked_bindings(showModal = function(...) NULL, removeModal = function(...) NULL,
                        showNotification = function(...) NULL, .package = "shiny")
  local_mocked_bindings(replaceData = function(...) NULL, .package = "DT")
  refreshes <- 0L

  shiny::testServer(function(input, output, session) {
    a <- form_server("a", f, conn = conn, columns = list(persist = FALSE),
                     refresh_triggers = function() b$saved())
    b <- form_server("b", f, conn = conn, columns = list(persist = FALSE),
                     refresh_triggers = function() a$saved())
    shiny::observeEvent(b$changed(), refreshes <<- refreshes + 1L, ignoreInit = TRUE)
    session$userData$a <- a
  }, {
    session$flushReact()
    expect_identical(session$userData$a$saved(), 0L)
    session$userData$a$refresh()
    session$flushReact()
    expect_identical(session$userData$a$saved(), 0L)
    session$setInputs(`a-add_name` = "x", `a-add_note` = "")
    session$setInputs(`a-submit_add` = 1)
    session$flushReact()
    expect_identical(session$userData$a$saved(), 1L)
  })
  expect_identical(refreshes, 1L)
  expect_identical(nrow(fetch_records(f, conn = conn)), 1L)
})

test_that("a table refresh names the columns in the module's language", {
  f <- robust_form()
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(name = "A"), conn = conn)
  sent <- NULL
  local_mocked_bindings(replaceData = function(proxy, data, ...) sent <<- data, .package = "DT")

  shiny::testServer(form_server, args = list(
    id = "m", form = f, conn = conn, language = german(), columns = list(persist = FALSE)
  ), {
    session$flushReact()
    output$records
    session$returned$refresh()
    session$flushReact()
  })
  expect_true("Letzte Bearbeitung" %in% names(sent))
})
