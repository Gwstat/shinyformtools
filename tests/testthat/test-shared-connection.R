two_forms <- function(db_a, db_b = db_a) {
  list(
    a = form(form_id = "a", table_name = "a", db = db_a, fields = list(form_field(id = "x", label = "X"))),
    b = form(form_id = "b", table_name = "b", db = db_b, fields = list(form_field(id = "y", label = "Y")))
  )
}

page_server <- function(forms) {
  function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      list(
        a = form_server("a", forms$a),
        b = form_server("b", forms$b),
        c = form_server("c", forms$a)
      )
    })
  }
}

test_that("modules of one session share one connection per database", {
  forms <- two_forms(db_sqlite(tempfile(fileext = ".sqlite")))
  forms$a <- form(form_id = "a", table_name = "a", db = forms$a$db,
                  fields = list(form_field(id = "x", label = "X"), form_field(id = "n", label = "N", input_type = "numericInput")))
  handle <- NULL

  shiny::testServer(page_server(forms), {
    r <- session$getReturned()
    expect_true(DBI::dbIsValid(r$a$conn))
    expect_identical(r$a$conn, r$b$conn)
    expect_identical(r$a$connection(), r$b$connection())
    expect_length(ls(session$userData$sft_connections, all.names = TRUE), 1L)
    handle <<- r$a$conn
    # Both forms work on it.
    insert_record(forms$a, list(x = "1"), conn = r$a$connection())
    insert_record(forms$b, list(y = "2"), conn = r$b$connection())
  })

  # Closed when the session ends.
  expect_false(DBI::dbIsValid(handle))
})

test_that("modules of different databases get their own connections", {
  forms <- two_forms(db_sqlite(tempfile(fileext = ".sqlite")), db_sqlite(tempfile(fileext = ".sqlite")))
  forms$a <- form(form_id = "a", table_name = "a", db = forms$a$db,
                  fields = list(form_field(id = "x", label = "X"), form_field(id = "n", label = "N", input_type = "numericInput")))

  shiny::testServer(page_server(forms), {
    r <- session$getReturned()
    expect_false(identical(r$a$conn, r$b$conn))
    expect_length(ls(session$userData$sft_connections, all.names = TRUE), 2L)
  })
})

test_that("share_connections = FALSE gives every module its own connection", {
  withr::local_options(shinyformtools.share_connections = FALSE)
  forms <- two_forms(db_sqlite(tempfile(fileext = ".sqlite")))
  forms$a <- form(form_id = "a", table_name = "a", db = forms$a$db,
                  fields = list(form_field(id = "x", label = "X"), form_field(id = "n", label = "N", input_type = "numericInput")))

  shiny::testServer(page_server(forms), {
    r <- session$getReturned()
    expect_false(identical(r$a$conn, r$b$conn))
    expect_null(session$userData$sft_connections)
  })
})
