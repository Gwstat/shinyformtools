# The form module exposes a `changed` signal and accepts `refresh_triggers`, so
# dependent tables and downstream outputs (e.g. a map) re-fetch when an upstream
# table changes. Driven with testServer over two wired-up form modules.

make_reactivity_form <- function(id) {
  db_path <- tempfile(fileext = ".sqlite")
  form <- form(
    form_id = id,
    table_name = id,
    db_path = db_path,
    fields = list(form_field(id = "name", label = "Name"))
  )
  conn <- db_connect(db_path)
  init_db(form, conn = conn)
  insert_record(form, list(name = "x"), conn = conn)
  list(form = form, conn = conn)
}

testthat::test_that("a form re-fetches when an upstream form it depends on changes", {
  upstream_tbl <- make_reactivity_form("up_tbl")
  downstream_tbl <- make_reactivity_form("down_tbl")
  on.exit(
    {
      db_disconnect(upstream_tbl$conn)
      db_disconnect(downstream_tbl$conn)
    },
    add = TRUE
  )

  app <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      up <- form_server("up", form = upstream_tbl$form, conn = upstream_tbl$conn)
      down <- form_server(
        "down",
        form = downstream_tbl$form,
        conn = downstream_tbl$conn,
        refresh_triggers = list(up$changed)
      )
      list(up = up, down = down)
    })
  }

  shiny::testServer(app, args = list(id = "wrap"), {
    session$flushReact() # startup flush, as a real session does

    before <- down$changed()

    # A change in the upstream table bumps its `changed` signal, which the
    # downstream table depends on via refresh_triggers.
    up$refresh()
    session$flushReact()

    testthat::expect_gt(down$changed(), before)
  })
})
