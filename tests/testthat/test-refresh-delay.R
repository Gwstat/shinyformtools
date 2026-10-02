test_that("table refresh_delay collects refreshes into one per window", {
  f <- form(form_id = "rd", table_name = "rd", db = db_sqlite(tempfile(fileext = ".sqlite")),
            fields = list(form_field(id = "name", label = "Name")))
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)

  replaced <- 0L
  local_mocked_bindings(replaceData = function(proxy, data, ...) replaced <<- replaced + 1L, .package = "DT")

  run <- function(table) {
    replaced <<- 0L
    out <- NULL
    shiny::testServer(
      form_server,
      args = list(id = "m", form = f, conn = conn, columns = list(persist = FALSE), table = table),
      {
        session$flushReact()
        output$records
        # The page has been open a while before anyone saves.
        session$elapse(1100)
        session$flushReact()
        replaced <<- 0L
        for (i in 1:5) {
          insert_record(f, list(name = paste0("r", i)), conn = conn)
          session$returned$refresh()
          session$flushReact()
        }
        first <- replaced
        session$elapse(1100)
        session$flushReact()
        out <<- c(first, replaced)
      }
    )
    out
  }

  # Default: every refresh rebuilds the table, as before.
  expect_identical(run(list())[1], 5L)
  # 1000 ms: the first shows at once, the other four arrive as one.
  counts <- run(list(refresh_delay = 1000))
  expect_identical(counts[1], 1L)
  expect_identical(counts[2], 2L)
})
