votes <- matrix(c(412, 388, 0, NA), nrow = 2, dimnames = list(c("A", "B"), c("First", "Second")))

test_that("grid_input renders one number cell per row and column", {
  html <- as.character(grid_input("g", "Votes", value = votes, hint = votes * 0 + 1))

  expect_true(grepl('id="g"', html, fixed = TRUE))
  expect_identical(lengths(regmatches(html, gregexpr('type="number"', html))), 4L)
  expect_true(grepl('data-r="1" data-c="0"', html, fixed = TRUE))
  expect_true(grepl('value="412"', html, fixed = TRUE))
  # An empty cell is an empty input; a hint is a span next to it, none for NA.
  expect_true(grepl('value=""', html, fixed = TRUE))
  expect_identical(lengths(regmatches(html, gregexpr("sft-grid-hint", html))), 3L)
  expect_true(grepl('data-rows="[&quot;A&quot;,&quot;B&quot;]"', html, fixed = TRUE))
  expect_true(grepl("data-sum-col", html, fixed = TRUE))

  deps <- htmltools::findDependencies(grid_input("g", rows = "A", cols = "x"))
  expect_identical(deps[[1]]$name, "sft-grid")
  expect_true(file.exists(file.path(deps[[1]]$src$file, "sft-grid.js")))

  expect_false(grepl("data-sum-col", as.character(grid_input("g", rows = "A", cols = "x", sums = FALSE)), fixed = TRUE))
  expect_error(grid_input("g", value = matrix(1)), "rows and cols")
  expect_error(grid_input("g", value = matrix(1), rows = c("A", "B"), cols = "x"), "2 x 1 matrix")
})

test_that("the input handler turns the client's rows into a named matrix", {
  received <- list(rows = list("A", "B"), cols = list("x", "y"), values = list(list(1, NULL), list(2.5, 0)))
  m <- sft_grid_input_handler(received)

  expect_identical(dim(m), c(2L, 2L))
  expect_identical(dimnames(m), list(c("A", "B"), c("x", "y")))
  expect_identical(m["A", "y"], NA_real_)
  expect_identical(m["B", "x"], 2.5)

  # Shiny may hand simplified vectors instead of lists; same result.
  simplified <- list(rows = c("A", "B"), cols = c("x", "y"), values = list(c(1, NA), c(2.5, 0)))
  expect_identical(sft_grid_input_handler(simplified), m)
  expect_null(sft_grid_input_handler(NULL))
  expect_identical(dim(sft_grid_input_handler(list(rows = list(), cols = list("x"), values = list()))), c(0L, 1L))
})

test_that("a grid round-trips through the stored JSON and shows its totals", {
  stored <- sft_grid_encode(votes)
  expect_identical(stored, "{\"rows\":[\"A\",\"B\"],\"cols\":[\"First\",\"Second\"],\"values\":[[412,0],[388,null]]}")
  expect_identical(sft_grid_decode(stored), votes)
  expect_identical(sft_grid_encode(NULL), NA_character_)
  expect_null(sft_grid_decode(NA_character_))
  expect_identical(sft_grid_format(stored), "First 800; Second 0")

  # As a form field: stored in one column, back as a matrix, totals in the table.
  results <- form(
    form_id = "results", table_name = "results",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "district", label = "District"),
      form_field(id = "votes", label = "Votes", input_type = "grid_input",
                 args = list(rows = c("A", "B"), cols = c("First", "Second")))
    )
  )
  conn <- local_test_conn(results$db)
  insert_record(results, list(district = "1", votes = votes), conn = conn)
  row <- fetch_records(results, conn = conn)
  expect_identical(sft_ui_value(results$fields[[2]], row$votes), votes)
  expect_identical(records_datatable(row, results)$x$data$Votes, "First 800; Second 0")
})

test_that("update_grid_input sends values and hints as rows with NA as null", {
  messages <- list()
  session <- list(sendInputMessage = function(inputId, message) messages[[inputId]] <<- message)

  update_grid_input(session, "g", value = votes, hint = votes + 1)
  expect_identical(messages$g$values, list(list(412, 0), list(388, NULL)))
  expect_identical(messages$g$hint[[1]], list(413, 1))

  update_grid_input(session, "h")
  expect_null(messages$h)
})
