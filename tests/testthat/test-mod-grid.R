counts_form <- function() {
  form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day"),
      form_field(id = "station", label = "Station"),
      form_field(id = "party", label = "Party"),
      form_field(id = "posters", label = "Posters", input_type = "numericInput"),
      form_field(id = "flyers", label = "Flyers", input_type = "numericInput"),
      form_field(id = "note", label = "Note")
    )
  )
}

test_that("the grid loads a group's records and saves the grid as records", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", station = "1", party = "B", posters = 3, flyers = 1), conn = conn)
  insert_record(counts, list(day = "fri", station = "2", party = "A", posters = 9), conn = conn)
  station <- shiny::reactiveVal("1")

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts, rows = c("Party A" = "A", "Party B" = "B"), key = "party",
      group = function() list(day = "fri", station = station()), conn = conn, user = "ada"
    ),
    {
      session$flushReact()
      html <- as.character(output$grid$html)
      # The value columns are the numeric fields; the text field is not one.
      expect_true(grepl("Posters", html, fixed = TRUE))
      expect_false(grepl("Note", html, fixed = TRUE))
      # Row B of station 1 came from the database; row A is empty.
      expect_true(grepl('value="3"[^>]*data-r="1" data-c="0"', html))

      # The same values again: nothing is written.
      session$setInputs(cells = matrix(c(NA, 3, NA, 1), nrow = 2))
      expect_identical(nrow(fetch_audit_log(counts, conn = conn)), 2L)
      expect_identical(session$returned$changed(), 0L)

      # A change: A gets values, B is emptied.
      session$setInputs(cells = matrix(c(5, 0, 2, 0), nrow = 2))
      stored <- fetch_records(counts, conn = conn)
      expect_identical(paste(stored$station, stored$party), c("2 A", "1 A"))
      expect_identical(stored$posters[stored$station == "1"], 5)
      expect_identical(session$returned$changed(), 1L)
      expect_match(output$status, "^Saved ")
      audit <- fetch_audit_log(counts, conn = conn)
      # fetch_audit_log() orders by record, so count instead of reading the tail.
      expect_identical(sum(audit$action == "delete"), 1L)
      expect_identical(sum(audit$action == "insert"), 3L)
      expect_identical(sort(unique(audit$changed_by)), "ada")

      # Switching the group re-renders the grid with that group's records.
      station("2")
      session$flushReact()
      expect_true(grepl('value="9"[^>]*data-r="0" data-c="0"', as.character(output$grid$html)))
      expect_identical(session$returned$value()[1, 1], 5)
    }
  )
})

test_that("without autosave the grid writes on the Save button only", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts, rows = c("A", "B"), key = "party",
      group = list(day = "fri", station = "1"), conn = conn, autosave = FALSE,
      language = german()
    ),
    {
      session$flushReact()
      session$setInputs(cells = matrix(c(1, 2, 3, 4), nrow = 2))
      expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
      expect_true(grepl("Speichern", as.character(output$save_button$html), fixed = TRUE))

      session$setInputs(save = 1)
      expect_identical(nrow(fetch_records(counts, conn = conn)), 2L)
      expect_match(output$status, "^Gespeichert ")
    }
  )
})

test_that("a refused save keeps the data and reports the reason", {
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "party", label = "Party"),
      form_field(id = "posters", label = "Posters", input_type = "numericInput")
    ),
    validation_rules = list(
      forbid_if("no_negatives", function(values) isTRUE(values$posters < 0), fields = "posters",
                message = "No negative counts.")
    )
  )
  conn <- local_test_conn(counts$db)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(
    grid_server,
    args = list(id = "g", form = counts, rows = c("A", "B"), key = "party", conn = conn),
    {
      session$flushReact()
      session$setInputs(cells = matrix(c(1, -2), nrow = 2))
      expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
      expect_match(shown, "Row 2 .*No negative counts")
      expect_identical(session$returned$changed(), 0L)
    }
  )
})

test_that("hint, cols and rows as a data frame shape the grid", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts,
      rows = data.frame(value = c("A", "B"), label = c("Alpha", "Beta")), key = "party",
      group = list(day = "fri", station = "1"), cols = "flyers", conn = conn,
      hint = function(group) matrix(c(7, 8), nrow = 2)
    ),
    {
      session$flushReact()
      html <- as.character(output$grid$html)
      expect_true(grepl("Alpha", html, fixed = TRUE))
      expect_true(grepl("Flyers", html, fixed = TRUE))
      expect_false(grepl("Posters", html, fixed = TRUE))
      expect_identical(lengths(regmatches(html, gregexpr("sft-grid-hint", html))), 2L)
    }
  )

  expect_error(sft_grid_value_fields(counts, "nope", exclude = "party"), "does not have: nope")
  expect_error(sft_grid_row_spec(data.frame(x = 1)), "need a `value` column")
})
