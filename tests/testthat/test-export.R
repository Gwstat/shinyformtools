test_form_export <- function(db = db_sqlite(tempfile(fileext = ".sqlite"))) {
  form(
    form_id = "export_people",
    table_name = "export_people",
    db = db,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "age", label = "Age", input_type = "numericInput"),
      form_field(id = "member", label = "Member", input_type = "checkboxInput"),
      form_field(
        id = "tags", label = "Tags", input_type = "checkboxGroupInput",
        args = list(choices = c("a", "b", "c"))
      ),
      form_field(id = "note", label = "Note", markdown = TRUE)
    )
  )
}

seed_export <- function(people, conn) {
  insert_record(
    people,
    list(name = "Müller, Jörg", age = 36.5, member = TRUE, tags = c("a", "b"), note = "**bold**"),
    conn = conn
  )
  insert_record(
    people,
    list(name = "Ada \"the first\"", age = 28, member = FALSE, tags = "c"),
    conn = conn
  )
}

read_export_csv <- function(path, sep = ",", dec = ".") {
  utils::read.csv(
    path, sep = sep, dec = dec, fileEncoding = "UTF-8-BOM",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("export_records prepares values for a spreadsheet", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  out <- export_records(people, conn = conn)

  expect_identical(names(out)[names(out) %in% c("Name", "Age", "Member", "Tags", "Note")],
                   c("Name", "Age", "Member", "Tags", "Note"))
  expect_type(out$Age, "double")
  expect_identical(out$Member, c(TRUE, FALSE))
  # Multi-value fields are readable text, also for a single selection.
  expect_identical(out$Tags, c("a; b", "c"))
  # Markdown stays source text: an export is not an HTML page.
  expect_identical(out$Note[1], "**bold**")
  expect_false(any(grepl("^sft_(created|updated|is_deleted|unique)", names(out))))
})

test_that("labels = FALSE and columns control the header and the column set", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  out <- export_records(people, conn = conn, labels = FALSE, columns = c("age", "name"))

  expect_identical(names(out), c("age", "name"))
})

test_that("soft-deleted records are exported only on request", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)
  soft_delete_record(people, record_id = 2L, conn = conn)

  expect_identical(nrow(export_records(people, conn = conn)), 1L)
  expect_identical(nrow(export_records(people, conn = conn, include_deleted = TRUE)), 2L)
  expect_identical(export_records(people, conn = conn, include_deleted = "only")$Name,
                   "Ada \"the first\"")
})

test_that("a CSV export survives umlauts, quotes and commas", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  path <- tempfile(fileext = ".csv")
  export_records(people, path, conn = conn)

  # Excel needs the byte order mark to read the file as UTF-8.
  expect_identical(readBin(path, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))

  back <- read_export_csv(path)
  expect_identical(back$Name, c("Müller, Jörg", "Ada \"the first\""))
  expect_equal(back$Age, c(36.5, 28))
  expect_identical(back$Tags, c("a; b", "c"))
})

test_that("csv2 writes semicolons and a decimal comma", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  path <- tempfile(fileext = ".csv")
  export_records(people, path, conn = conn, format = "csv2")

  expect_true(any(grepl("36,5", readLines(path, warn = FALSE), fixed = TRUE)))
  expect_equal(read_export_csv(path, sep = ";", dec = ",")$Age, c(36.5, 28))
})

test_that("an Excel export keeps numbers numeric", {
  skip_if_not_installed("openxlsx")

  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  path <- tempfile(fileext = ".xlsx")
  export_records(people, path, conn = conn)
  back <- openxlsx::read.xlsx(path, check.names = FALSE)

  expect_identical(back$Name[1], "Müller, Jörg")
  expect_equal(back$Age, c(36.5, 28))
})

test_that("the format must be recognisable", {
  people <- test_form_export()

  expect_error(export_records(people, tempfile(fileext = ".txt")), "Cannot tell the export format")
  expect_error(export_records(people, tempfile(fileext = ".csv"), format = "pdf"), "format must be one of")
})

test_that("an empty table exports its header", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  init_db(people, conn = conn)

  path <- tempfile(fileext = ".csv")
  out <- export_records(people, path, conn = conn)

  expect_identical(nrow(out), 0L)
  expect_true("Name" %in% names(read_export_csv(path)))
})

test_that("form_ui draws export buttons only on request", {
  plain <- as.character(form_ui("m"))
  expect_false(grepl("m-export_", plain, fixed = TRUE))

  chosen <- as.character(form_ui("m", show_export = c("csv2", "xlsx")))
  expect_true(grepl("m-export_csv2", chosen, fixed = TRUE))
  expect_true(grepl("m-export_xlsx", chosen, fixed = TRUE))
  expect_false(grepl("m-export_csv\"", chosen, fixed = TRUE))

  german_ui <- as.character(form_ui("m", show_export = "csv", language = german()))
  expect_true(grepl("CSV-Export", german_ui, fixed = TRUE))

  expect_error(form_ui("m", show_export = "pdf"), "show_export must be")
})

test_that("the module's download holds the visible columns and respects can_export", {
  people <- test_form_export()
  conn <- local_test_conn(people$db)
  seed_export(people, conn)

  shiny::testServer(
    form_server,
    args = list(
      id = "m", form = people, conn = conn,
      columns = list(visible = c("name", "age"), persist = FALSE)
    ),
    {
      session$flushReact()
      back <- read_export_csv(output$export_csv)

      expect_identical(names(back), c("Name", "Age"))
      expect_identical(nrow(back), 2L)

      # The table's search box narrows the download to the matching rows.
      session$setInputs(records_rows_all = 2L)
      expect_identical(read_export_csv(output$export_csv)$Name, "Ada \"the first\"")
    }
  )

  shiny::testServer(
    form_server,
    args = list(
      id = "m", form = people, conn = conn,
      permissions = list(can_export = FALSE)
    ),
    {
      session$flushReact()
      expect_error(output$export_csv, "not permitted")
    }
  )
})

test_that("form_ui and form_buttons bring Shiny's selectize before any table can", {
  # DT's column filter ships an older selectize; whichever registers first
  # wins on the client, and the dialog's select inputs need Shiny's copy.
  for (ui in list(form_ui("m"), form_buttons("m"))) {
    deps <- htmltools::findDependencies(ui)
    selectize <- Filter(function(d) identical(d$name, "selectize"), deps)
    expect_length(selectize, 1L)
    expect_true(package_version(selectize[[1L]]$version) >= "0.15")
  }
})
