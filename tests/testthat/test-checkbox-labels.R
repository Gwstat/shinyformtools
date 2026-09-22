test_that("checkbox fields show 0 / 1 unless checkbox_labels asks for words", {
  flags <- form(
    form_id = "flags", table_name = "flags",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "active", label = "Active", input_type = "checkboxInput")
    )
  )
  conn <- local_test_conn(flags$db)
  insert_record(flags, list(name = "a", active = TRUE), conn = conn)
  insert_record(flags, list(name = "b", active = FALSE), conn = conn)
  data <- fetch_records(flags, conn = conn)

  # Default: unchanged from earlier releases.
  expect_identical(records_datatable(data, flags)$x$data$Active, c(1L, 0L))

  expect_identical(
    records_datatable(data, flags, checkbox_labels = TRUE)$x$data$Active,
    c("Yes", "No")
  )
  sft_with_language(german(), {
    expect_identical(
      records_datatable(data, flags, checkbox_labels = TRUE)$x$data$Active,
      c("Ja", "Nein")
    )
  })

  # The module threads the setting through its table bundle. renderDT serves
  # the rows through its own endpoint, so the argument is captured instead.
  seen <- NULL
  local_mocked_bindings(records_datatable = function(data, form, ..., checkbox_labels = FALSE) {
    seen <<- checkbox_labels
    DT::datatable(data.frame())
  })
  shiny::testServer(
    form_server,
    args = list(id = "m", form = flags, conn = conn,
                columns = list(persist = FALSE), table = list(checkbox_labels = TRUE)),
    {
      session$flushReact()
      output$records
      expect_true(seen)
    }
  )
})
