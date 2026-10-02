testthat::test_that("validation_rule blocks contradictory records on insert", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "rules_test",
    table_name = "rules_test",
    db_path = db_path,
    fields = list(
      form_field(id = "status", label = "Status"),
      form_field(id = "reason", label = "Begründung")
    ),
    validation_rules = list(
      required_if(
        id = "reason_required_when_rejected",
        condition = function(values) identical(values$status, "abgelehnt"),
        fields = "reason",
        message = "Bei abgelehntem Status muss eine Begründung angegeben werden."
      )
    )
  )

  testthat::expect_error(
    insert_record(
      form = form,
      record = list(status = "abgelehnt", reason = ""),
      conn = conn
    ),
    "Begründung"
  )

  ok <- insert_record(
    form = form,
    record = list(status = "abgelehnt", reason = "Dublettenprüfung"),
    conn = conn
  )

  testthat::expect_equal(ok$reason, "Dublettenprüfung")
})

testthat::test_that("validation_rule is evaluated on merged update records", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "update_rules_test",
    table_name = "update_rules_test",
    db_path = db_path,
    fields = list(
      form_field(id = "start_date", label = "Start"),
      form_field(id = "end_date", label = "Ende")
    ),
    validation_rules = list(
      validation_rule(
        id = "end_after_start",
        validate = function(values) {
          is.na(values$end_date) ||
            !nzchar(values$end_date) ||
            as.Date(values$end_date) >= as.Date(values$start_date)
        },
        message = "Das Ende darf nicht vor dem Start liegen."
      )
    )
  )

  inserted <- insert_record(
    form = form,
    record = list(start_date = "2026-01-10", end_date = "2026-01-15"),
    conn = conn
  )

  testthat::expect_error(
    update_record(
      form = form,
      record_id = inserted$sft_id[1],
      values = list(end_date = "2026-01-01"),
      conn = conn
    ),
    "Ende"
  )
})

testthat::test_that("validation warnings do not block persistence", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)

  form <- form(
    form_id = "warning_rules_test",
    table_name = "warning_rules_test",
    db_path = db_path,
    fields = list(
      form_field(id = "note", label = "Hinweis")
    ),
    validation_rules = list(
      validation_rule(
        id = "short_note_warning",
        validate = function(values) nchar(values$note %||% "") >= 3L,
        severity = "warning",
        message = "Hinweis ist sehr kurz."
      )
    )
  )

  testthat::expect_warning(
    out <- insert_record(
      form = form,
      record = list(note = "x"),
      conn = conn
    ),
    "kurz"
  )

  testthat::expect_equal(out$note, "x")
})
