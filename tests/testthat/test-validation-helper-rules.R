test_that("forbid_if blocks forbidden states", {
  form <- form(
    form_id = "validation_forbid",
    fields = list(
      form_field("status", "Status"),
      form_field("comment", "Kommentar")
    ),
    validation_rules = list(
      forbid_if(
        id = "no_done_without_comment",
        condition = function(values) identical(values$status, "erledigt") && sft_is_empty_value(values$comment),
        fields = c("status", "comment"),
        message = "Erledigt braucht Kommentar."
      )
    )
  )

  expect_error(
    validate_record(form, list(status = "erledigt", comment = "")),
    "Erledigt braucht Kommentar",
    fixed = TRUE
  )

  expect_silent(validate_record(form, list(status = "offen", comment = "")))
})

test_that("compare_fields validates simple field comparisons", {
  form <- form(
    form_id = "validation_compare",
    fields = list(
      form_field("start", "Start", input_type = "dateInput"),
      form_field("end", "Ende", input_type = "dateInput")
    ),
    validation_rules = list(
      compare_fields(
        id = "start_before_end",
        left = "start",
        operator = "<=",
        right = "end",
        message = "Start muss vor Ende liegen."
      )
    )
  )

  expect_error(
    validate_record(form, list(start = as.Date("2026-01-02"), end = as.Date("2026-01-01"))),
    "Start muss vor Ende liegen",
    fixed = TRUE
  )

  expect_silent(
    validate_record(form, list(start = as.Date("2026-01-01"), end = as.Date("2026-01-02")))
  )
})

test_that("must_be_unique validates compound uniqueness", {
  db_file <- tempfile(fileext = ".sqlite")
  form <- form(
    form_id = "validation_unique_combo",
    db = db_sqlite(db_file),
    fields = list(
      form_field("street", "Straße"),
      form_field("house_no", "Hausnummer")
    ),
    validation_rules = list(
      must_be_unique(
        id = "unique_address",
        fields = c("street", "house_no"),
        message = "Adresse ist bereits vorhanden."
      )
    )
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  init_db(form, con)
  insert_record(form, list(street = "Ackerstraße", house_no = "1"), conn = con)

  expect_error(
    insert_record(form, list(street = "Ackerstraße", house_no = "1"), conn = con),
    "Adresse ist bereits vorhanden",
    fixed = TRUE
  )
})
