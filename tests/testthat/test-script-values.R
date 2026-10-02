# Values a script (not the browser) hands to insert_record(): each is stored
# the same way on every backend, or refused with a validation error.

script_form <- function(db) {
  form(form_id = "scripted", table_name = "scripted", db = db, fields = list(
    form_field(id = "name", label = "Name"),
    form_field(id = "n", label = "Amount", input_type = "numericInput"),
    form_field(id = "span", label = "Span", input_type = "sliderInput",
               args = list(min = 0, max = 10, value = c(2, 5))),
    form_field(id = "d", label = "Day", input_type = "dateInput"),
    form_field(id = "tm", label = "Time", input_type = "timeInput")
  ))
}

script_backends <- function() {
  out <- list(sqlite = function() db_sqlite(tempfile(fileext = ".sqlite")))
  if (requireNamespace("duckdb", quietly = TRUE)) {
    out$duckdb <- function() db_duckdb(tempfile(fileext = ".duckdb"))
  }
  out
}

for (backend in names(script_backends())) {
  test_that(paste("text in a number field is read or refused,", backend), {
    db <- script_backends()[[backend]]()
    f <- script_form(db)
    conn <- local_test_conn(db)
    init_db(f, conn = conn)

    err <- tryCatch(insert_record(f, list(name = "a", n = "abc"), conn = conn),
                    error = function(e) e)
    expect_s3_class(err, "sft_validation_error")
    expect_identical(err$issues$source, "number")
    expect_identical(err$issues$fields[[1]], "n")
    expect_error(insert_record(f, list(name = "b", n = "1,5"), conn = conn),
                 class = "sft_validation_error")

    insert_record(f, list(name = "c", n = " 12 "), conn = conn)
    insert_record(f, list(name = "d", n = ""), conn = conn)
    rows <- fetch_records(f, conn = conn)
    expect_identical(rows$name, c("c", "d"))
    expect_true(is.numeric(rows$n))
    expect_identical(rows$n, c(12, NA))
  })

  test_that(paste("a range slider without values is stored empty,", backend), {
    db <- script_backends()[[backend]]()
    f <- script_form(db)
    conn <- local_test_conn(db)
    init_db(f, conn = conn)

    insert_record(f, list(name = "a", span = c(NA, NA)), conn = conn)
    insert_record(f, list(name = "b", span = c(1, NA)), conn = conn)
    rows <- fetch_records(f, conn = conn)
    expect_true(is.na(rows$span[1]))
    expect_identical(as.character(rows$span[2]), "[1,null]")
  })
}

test_that("a time for a date field keeps the caller's calendar day", {
  withr::local_timezone("Europe/Berlin")
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  f <- script_form(db)
  conn <- local_test_conn(db)
  init_db(f, conn = conn)

  insert_record(f, list(name = "a", d = as.POSIXct("2026-02-03 00:30:00", tz = "Europe/Berlin")),
                conn = conn)
  insert_record(f, list(name = "b", d = as.Date("2026-02-03")), conn = conn)
  expect_identical(fetch_records(f, conn = conn)$d, c("2026-02-03", "2026-02-03"))
})

test_that("a time stored as HH:MM is read back, not reset to midnight", {
  decoded <- sft_decode_time("10:30")
  expect_identical(format(decoded, "%H:%M:%S"), "10:30:00")
  expect_identical(format(sft_decode_time("10:30:15"), "%H:%M:%S"), "10:30:15")
  expect_null(sft_decode_time("half past ten"))
})

test_that("a rule reads an absent field as NULL, not a field with a longer id", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  f <- form(form_id = "tagged", table_name = "tagged", db = db, fields = list(
    form_field(id = "tag", label = "Tag", input_type = "checkboxGroupInput",
               args = list(choices = c("urgent", "later"))),
    form_field(id = "tags", label = "Tags")
  ), validation_rules = list(forbid_if(
    id = "no_urgent", condition = function(values) "urgent" %in% values$tag,
    message = "urgent is not allowed"
  )))
  conn <- local_test_conn(db)
  init_db(f, conn = conn)

  expect_no_error(insert_record(f, list(tags = "urgent"), conn = conn))
  expect_error(insert_record(f, list(tag = "urgent"), conn = conn), "urgent is not allowed")
})

for (backend in names(script_backends())) {
  test_that(paste("a number in a text field is stored as its digits; old '7.0' keys still match,", backend), {
    db <- script_backends()[[backend]]()
    f <- form(form_id = "keys", table_name = "keys", db = db, fields = list(
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "count", label = "Count", input_type = "numericInput")
    ))
    conn <- local_test_conn(db)
    init_db(f, conn = conn)

    insert_record(f, list(site = 7, species = "A", count = 1), conn = conn)
    insert_record(f, list(site = 100000, species = "B", count = 1), conn = conn)
    insert_record(f, list(site = TRUE, species = "C", count = 1), conn = conn)
    expect_identical(fetch_records(f, conn = conn)$site, c("7", "100000", "1"))

    # A row an older version wrote as "7.0" is found by 7 and by "7".
    DBI::dbExecute(conn, "UPDATE keys SET site = '7.0' WHERE species = 'A'")
    upsert_records(f, data.frame(site = 7, species = "A", count = 2), key = c("site", "species"), conn = conn)
    upsert_records(f, data.frame(site = "7", species = "A", count = 3), key = c("site", "species"), conn = conn)
    rows <- fetch_records(f, conn = conn)
    expect_identical(nrow(rows), 3L)
    expect_identical(rows$count[rows$species == "A"], 3)
  })
}

test_that("long numbers in a text field keep every digit, and stay distinct upsert keys", {
  expect_identical(sft_encode_text(1234567890123456), "1234567890123456")
  expect_identical(sft_encode_text(0.1234567890123456789), "0.12345678901234568")
  expect_identical(sft_encode_text(1 / 3), "0.3333333333333333")
  expect_identical(sft_encode_text(0.1), "0.1")
  expect_identical(sft_encode_text(-0), "0")

  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  f <- form(form_id = "accounts", table_name = "accounts", db = db, fields = list(
    form_field(id = "account", label = "Account"),
    form_field(id = "owner", label = "Owner")
  ))
  conn <- local_test_conn(db)
  init_db(f, conn = conn)
  upsert_records(f, data.frame(account = c(1234567890123456, 1234567890123457), owner = c("Anna", "Bert")),
                 key = "account", conn = conn)
  rows <- fetch_records(f, conn = conn)
  expect_identical(rows$account, c("1234567890123456", "1234567890123457"))
  expect_identical(rows$owner, c("Anna", "Bert"))
})

for (backend in names(script_backends())) {
  test_that(paste("text in a number field is refused on update and upsert too,", backend), {
    db <- script_backends()[[backend]]()
    f <- script_form(db)
    conn <- local_test_conn(db)
    init_db(f, conn = conn)
    insert_record(f, list(name = "a", n = 1), conn = conn)
    expect_error(update_record(f, list(n = "abc"), record_id = 1L, conn = conn), class = "sft_validation_error")
    expect_error(upsert_records(f, data.frame(name = "a", n = "1,5"), key = "name", conn = conn),
                 class = "sft_validation_error")
    expect_error(insert_record(f, list(name = "b", n = "0x1A"), conn = conn), class = "sft_validation_error")
    expect_identical(fetch_records(f, conn = conn)$n, 1)
  })
}

test_that("upsert keys find numbers and flags under their earlier spellings", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  f <- form(form_id = "k", table_name = "k", db = db, fields = list(
    form_field(id = "key", label = "Key"), form_field(id = "v", label = "V")
  ))
  conn <- local_test_conn(db)
  init_db(f, conn = conn)
  old <- c("0.0001", "1.0e-05", "1.0e+20", "1e15", "0.3")
  for (k in old) insert_record(f, list(key = k, v = "old"), conn = conn)
  keys <- list(0.0001, 0.00001, 1e20, 1e15, 0.1 + 0.2)
  for (k in keys) upsert_records(f, list(list(key = k, v = "new")), key = "key", conn = conn)
  rows <- fetch_records(f, conn = conn)
  expect_identical(nrow(rows), length(old))
  expect_identical(sum(rows$v == "new"), length(keys))
  expect_identical(sft_encode_text(0.0001), "0.0001")

  skip_if_not_installed("duckdb")
  duck <- db_duckdb(tempfile(fileext = ".duckdb"))
  g <- form(form_id = "k", table_name = "k", db = duck, fields = list(
    form_field(id = "key", label = "Key"), form_field(id = "v", label = "V")
  ))
  dconn <- local_test_conn(duck)
  init_db(g, conn = dconn)
  insert_record(g, list(key = "true", v = "old"), conn = dconn)
  upsert_records(g, list(list(key = TRUE, v = "new")), key = "key", conn = dconn)
  expect_identical(fetch_records(g, conn = dconn)$v, "new")
})
