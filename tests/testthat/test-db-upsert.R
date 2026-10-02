counts_form <- function() {
  form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "site", label = "Site", mandatory = TRUE),
      form_field(id = "species", label = "Species", mandatory = TRUE),
      form_field(id = "first", label = "Adults", input_type = "numericInput"),
      form_field(id = "second", label = "Juveniles", input_type = "numericInput")
    )
  )
}

counts_only <- function(values) all(unlist(values) %in% c(0, NA))

test_that("upsert_records inserts, updates and leaves unchanged rows alone", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  first <- upsert_records(
    counts,
    data.frame(site = "1", species = c("A", "B"), first = c(10, 20), second = c(11, 21)),
    key = c("site", "species"), conn = conn, user = "ada"
  )
  expect_identical(first$action, c("insert", "insert"))

  second <- upsert_records(
    counts,
    data.frame(site = "1", species = c("A", "B", "C"), first = c(10, 25, 5), second = c(11, 21, 5)),
    key = c("site", "species"), conn = conn, user = "bob"
  )
  expect_identical(second$action, c("unchanged", "update", "insert"))
  expect_identical(second$sft_id[1:2], first$sft_id)

  stored <- fetch_records(counts, conn = conn)
  expect_identical(stored$species, c("A", "B", "C"))
  expect_identical(stored$first, c(10, 25, 5))

  audit <- fetch_audit_log(counts, conn = conn)
  expect_identical(audit$action, c("insert", "insert", "update", "insert"))
  # The unchanged row wrote nothing; the update names only what changed.
  expect_identical(audit$changed_fields_json[3], "[\"first\"]")
  expect_identical(audit$changed_by[3], "bob")
})

test_that("empty rows are soft-deleted when stored and skipped otherwise", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  upsert_records(
    counts, data.frame(site = "1", species = c("A", "B"), first = c(10, 20), second = 0),
    key = c("site", "species"), conn = conn
  )

  result <- upsert_records(
    counts,
    data.frame(site = "1", species = c("A", "B", "C"), first = c(10, 0, 0), second = c(0, 0, NA)),
    key = c("site", "species"), conn = conn, empty = counts_only
  )
  expect_identical(result$action, c("unchanged", "delete", "skip"))
  expect_true(is.na(result$sft_id[3]))

  live <- fetch_records(counts, conn = conn)
  expect_identical(live$species, "A")
  deleted <- fetch_records(counts, conn = conn, include_deleted = "only")
  expect_identical(deleted$species, "B")
})

test_that("scope soft-deletes stored rows of the group that records do not mention", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  upsert_records(
    counts,
    data.frame(site = c("1", "1", "2"), species = c("A", "B", "A"), first = 1, second = 1),
    key = c("site", "species"), conn = conn
  )

  result <- upsert_records(
    counts, data.frame(site = "1", species = "A", first = 1, second = 1),
    key = c("site", "species"), conn = conn, scope = list(site = "1")
  )
  expect_identical(result$action, c("unchanged", "delete"))
  expect_true(is.na(result$row[2]))

  live <- fetch_records(counts, conn = conn)
  # Site 2 is outside the scope and untouched.
  expect_identical(paste(live$site, live$species), c("1 A", "2 A"))
})

test_that("a failing row rolls the whole call back and names the row", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  bad <- list(
    list(site = "1", species = "A", first = 1),
    list(site = "1", species = "B", first = 2),
    list(site = "1", species = "", first = 3)
  )
  err <- tryCatch(
    upsert_records(counts, bad, key = c("site", "species"), conn = conn),
    error = function(e) e
  )
  expect_s3_class(err, "sft_validation_error")
  expect_match(conditionMessage(err), "^Row 3 \\(site = 1, species = \\)")

  expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
  expect_identical(nrow(fetch_audit_log(counts, conn = conn)), 0L)
})

test_that("the key is checked before anything is written", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  rows <- data.frame(site = "1", species = "A", first = 1)

  expect_error(upsert_records(counts, rows, key = "nope", conn = conn), "does not have: nope")
  expect_error(upsert_records(counts, rows, key = character(), conn = conn), "at least one field")
  expect_error(
    upsert_records(counts, data.frame(site = "1", first = 1), key = c("site", "species"), conn = conn),
    "Row 1 has no value for key field 'species'"
  )
  expect_error(upsert_records(counts, "no", key = "species", conn = conn), "data frame or a list")
  expect_error(upsert_records(counts, rows, key = "species", conn = conn, empty = 1), "empty must be")
  expect_error(upsert_records(counts, rows, key = "species", conn = conn, scope = list(1)), "scope must be")
  expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
})

test_that("an ambiguous key is refused instead of updating one of the records", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(site = "1", species = "A", first = 1), conn = conn)
  insert_record(counts, list(site = "1", species = "A", first = 2), conn = conn)

  expect_error(
    upsert_records(counts, data.frame(site = "1", species = "A", first = 3),
                   key = c("site", "species"), conn = conn),
    "2 live records share this key"
  )
  expect_identical(fetch_records(counts, conn = conn)$first, c(1, 2))
})

test_that("a list of named lists works like a data frame and NA keys match NULL", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  first <- upsert_records(
    counts, list(list(site = "1", species = "A", first = 1, second = NA)),
    key = c("site", "species", "second"), conn = conn
  )
  second <- upsert_records(
    counts, list(list(site = "1", species = "A", first = 9, second = NA)),
    key = c("site", "species", "second"), conn = conn
  )
  expect_identical(first$action, "insert")
  expect_identical(second$action, "update")
  expect_identical(fetch_records(counts, conn = conn)$first, 9)
})

test_that("upsert_records works on DuckDB, where several audit rows share one transaction", {
  skip_if_not_installed("duckdb")
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_duckdb(tempfile(fileext = ".duckdb")),
    fields = list(
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "first", label = "Adults", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(counts$db)

  first <- upsert_records(
    counts, data.frame(site = "1", species = c("A", "B"), first = c(10, 20)),
    key = c("site", "species"), conn = conn
  )
  second <- upsert_records(
    counts, data.frame(site = "1", species = c("A", "C"), first = c(11, 0)),
    key = c("site", "species"), conn = conn,
    empty = counts_only, scope = list(site = "1")
  )
  expect_identical(first$action, c("insert", "insert"))
  expect_identical(second$action, c("update", "skip", "delete"))
  expect_identical(fetch_records(counts, conn = conn)$first, 11)
  # Two versions per record, numbered per record, in one transaction.
  audit <- fetch_audit_log(counts, conn = conn)
  expect_identical(as.vector(tapply(audit$version_no, audit$record_id, max)), c(2L, 2L))
})

test_that("scope keeps rows whose key the database stores in another spelling", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  # site is a text field: 12 is stored as "12.0" and 1e5 as "100000.0",
  # so comparing key strings in R would call these rows foreign and delete them.
  first <- upsert_records(
    counts, data.frame(site = 12, species = c("A", "B"), first = 1:2),
    key = c("site", "species"), conn = conn, scope = list(site = 12)
  )
  expect_identical(first$action, c("insert", "insert"))

  again <- upsert_records(
    counts, data.frame(site = 12, species = c("A", "B"), first = 1:2),
    key = c("site", "species"), conn = conn, scope = list(site = 12)
  )
  expect_identical(again$action, c("unchanged", "unchanged"))

  big <- upsert_records(
    counts, data.frame(site = 100000L, species = "A", first = 1),
    key = c("site", "species"), conn = conn, scope = list(site = 100000L)
  )
  expect_identical(big$action, "insert")
  expect_identical(nrow(fetch_records(counts, conn = conn)), 3L)
})

test_that("an empty key value finds the record it stored", {
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "first", label = "Adults", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(counts$db)
  rows <- function(k) data.frame(site = "", species = "A", first = k)

  expect_identical(upsert_records(counts, rows(1), key = c("site", "species"), conn = conn)$action, "insert")
  expect_identical(upsert_records(counts, rows(2), key = c("site", "species"), conn = conn)$action, "update")
  expect_identical(nrow(fetch_records(counts, conn = conn)), 1L)
})

test_that("scope fields have to be part of the key", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(site = "2", species = "A", first = 9), conn = conn)

  expect_error(
    upsert_records(counts, data.frame(site = "1", species = "A", first = 5),
                   key = "species", conn = conn, scope = list(site = "1")),
    "scope fields must be part of key: site"
  )
  expect_identical(fetch_records(counts, conn = conn)$site, "2")
})

test_that("a number that comes back in another spelling counts as unchanged", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  row <- data.frame(site = "1", species = "A", first = 0.1, second = 12)

  upsert_records(counts, row, key = c("site", "species"), conn = conn)
  expect_identical(upsert_records(counts, row, key = c("site", "species"), conn = conn)$action, "unchanged")
  expect_identical(nrow(fetch_audit_log(counts, conn = conn)), 1L)
})

test_that("expected stops the whole call when a row was changed meanwhile", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  upsert_records(counts, data.frame(site = "1", species = c("A", "B"), first = c(1, 2)),
                 key = c("site", "species"), conn = conn)
  update_record(counts, list(first = 9), record_id = 2, conn = conn, user = "bob")

  err <- tryCatch(
    upsert_records(
      counts, data.frame(site = "1", species = c("A", "B"), first = c(5, 6)),
      key = c("site", "species"), conn = conn,
      expected = list(list(first = 1), list(first = 2))
    ),
    error = function(e) e
  )
  expect_s3_class(err, "sft_edit_conflict")
  expect_identical(err$row, 2L)
  expect_identical(fetch_records(counts, conn = conn)$first, c(1, 9))

  # What the caller saw is still stored: the check passes. NA means "no record".
  ok <- upsert_records(
    counts, data.frame(site = "1", species = c("A", "C"), first = c(5, 3)),
    key = c("site", "species"), conn = conn,
    expected = list(list(first = 1), list(first = NA))
  )
  expect_identical(ok$action, c("update", "insert"))
  expect_error(
    upsert_records(counts, data.frame(site = "1", species = "A", first = 1),
                   key = c("site", "species"), conn = conn, expected = list()),
    "one element per record"
  )
})

test_that("an empty numeric key is looked up as NULL only", {
  skip_if_not_installed("duckdb")
  years <- form(
    form_id = "years", table_name = "years",
    db = db_duckdb(tempfile(fileext = ".duckdb")),
    fields = list(
      form_field(id = "year", label = "Year", input_type = "numericInput"),
      form_field(id = "species", label = "Species"),
      form_field(id = "n", label = "N", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(years$db)
  upsert_records(years, data.frame(year = 2020, species = "A", n = 1), key = c("year", "species"), conn = conn)

  first <- upsert_records(years, data.frame(year = NA_real_, species = "A", n = 1), key = c("year", "species"), conn = conn)
  second <- upsert_records(years, data.frame(year = NA_real_, species = "A", n = 2), key = c("year", "species"), conn = conn)
  expect_identical(c(first$action, second$action), c("insert", "update"))
})

test_that("numbers that differ in the 8th significant digit differ", {
  expect_true(sft_values_differ(100000.01, 100000.02))
  expect_true(sft_values_differ(48.137154, 48.137155))
  expect_false(sft_values_differ(12, "12"))
  expect_false(sft_values_differ(c(1, 2.5), c(1, 2.5)))
  expect_identical(sft_norm_value(c(1, 2.5)), paste("1", "2.5", sep = intToUtf8(31L)))
})

test_that("empty can be given per record", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  upsert_records(counts, data.frame(site = "1", species = c("A", "B"), first = c(1, 2)),
                 key = c("site", "species"), conn = conn)

  # Row A carries only one cell, 0; the caller knows the row is not empty.
  result <- upsert_records(
    counts, list(list(site = "1", species = "A", first = 0), list(site = "1", species = "B", first = 2)),
    key = c("site", "species"), conn = conn, empty = c(FALSE, TRUE)
  )
  expect_identical(result$action, c("update", "delete"))
  expect_identical(fetch_records(counts, conn = conn)$species, "A")
  expect_error(
    upsert_records(counts, data.frame(site = "1", species = "A", first = 1),
                   key = c("site", "species"), conn = conn, empty = c(TRUE, FALSE)),
    "one element per record"
  )
})

test_that("an empty row does not delete a record that holds more than the row", {
  notes <- form(
    form_id = "n", table_name = "n", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "first", label = "First", input_type = "numericInput"),
      form_field(id = "remark", label = "Remark")
    )
  )
  conn <- local_test_conn(notes$db)
  insert_record(notes, list(site = "1", species = "A", first = 5, remark = "recount asked"), conn = conn)
  insert_record(notes, list(site = "1", species = "B", first = 3), conn = conn)

  result <- upsert_records(
    notes, data.frame(site = "1", species = c("A", "B"), first = 0),
    key = c("site", "species"), conn = conn, empty = function(values) all(unlist(values) %in% c(0, NA))
  )
  expect_identical(result$action, c("update", "delete"))
  live <- fetch_records(notes, conn = conn)
  expect_identical(live$species, "A")
  expect_identical(c(live$remark, as.character(live$first)), c("recount asked", "0"))
})

test_that("an emptied row whose record holds only an unticked checkbox is deleted", {
  f <- form(form_id = "cb", table_name = "cb", db = db_sqlite(tempfile(fileext = ".sqlite")), fields = list(
    form_field(id = "species", label = "Species"),
    form_field(id = "posters", label = "Posters", input_type = "numericInput"),
    form_field(id = "checked", label = "Checked", input_type = "checkboxInput")))
  conn <- db_connect(f$db); on.exit(db_disconnect(conn))
  init_db(f, conn = conn)
  insert_record(f, list(species = "A", posters = 4, checked = FALSE), conn = conn)
  insert_record(f, list(species = "B", posters = 4, checked = TRUE), conn = conn)

  res <- upsert_records(f, list(list(species = "A", posters = NA), list(species = "B", posters = NA)),
                        key = "species", empty = c(TRUE, TRUE), conn = conn)
  expect_identical(res$action, c("delete", "update"))
  live <- fetch_records(f, conn = conn)
  expect_identical(live$species, "B")
  expect_true(is.na(live$posters))
})

test_that("a value for a locked field does not turn an upsert into an empty update", {
  f <- form(form_id = "lk", table_name = "lk", db = db_sqlite(tempfile(fileext = ".sqlite")), fields = list(
    form_field(id = "k", label = "K"),
    form_field(id = "price", label = "Price", input_type = "numericInput", editable = FALSE)))
  conn <- db_connect(f$db); on.exit(db_disconnect(conn))
  init_db(f, conn = conn)
  upsert_records(f, list(list(k = "a", price = 1)), key = "k", conn = conn)
  for (i in 1:2) {
    expect_identical(upsert_records(f, list(list(k = "a", price = 2)), key = "k", conn = conn)$action,
                     "unchanged")
  }
  expect_identical(fetch_records(f, conn = conn)$price, 1)
  expect_identical(nrow(fetch_audit_log(f, conn = conn)), 1L)
})
