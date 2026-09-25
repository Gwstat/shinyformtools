votes_form <- function() {
  form(
    form_id = "votes", table_name = "votes",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "district", label = "District", mandatory = TRUE),
      form_field(id = "party", label = "Party", mandatory = TRUE),
      form_field(id = "first", label = "First vote", input_type = "numericInput"),
      form_field(id = "second", label = "Second vote", input_type = "numericInput")
    )
  )
}

counts_only <- function(values) all(unlist(values) %in% c(0, NA))

test_that("upsert_records inserts, updates and leaves unchanged rows alone", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)

  first <- upsert_records(
    votes,
    data.frame(district = "1", party = c("A", "B"), first = c(10, 20), second = c(11, 21)),
    key = c("district", "party"), conn = conn, user = "ada"
  )
  expect_identical(first$action, c("insert", "insert"))

  second <- upsert_records(
    votes,
    data.frame(district = "1", party = c("A", "B", "C"), first = c(10, 25, 5), second = c(11, 21, 5)),
    key = c("district", "party"), conn = conn, user = "bob"
  )
  expect_identical(second$action, c("unchanged", "update", "insert"))
  expect_identical(second$sft_id[1:2], first$sft_id)

  stored <- fetch_records(votes, conn = conn)
  expect_identical(stored$party, c("A", "B", "C"))
  expect_identical(stored$first, c(10, 25, 5))

  audit <- fetch_audit_log(votes, conn = conn)
  expect_identical(audit$action, c("insert", "insert", "update", "insert"))
  # The unchanged row wrote nothing; the update names only what changed.
  expect_identical(audit$changed_fields_json[3], "[\"first\"]")
  expect_identical(audit$changed_by[3], "bob")
})

test_that("empty rows are soft-deleted when stored and skipped otherwise", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)
  upsert_records(
    votes, data.frame(district = "1", party = c("A", "B"), first = c(10, 20), second = 0),
    key = c("district", "party"), conn = conn
  )

  result <- upsert_records(
    votes,
    data.frame(district = "1", party = c("A", "B", "C"), first = c(10, 0, 0), second = c(0, 0, NA)),
    key = c("district", "party"), conn = conn, empty = counts_only
  )
  expect_identical(result$action, c("unchanged", "delete", "skip"))
  expect_true(is.na(result$sft_id[3]))

  live <- fetch_records(votes, conn = conn)
  expect_identical(live$party, "A")
  deleted <- fetch_records(votes, conn = conn, include_deleted = "only")
  expect_identical(deleted$party, "B")
})

test_that("scope soft-deletes stored rows of the group that records do not mention", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)
  upsert_records(
    votes,
    data.frame(district = c("1", "1", "2"), party = c("A", "B", "A"), first = 1, second = 1),
    key = c("district", "party"), conn = conn
  )

  result <- upsert_records(
    votes, data.frame(district = "1", party = "A", first = 1, second = 1),
    key = c("district", "party"), conn = conn, scope = list(district = "1")
  )
  expect_identical(result$action, c("unchanged", "delete"))
  expect_true(is.na(result$row[2]))

  live <- fetch_records(votes, conn = conn)
  # District 2 is outside the scope and untouched.
  expect_identical(paste(live$district, live$party), c("1 A", "2 A"))
})

test_that("a failing row rolls the whole call back and names the row", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)

  bad <- list(
    list(district = "1", party = "A", first = 1),
    list(district = "1", party = "B", first = 2),
    list(district = "1", party = "", first = 3)
  )
  err <- tryCatch(
    upsert_records(votes, bad, key = c("district", "party"), conn = conn),
    error = function(e) e
  )
  expect_s3_class(err, "sft_validation_error")
  expect_match(conditionMessage(err), "^Row 3 \\(district = 1, party = \\)")

  expect_identical(nrow(fetch_records(votes, conn = conn)), 0L)
  expect_identical(nrow(fetch_audit_log(votes, conn = conn)), 0L)
})

test_that("the key is checked before anything is written", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)
  rows <- data.frame(district = "1", party = "A", first = 1)

  expect_error(upsert_records(votes, rows, key = "nope", conn = conn), "does not have: nope")
  expect_error(upsert_records(votes, rows, key = character(), conn = conn), "at least one field")
  expect_error(
    upsert_records(votes, data.frame(district = "1", first = 1), key = c("district", "party"), conn = conn),
    "Row 1 has no value for key field 'party'"
  )
  expect_error(upsert_records(votes, "no", key = "party", conn = conn), "data frame or a list")
  expect_error(upsert_records(votes, rows, key = "party", conn = conn, empty = 1), "empty must be")
  expect_error(upsert_records(votes, rows, key = "party", conn = conn, scope = list(1)), "scope must be")
  expect_identical(nrow(fetch_records(votes, conn = conn)), 0L)
})

test_that("an ambiguous key is refused instead of updating one of the records", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)
  insert_record(votes, list(district = "1", party = "A", first = 1), conn = conn)
  insert_record(votes, list(district = "1", party = "A", first = 2), conn = conn)

  expect_error(
    upsert_records(votes, data.frame(district = "1", party = "A", first = 3),
                   key = c("district", "party"), conn = conn),
    "2 live records share this key"
  )
  expect_identical(fetch_records(votes, conn = conn)$first, c(1, 2))
})

test_that("a list of named lists works like a data frame and NA keys match NULL", {
  votes <- votes_form()
  conn <- local_test_conn(votes$db)

  first <- upsert_records(
    votes, list(list(district = "1", party = "A", first = 1, second = NA)),
    key = c("district", "party", "second"), conn = conn
  )
  second <- upsert_records(
    votes, list(list(district = "1", party = "A", first = 9, second = NA)),
    key = c("district", "party", "second"), conn = conn
  )
  expect_identical(first$action, "insert")
  expect_identical(second$action, "update")
  expect_identical(fetch_records(votes, conn = conn)$first, 9)
})

test_that("upsert_records works on DuckDB, where several audit rows share one transaction", {
  skip_if_not_installed("duckdb")
  votes <- form(
    form_id = "votes", table_name = "votes",
    db = db_duckdb(tempfile(fileext = ".duckdb")),
    fields = list(
      form_field(id = "district", label = "District"),
      form_field(id = "party", label = "Party"),
      form_field(id = "first", label = "First vote", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(votes$db)

  first <- upsert_records(
    votes, data.frame(district = "1", party = c("A", "B"), first = c(10, 20)),
    key = c("district", "party"), conn = conn
  )
  second <- upsert_records(
    votes, data.frame(district = "1", party = c("A", "C"), first = c(11, 0)),
    key = c("district", "party"), conn = conn,
    empty = counts_only, scope = list(district = "1")
  )
  expect_identical(first$action, c("insert", "insert"))
  expect_identical(second$action, c("update", "skip", "delete"))
  expect_identical(fetch_records(votes, conn = conn)$first, 11)
  # Two versions per record, numbered per record, in one transaction.
  audit <- fetch_audit_log(votes, conn = conn)
  expect_identical(as.vector(tapply(audit$version_no, audit$record_id, max)), c(2L, 2L))
})
