# The opt-in probe cache: options(shinyformtools.schema_probe_ttl = seconds)
# remembers a positive schema probe per database and form definition. Off by
# default, because it trades freshness for round trips: a schema change made by
# another process is noticed up to `ttl` seconds late.

sft_local_probe_counter <- function(env = parent.frame()) {
  counter <- new.env()
  counter$n <- 0L
  real_probe <- sft_schema_is_current

  testthat::local_mocked_bindings(
    sft_schema_is_current = function(conn, form) {
      counter$n <- counter$n + 1L
      real_probe(conn, form)
    },
    .env = env
  )

  counter
}

test_that("the cache is off by default: every call probes", {
  withr::local_options(shinyformtools.schema_probe_ttl = NULL)
  sft_probe_cache_clear()

  conn <- local_test_conn()
  f <- test_form_basic()
  probes <- sft_local_probe_counter()

  init_db(f, conn = conn)
  for (i in 1:3) fetch_records(f, conn = conn)

  expect_identical(probes$n, 3L)
  expect_null(sft_probe_cache_key(conn, f))
})

test_that("with a ttl, calls inside the window skip the probe and expiry brings it back", {
  withr::local_options(shinyformtools.schema_probe_ttl = 30)
  sft_probe_cache_clear()
  withr::defer(sft_probe_cache_clear())

  now <- 1000
  testthat::local_mocked_bindings(sft_probe_clock = function() now)

  conn <- local_test_conn()
  f <- test_form_basic()
  probes <- sft_local_probe_counter()

  init_db(f, conn = conn)
  insert_record(f, list(name = "Ada"), conn = conn)   # probes once, stores
  fetch_records(f, conn = conn)
  update_record(f, record_id = 1, values = list(name = "Ada L."), conn = conn)
  expect_identical(probes$n, 1L)

  now <- 1029
  fetch_records(f, conn = conn)
  expect_identical(probes$n, 1L)

  now <- 1031
  fetch_records(f, conn = conn)
  expect_identical(probes$n, 2L)
})

test_that("an edited form definition re-probes at once and still migrates", {
  withr::local_options(shinyformtools.schema_probe_ttl = 30)
  sft_probe_cache_clear()
  withr::defer(sft_probe_cache_clear())

  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  v1 <- test_form_basic(db = db_sqlite(db_path))
  insert_record(v1, list(name = "Ada"), conn = conn)

  v2 <- form(
    form_id = "simple", table_name = "simple", db = db_sqlite(db_path), version = 2L,
    fields = list(
      form_field(id = "name", label = "Name", mandatory = TRUE),
      form_field(id = "email", label = "E-Mail", unique = TRUE),
      form_field(id = "city", label = "City")
    )
  )

  # A different signature is a different cache key: v1's positive probe does
  # not vouch for v2, so the new column is added on first contact.
  expect_false(identical(sft_probe_cache_key(conn, v1), sft_probe_cache_key(conn, v2)))
  insert_record(v2, list(name = "Bob", city = "Kiel"), conn = conn)
  expect_true("city" %in% DBI::dbListFields(conn, "simple"))
})

test_that("the stated price: drift from another process is noticed only after the ttl", {
  withr::local_options(shinyformtools.schema_probe_ttl = 30)
  sft_probe_cache_clear()
  withr::defer(sft_probe_cache_clear())

  now <- 1000
  testthat::local_mocked_bindings(sft_probe_clock = function() now)

  db_path <- tempfile(fileext = ".sqlite")
  conn <- local_test_conn(db_path)
  f <- test_form_basic(db = db_sqlite(db_path))
  insert_record(f, list(name = "Ada", email = "a@b.c"), conn = conn)

  # "Another process" drops the unique index behind the package's back.
  DBI::dbExecute(conn, "DROP INDEX uq_simple__email")
  expect_false(sft_schema_is_current(conn, f))

  # Inside the window the cached answer stands ...
  fetch_records(f, conn = conn)
  expect_false("uq_simple__email" %in% sft_list_index_names(conn, "simple"))

  # ... after it, the probe runs again and the schema is healed.
  now <- 1031
  fetch_records(f, conn = conn)
  expect_true("uq_simple__email" %in% sft_list_index_names(conn, "simple"))
})

test_that("an in-memory database is never cached, and a bad ttl means off", {
  withr::local_options(shinyformtools.schema_probe_ttl = 30)
  sft_probe_cache_clear()

  mem <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(mem))
  expect_null(sft_probe_cache_key(mem, test_form_basic()))

  for (bad in list("30", NA_real_, -5, c(1, 2))) {
    withr::local_options(shinyformtools.schema_probe_ttl = bad)
    expect_identical(sft_probe_ttl(), 0)
  }
})
