# What a user experiences when the database refuses a connection (MariaDB at
# max_connections). Measured live with tools/load-test/connection_ceiling.R:
# before these guards a save lost its input and killed the submit observer,
# and a new session died with the driver's message. db_connect() is mocked so
# the refusal can be switched on and off; everything else is the real module.

sft_test_ceiling_form <- function(db_path) {
  form(
    form_id = "ceiling",
    table_name = "ceiling",
    db = db_sqlite(db_path),
    fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
  )
}

sft_local_flaky_connect <- function(env = parent.frame()) {
  switchboard <- new.env()
  switchboard$refuse <- FALSE
  real_connect <- db_connect

  testthat::local_mocked_bindings(
    db_connect = function(...) {
      if (isTRUE(switchboard$refuse)) {
        stop("Failed to connect: Too many connections", call. = FALSE)
      }
      real_connect(...)
    },
    .env = env
  )

  switchboard
}

sft_local_notifications <- function(env = parent.frame()) {
  log <- new.env()
  log$shown <- character()

  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      log$shown <- c(log$shown, paste0(type, ": ", as.character(ui)))
      invisible(NULL)
    },
    .package = "shiny",
    .env = env
  )

  log
}

test_that("a save survives a refused reconnect: message shown, next save works", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_ceiling_form(db_path)
  net <- sft_local_flaky_connect()
  log <- sft_local_notifications()

  shiny::testServer(
    form_server,
    args = list(id = "ceiling", form = f, user = function() "alice"),
    {
      session$flushReact()

      # The session loses its connection while the server is full.
      DBI::dbDisconnect(state$handle)
      net$refuse <- TRUE

      session$setInputs(add_name = "Ada")
      session$setInputs(submit_add = 1)
      expect_true(any(grepl("^error: .*Too many connections", log$shown)))
      expect_false(any(grepl("^message: ", log$shown)))

      # Capacity returns: the SAME observer saves. It used to be dead.
      net$refuse <- FALSE
      log$shown <- character()
      session$setInputs(submit_add = 2)

      expect_true(any(grepl("^message: Record added", log$shown)))
      expect_identical(fetch_records(f, conn = state$conn())$name, "Ada")
    }
  )
})

test_that("a session that cannot connect at start-up stays up and recovers", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_ceiling_form(db_path)
  net <- sft_local_flaky_connect()
  log <- sft_local_notifications()
  net$refuse <- TRUE

  expect_no_error(
    shiny::testServer(
      form_server,
      args = list(id = "ceiling", form = f, user = function() "alice"),
      {
        session$flushReact()
        expect_null(state$handle)
        expect_true(any(grepl("^error: The database cannot be reached right now [(]Failed to connect", log$shown)))

        # A read fails cleanly while the refusal lasts ...
        expect_error(state$records(), "Too many connections")

        # ... and the first action after it ends connects, creates the schema
        # and saves.
        net$refuse <- FALSE
        log$shown <- character()
        session$setInputs(add_name = "Grace")
        session$setInputs(submit_add = 1)

        expect_true(any(grepl("^message: Record added", log$shown)))
        expect_true(DBI::dbIsValid(state$handle))
        state$refresh()
        expect_identical(state$records()$name, "Grace")
      }
    )
  )
})

test_that("a column-view action under a refused connection costs the action, not the session", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_ceiling_form(db_path)
  net <- sft_local_flaky_connect()
  log <- sft_local_notifications()

  shiny::testServer(
    form_server,
    args = list(id = "ceiling", form = f, user = function() "alice"),
    {
      session$flushReact()
      DBI::dbDisconnect(state$handle)
      net$refuse <- TRUE

      expect_no_error(session$setInputs(open_column_selection = 1))
      expect_true(any(grepl("^error: .*Too many connections", log$shown)))

      # The guard reports; it does not swallow success.
      expect_true(state$guard(function() TRUE))
      expect_false(state$guard(function() stop("boom")))
    }
  )
})

test_that("a caller-supplied connection is never reopened by the module", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_ceiling_form(db_path)
  conn <- local_test_conn(db_path)
  net <- sft_local_flaky_connect()
  net$refuse <- TRUE          # any attempt to connect would now throw

  shiny::testServer(
    form_server,
    args = list(id = "ceiling", form = f, conn = conn),
    {
      session$flushReact()
      expect_identical(state$conn(), conn)
      expect_no_error(state$records())
    }
  )
})
