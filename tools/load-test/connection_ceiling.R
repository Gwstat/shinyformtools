# Load test, part B: the connection ceiling.
#
# form_server() opens one connection per Shiny session per form. This script
# does not measure the number (it is max_connections, by arithmetic) but the
# FAILURE MODE: what each kind of caller experiences once the server refuses.
#
# Runs inside the test container against a throwaway MariaDB server:
#   docker network create sft-load
#   docker run -d --name sft-load-db --network sft-load \
#     -e MARIADB_ROOT_PASSWORD=root -e MARIADB_DATABASE=zz_load mariadb:11
#   docker run --rm --network sft-load -v "<repo>:/pkg:ro" sft-test \
#     Rscript /pkg/tools/load-test/connection_ceiling.R
# Prints aggregates only.

suppressMessages(devtools::load_all("/pkg", quiet = TRUE))
suppressMessages(library(shiny))

host <- Sys.getenv("SFT_LOAD_HOST", "sft-load-db")
admin <- DBI::dbConnect(RMariaDB::MariaDB(), host = host, user = "root", password = "root", dbname = "zz_load")

# A non-SUPER application user: root gets one connection beyond max_connections,
# which would blur the ceiling.
invisible(DBI::dbExecute(admin, "CREATE USER IF NOT EXISTS 'app'@'%' IDENTIFIED BY 'app'"))
invisible(DBI::dbExecute(admin, "GRANT ALL PRIVILEGES ON zz_load.* TO 'app'@'%'"))

db <- db_mariadb("zz_load", host = host, user = "app", password = "app")
f <- form(
  form_id = "load", table_name = "load", db = db,
  fields = list(form_field("name", "Name", mandatory = TRUE))
)

setup <- db_connect(db)
init_db(f, conn = setup)
invisible(insert_record(f, list(name = "seed"), conn = setup, user = "setup"))
db_disconnect(setup)

max_conn <- as.integer(DBI::dbGetQuery(admin, "SELECT @@max_connections AS n")$n)
threads <- function() as.integer(DBI::dbGetQuery(admin, "SHOW STATUS LIKE 'Threads_connected'")$Value)
say <- function(...) cat(sprintf(...), "\n", sep = "")
outcome <- function(expr) {
  t0 <- Sys.time()
  r <- tryCatch({ force(expr); "ok" }, error = function(e) paste0("ERROR: ", gsub("[[:space:]]+", " ", conditionMessage(e))))
  sprintf("%s  [%.2fs]", r, as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

say("server max_connections = %d, connected before test = %d", max_conn, threads())

shown <- character()
testthat::local_mocked_bindings(
  showNotification = function(ui, ..., type = "default") {
    shown <<- c(shown, paste0(type, ": ", gsub("[[:space:]]+", " ", as.character(ui))))
    invisible(NULL)
  },
  .package = "shiny"
)

filler <- list()

fill <- function() {
  refusal <- NULL
  repeat {
    r <- tryCatch(db_connect(db), error = function(e) e)
    if (inherits(r, "error")) { refusal <- r; break }
    filler[[length(filler) + 1L]] <<- r
    if (length(filler) > max_conn + 20L) break
  }
  refusal
}

release <- function(n = length(filler)) {
  for (conn in utils::head(filler, n)) try(db_disconnect(conn), silent = TRUE)
  filler <<- utils::tail(filler, -n)
}

# A scenario runs in its own module session. An error that escapes an observer
# ends a Shiny session, so "the session died" is itself a result.
scenario <- function(title, id, body) {
  say("\n== %s", title)
  r <- tryCatch(
    {
      # `body` is a quoted block; do.call() hands it to testServer() as the
      # expression itself rather than as the symbol `body`.
      do.call(
        shiny::testServer,
        list(app = form_server, expr = body, args = list(id = id, form = f, user = function() "alice"))
      )
      "session survived"
    },
    error = function(e) paste0("SESSION DIED: ", gsub("[[:space:]]+", " ", conditionMessage(e)))
  )
  say("   -> %s", r)
  release()
}

save_in_session <- function(session, n, name) {
  shown <<- character()
  session$setInputs(add_name = name)
  session$setInputs(submit_add = n)
  if (length(shown)) paste(shown, collapse = " | ") else "(no notification)"
}

# ---- A. a running session while the server fills up -------------------------
scenario("A. running session, server fills up around it", "a", quote({
  session$flushReact()
  invisible(state$records())
  t0 <- Sys.time()
  refusal <- fill()
  say("   opened %d extra connections in %.1fs; server reports %d connected",
      length(filler), as.numeric(difftime(Sys.time(), t0, units = "secs")), threads())
  say("   refusal: %s (%s)", gsub("[[:space:]]+", " ", conditionMessage(refusal)), class(refusal)[1])
  say("   read : %s", outcome(state$records()))
  say("   save : %s", save_in_session(session, 1, "A at the ceiling"))
  say("   script call without conn: %s", outcome(insert_record(f, list(name = "A standalone"), user = "script")))
}))

# ---- B. connection dropped while the server is full: READ -------------------
scenario("B. dropped connection + full server, then a read", "b", quote({
  session$flushReact()
  invisible(state$records())
  thread <- format(DBI::dbGetQuery(state$handle, "SELECT CONNECTION_ID() AS id")$id, scientific = FALSE)
  fill()
  invisible(DBI::dbExecute(admin, paste("KILL", thread)))
  fill()                                  # the freed slot is taken at once
  state$refresh()
  # The flush matters: it runs the observers that consume the read (the table
  # proxy sync), and an error escaping one of them ends the session.
  say("   refresh + flush while full: %s", outcome(session$flushReact()))
  say("   read while full : %s", outcome(state$records()))
  release(10)
  state$refresh()
  say("   refresh + flush after 10 freed: %s", outcome(session$flushReact()))
  say("   read after 10 freed: %s", outcome(state$records()))
  say("   save after 10 freed: %s", save_in_session(session, 1, "B recovered"))
}))

# ---- C. connection dropped while the server is full: SAVE -------------------
scenario("C. dropped connection + full server, then a save", "c", quote({
  session$flushReact()
  invisible(state$records())
  thread <- format(DBI::dbGetQuery(state$handle, "SELECT CONNECTION_ID() AS id")$id, scientific = FALSE)
  fill()
  invisible(DBI::dbExecute(admin, paste("KILL", thread)))
  fill()
  say("   save while full : %s", save_in_session(session, 1, "C while full"))
  release(10)
  say("   save after 10 freed: %s", save_in_session(session, 2, "C recovered"))
}))

# ---- D. a new session arriving at the ceiling --------------------------------
fill()
scenario("D. a NEW session starting while the server is full", "d", quote({
  session$flushReact()
  say("   started; read: %s", outcome(state$records()))
}))

stored <- DBI::dbGetQuery(admin, "SELECT name FROM `load` ORDER BY sft_id")$name
say("\nrows stored: %s", paste(stored, collapse = ", "))
say("connected after cleanup = %d", threads())
DBI::dbDisconnect(admin)
