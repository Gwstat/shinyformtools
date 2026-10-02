# Load test, part A: write contention.
#
# N separate R processes write through the package at the same time, in two
# profiles:
#   hot     every worker updates THE SAME record. version_no is MAX + 1 per
#           record, so this is where writers actually collide and the
#           retry-on-conflict machinery (max 5 attempts) is exercised.
#   spread  every worker updates its own record: no logical contention, shows
#           throughput.
#
# PASS is a hard line, not a metric: ZERO lost writes. Every update that
# reported success must be in the audit log, version numbers per record must be
# contiguous with no duplicate, and the record must hold the last writer's
# value. Errors are counted by kind; an update that FAILS LOUDLY is not a lost
# write (the caller knows), a success that is missing from the log is.
#
# Runs in the test container against a throwaway MariaDB server (see
# connection_ceiling.R for the docker commands). Prints aggregates only.
#   SFT_LOAD_WORKERS  comma-separated worker counts (default 4,8,16,32)
#   SFT_LOAD_OPS      updates per worker (default 50)

suppressMessages(devtools::load_all("/pkg", quiet = TRUE))

host <- Sys.getenv("SFT_LOAD_HOST", "sft-load-db")
worker_counts <- as.integer(strsplit(Sys.getenv("SFT_LOAD_WORKERS", "4,8,16,32"), ",")[[1]])
ops <- as.integer(Sys.getenv("SFT_LOAD_OPS", "50"))

admin <- DBI::dbConnect(RMariaDB::MariaDB(), host = host, user = "root", password = "root", dbname = "zz_load")
invisible(DBI::dbExecute(admin, "CREATE USER IF NOT EXISTS 'app'@'%' IDENTIFIED BY 'app'"))
invisible(DBI::dbExecute(admin, "GRANT ALL PRIVILEGES ON zz_load.* TO 'app'@'%'"))

say <- function(...) cat(sprintf(...), "\n", sep = "")
run_id <- format(Sys.time(), "%H%M%S")

make_form <- function(table_name) {
  form(
    form_id = table_name, table_name = table_name,
    db = db_mariadb("zz_load", host = host, user = "app", password = "app"),
    fields = list(form_field("name", "Name"), form_field("counter", "Counter", input_type = "numericInput"))
  )
}

# Runs in each worker process.
worker <- function(worker_id, table_name, record_id, ops, host) {
  suppressMessages(pkgload::load_all("/pkg", quiet = TRUE))
  f <- form(
    form_id = table_name, table_name = table_name,
    db = db_mariadb("zz_load", host = host, user = "app", password = "app"),
    fields = list(form_field("name", "Name"), form_field("counter", "Counter", input_type = "numericInput"))
  )
  conn <- db_connect(f$db)
  on.exit(db_disconnect(conn), add = TRUE)

  errors <- character()
  durations <- numeric()
  ok <- 0L

  for (i in seq_len(ops)) {
    t0 <- Sys.time()
    r <- tryCatch(
      {
        update_record(
          f, record_id = record_id,
          values = list(name = sprintf("w%02d-%03d", worker_id, i), counter = i),
          conn = conn, user = sprintf("w%02d", worker_id)
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    durations <- c(durations, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (is.null(r)) ok <- ok + 1L else errors <- c(errors, r)
  }

  list(ok = ok, errors = errors, durations = durations)
}

# Reduce an error message to its kind: the server code if there is one.
error_kind <- function(messages) {
  vapply(messages, function(message) {
    code <- regmatches(message, regexpr("[[][0-9]{4}[]]", message))
    if (length(code) > 0L) code else substr(gsub("[[:space:]]+", " ", message), 1, 60)
  }, character(1), USE.NAMES = FALSE)
}

run <- function(profile, n) {
  # Unique per run: a table left over from an earlier run would add its audit
  # rows to this one's and fake a negative "lost" count.
  table_name <- sprintf("zz_%s_%02d_%s", profile, n, run_id)
  f <- make_form(table_name)
  setup <- db_connect(f$db)
  init_db(f, conn = setup)

  record_ids <- if (identical(profile, "hot")) {
    rep(insert_record(f, list(name = "start", counter = 0), conn = setup, user = "setup")$sft_id[1], n)
  } else {
    vapply(seq_len(n), function(i) {
      insert_record(f, list(name = "start", counter = 0), conn = setup, user = "setup")$sft_id[1]
    }, numeric(1))
  }
  db_disconnect(setup)

  cluster <- parallel::makePSOCKcluster(n)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  t0 <- Sys.time()
  results <- parallel::clusterMap(
    cluster, worker,
    worker_id = seq_len(n), record_id = record_ids,
    MoreArgs = list(table_name = table_name, ops = ops, host = host)
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ok <- sum(vapply(results, function(r) r$ok, integer(1)))
  errors <- unlist(lapply(results, function(r) r$errors))
  durations <- unlist(lapply(results, function(r) r$durations))

  # ---- the hard line ------------------------------------------------------
  audit <- DBI::dbGetQuery(
    admin,
    "SELECT record_id, version_no, action FROM sft_audit_log WHERE table_name = ? ORDER BY record_id, version_no",
    params = list(table_name)
  )
  per_record <- split(audit$version_no, audit$record_id)
  contiguous <- all(vapply(per_record, function(v) identical(as.integer(v), seq_along(v)), logical(1)))
  duplicates <- sum(vapply(per_record, function(v) sum(duplicated(v)), integer(1)))
  logged_updates <- sum(audit$action == "update")
  lost <- ok - logged_updates

  # The stored row must be the latest version the log describes.
  rows <- DBI::dbGetQuery(admin, sprintf("SELECT sft_id, name FROM `%s`", table_name))
  last_logged <- DBI::dbGetQuery(
    admin,
    "SELECT a.record_id, a.new_data_json FROM sft_audit_log a
       JOIN (SELECT record_id, MAX(version_no) AS v FROM sft_audit_log WHERE table_name = ? GROUP BY record_id) m
         ON a.record_id = m.record_id AND a.version_no = m.v
      WHERE a.table_name = ?",
    params = list(table_name, table_name)
  )
  row_matches_log <- all(vapply(seq_len(nrow(last_logged)), function(i) {
    stored <- rows$name[rows$sft_id == last_logged$record_id[i]]
    grepl(paste0("\"name\":\\[?\"", stored, "\""), last_logged$new_data_json[i])
  }, logical(1)))

  say("%-6s n=%2d | attempted %5d  ok %5d  failed %4d | lost %d  dup version_no %d  contiguous %s  row=log %s | %6.1fs  %6.0f ok/s | p50 %.0fms p95 %.0fms max %.0fms",
      profile, n, n * ops, ok, length(errors), lost, duplicates, contiguous, row_matches_log,
      wall, ok / wall,
      1000 * stats::median(durations), 1000 * stats::quantile(durations, 0.95), 1000 * max(durations))

  if (length(errors) > 0L) {
    kinds <- sort(table(error_kind(errors)), decreasing = TRUE)
    say("       errors: %s", paste(sprintf("%s x%d", names(kinds), as.integer(kinds)), collapse = "; "))
  }

  invisible(list(lost = lost, duplicates = duplicates, contiguous = contiguous, row_matches_log = row_matches_log))
}

say("MariaDB %s, %d updates per worker, retry max_attempts = 5",
    DBI::dbGetQuery(admin, "SELECT VERSION() AS v")$v, ops)

verdicts <- list()
for (profile in c("hot", "spread")) {
  for (n in worker_counts) {
    verdicts[[length(verdicts) + 1L]] <- run(profile, n)
  }
}

passed <- all(vapply(verdicts, function(v) v$lost == 0L && v$duplicates == 0L && v$contiguous && v$row_matches_log, logical(1)))
say("\nHARD LINE (no lost write, no duplicate or gap in version_no, row = last logged version): %s",
    if (passed) "PASS" else "FAIL")
DBI::dbDisconnect(admin)
