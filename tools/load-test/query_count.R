# Load test, part C: what one operation costs in round trips.
#
# Every CRUD call first runs the schema probe (sft_schema_is_current). Locally
# a query is sub-millisecond and none of this matters; against a remote server
# at ~20 ms per round trip the COUNT is the latency. This script counts the
# statements each operation sends (MariaDB's per-session `Questions` counter)
# and lists the probe's statements from the general log, so a redundant one is
# visible by name. No artificial latency: multiply by your own.
#
# Runs against a throwaway MariaDB server, from the host or the test container:
#   docker run -d --name sft-load-db -p 3399:3306 \
#     -e MARIADB_ROOT_PASSWORD=root -e MARIADB_DATABASE=zz_load mariadb:11
#   SFT_LOAD_HOST=127.0.0.1 SFT_LOAD_PORT=3399 Rscript tools/load-test/query_count.R

pkg <- Sys.getenv("SFT_LOAD_PKG", ".")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

# SFT_LOAD_PROBE_TTL=30 measures the opt-in probe cache
# (options(shinyformtools.schema_probe_ttl = )); unset = the default, off.
ttl <- suppressWarnings(as.numeric(Sys.getenv("SFT_LOAD_PROBE_TTL", "0")))
if (!is.na(ttl) && ttl > 0) options(shinyformtools.schema_probe_ttl = ttl)

host <- Sys.getenv("SFT_LOAD_HOST", "sft-load-db")
port <- as.integer(Sys.getenv("SFT_LOAD_PORT", "3306"))
say <- function(...) cat(sprintf(...), "\n", sep = "")

admin <- DBI::dbConnect(RMariaDB::MariaDB(), host = host, port = port, user = "root", password = "root", dbname = "zz_load")
invisible(DBI::dbExecute(admin, "CREATE USER IF NOT EXISTS 'app'@'%' IDENTIFIED BY 'app'"))
invisible(DBI::dbExecute(admin, "GRANT ALL PRIVILEGES ON zz_load.* TO 'app'@'%'"))

table_name <- paste0("zz_count_", format(Sys.time(), "%H%M%S"))
f <- form(
  form_id = table_name, table_name = table_name,
  db = db_mariadb("zz_load", host = host, port = port, user = "app", password = "app"),
  fields = list(
    form_field("name", "Name", mandatory = TRUE),
    form_field("email", "Email", unique = TRUE)
  )
)

conn <- db_connect(f$db)
init_db(f, conn = conn)

questions <- function() as.integer(DBI::dbGetQuery(conn, "SHOW SESSION STATUS LIKE 'Questions'")$Value)

# Statements `expr` sends on `conn`. Reading the counter is itself a statement;
# the calibration run measures that overhead once and subtracts it.
count_raw <- function(expr) {
  before <- questions()
  force(expr)
  questions() - before
}
overhead <- count_raw(NULL)
count <- function(expr) count_raw(expr) - overhead

rec <- insert_record(f, list(name = "Ada", email = "ada@example.com"), conn = conn, user = "t")
id <- rec$sft_id[1]

results <- list(
  "schema probe alone" = count(sft_schema_is_current(conn, f)),
  "fetch_records" = count(fetch_records(f, conn = conn)),
  "fetch_audit_log (one record)" = count(fetch_audit_log(f, conn = conn, record_id = id)),
  "insert_record" = count(insert_record(f, list(name = "Bob", email = "bob@example.com"), conn = conn, user = "t")),
  "update_record" = count(update_record(f, record_id = id, values = list(name = "Ada L."), conn = conn, user = "t")),
  "update_record + conflict check" = count(update_record(
    f, record_id = id, values = list(name = "Ada Lovelace"), conn = conn, user = "t",
    expected_record = fetch_records(f, conn = db_connect(f$db))[1, ]
  )),
  "soft_delete_record" = count(soft_delete_record(f, record_id = id, conn = conn, user = "t")),
  "restore_record" = count(restore_record(f, record_id = id, conn = conn, user = "t")),
  "sft_live_connection (reconnect probe)" = count(sft_live_connection(conn, f$db))
)

probe <- results[["schema probe alone"]]
say("MariaDB %s - round trips per operation (one connection, schema current, probe ttl = %s)",
    DBI::dbGetQuery(admin, "SELECT VERSION() AS v")$v, getOption("shinyformtools.schema_probe_ttl", 0))
say("%-40s %5s %12s", "operation", "total", "of it probe")
for (name in names(results)) {
  say("%-40s %5d %12s", name, results[[name]],
      if (getOption("shinyformtools.schema_probe_ttl", 0) > 0 ||
          name %in% c("schema probe alone", "sft_live_connection (reconnect probe)")) {
        "-"
      } else {
        sprintf("%d (%.0f%%)", probe, 100 * probe / results[[name]])
      })
}

# ---- what exactly the probe sends ------------------------------------------
invisible(DBI::dbExecute(admin, "SET GLOBAL log_output = 'TABLE'"))
invisible(DBI::dbExecute(admin, "TRUNCATE TABLE mysql.general_log"))
thread <- format(DBI::dbGetQuery(conn, "SELECT CONNECTION_ID() AS id")$id, scientific = FALSE)
invisible(DBI::dbExecute(admin, "SET GLOBAL general_log = 'ON'"))
invisible(sft_schema_is_current(conn, f))
invisible(DBI::dbExecute(admin, "SET GLOBAL general_log = 'OFF'"))

log <- DBI::dbGetQuery(
  admin,
  paste0("SELECT CONVERT(argument USING utf8mb4) AS statement FROM mysql.general_log ",
         "WHERE thread_id = ", thread, " AND command_type IN ('Query', 'Prepare', 'Execute') ORDER BY event_time")
)
statements <- gsub("[[:space:]]+", " ", log$statement)
statements <- statements[!grepl("CONNECTION_ID", statements)]
say("\nthe probe's statements, in order (%d logged events):", length(statements))
for (s in unique(statements)) say("  x%d  %s", sum(statements == s), substr(s, 1, 110))

db_disconnect(conn)
invisible(DBI::dbExecute(admin, sprintf("DROP TABLE IF EXISTS `%s`", table_name)))
DBI::dbDisconnect(admin)
