#' Define a SQLite database backend
#'
#' @param path Path to the SQLite database file.
#'
#' @return A shinyformtools database configuration.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' db$type
#' @export
db_sqlite <- function(path = "form_data.sqlite") {
  if (!sft_is_scalar_character(path)) {
    stop("path must be a non-empty character scalar.", call. = FALSE)
  }

  structure(
    list(
      type = "sqlite",
      path = path
    ),
    class = c("sft_db_config", "list")
  )
}


#' Define a DuckDB database backend
#'
#' Number fields are stored as `DOUBLE`. Tables created by earlier versions
#' used DuckDB's 4-byte `REAL` (about seven significant digits); their number
#' columns are widened to `DOUBLE` automatically on first access, without loss.
#'
#' @param path Path to the DuckDB database file. Use `":memory:"` for an
#'   in-memory database.
#' @param read_only Logical. Whether to open the database in read-only mode.
#' @param config Named list of DuckDB connection configuration options passed
#'   to [DBI::dbConnect()].
#'
#' @return A shinyformtools database configuration.
#' @examples
#' # Builds a configuration object only; it does not open a connection.
#' db <- db_duckdb(":memory:")
#' db$type
#' @export
db_duckdb <- function(path = "form_data.duckdb",
                          read_only = FALSE,
                          config = list()) {
  if (!sft_is_scalar_character(path)) {
    stop("path must be a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_logical(read_only)) {
    stop("read_only must be a logical scalar.", call. = FALSE)
  }

  if (!is.list(config)) {
    stop("config must be a named list.", call. = FALSE)
  }

  if (length(config) > 0L && (is.null(names(config)) || any(!nzchar(names(config))))) {
    stop("config must be a named list.", call. = FALSE)
  }

  structure(
    list(
      type = "duckdb",
      path = path,
      read_only = read_only,
      config = config
    ),
    class = c("sft_db_config", "list")
  )
}

#' Define a MariaDB database backend
#'
#' @section How MariaDB differs from SQLite:
#' The same form behaves slightly differently on a MariaDB server than it does
#' on SQLite. None of this needs configuring, but it is worth knowing:
#'
#' \itemize{
#'   \item **Uniqueness ignores case and accents.** MariaDB's default collations
#'     are accent- and case-insensitive, so a field declared `unique = TRUE`
#'     treats `"MULLER@example.com"` and `"muller@example.com"` as the same
#'     value -- and likewise two spellings that differ only by an umlaut, or
#'     only by spaces at the end. SQLite and DuckDB compare exactly, so the
#'     identical form accepts both there. Uniqueness is therefore stricter on
#'     MariaDB, never looser. The same holds for the keys of
#'     [upsert_records()].
#'   \item **Unique text fields are capped at 255 characters on older servers.**
#'     MariaDB 10.4 and newer index an unbounded `TEXT` column directly. Older
#'     MariaDB and MySQL cannot, so the column is created as `VARCHAR(255)`
#'     instead -- otherwise the unique constraint could not exist at all.
#'   \item **Schema changes are not transactional.** MariaDB commits each
#'     `CREATE`/`ALTER` statement as it runs, so a migration that fails halfway
#'     leaves the completed steps in place. [apply_migration()] therefore runs
#'     the plan directly on MariaDB rather than pretending a transaction would
#'     protect it. On SQLite and DuckDB the plan does roll back as one unit.
#'   \item **Tables are created as utf8mb4.** Without this they would inherit
#'     the server default, which on MariaDB 10.5 and older is `latin1` -- and a
#'     `latin1` column rejects (or worse, silently truncates) anything outside
#'     Western European text. Tables that already exist are left as they are;
#'     converting one rewrites stored data and is a deliberate manual step.
#'   \item **Databases created by an older version of this package are widened.**
#'     The columns holding JSON payloads were once `TEXT`, which MariaDB caps at
#'     64KB; they are altered to `MEDIUMTEXT` whenever the schema is reconciled,
#'     and always by an explicit [init_db()]. Note that reconciliation is
#'     triggered by the *form* changing, and the system tables are not part of
#'     that signature -- so on a database whose form is already current, run
#'     [init_db()] once after upgrading the package to be sure.
#'   \item **Every session holds one connection per database.** The forms
#'     of a session share it (since 0.5.0; before, each form opened its own),
#'     so a page with three forms costs one of the server's `max_connections`
#'     (151 by default) per user. A server that
#'     is full refuses new connections at once; running sessions keep working,
#'     and a session that cannot connect tells the user and tries again on the
#'     next action instead of failing.
#'   \item **A remote server makes the schema check expensive.** Before every
#'     database call the package checks that the schema matches the form. That
#'     is about 12 network round trips here (the driver turns each statement
#'     into prepare, execute and commit): unnoticeable on the same machine,
#'     around a quarter of a second per call at 20 ms latency. Set
#'     `options(shinyformtools.schema_probe_ttl = 30)` to remember a passed
#'     check for 30 seconds per database and form definition; a read then
#'     costs 3 round trips instead of 15. The price: a schema change made by
#'     another process is noticed up to that many seconds late. Off by
#'     default.
#' }
#'
#' MySQL is not supported. This function will connect to it, because the
#' protocol is the same, but the package is neither tested nor fixed against it:
#' a field carrying a `db_default` on a text column cannot be created there.
#'
#' @param dbname Database name.
#' @param host Database host.
#' @param port Database port.
#' @param user Database user name. If omitted, `SFT_MARIADB_USER` is used.
#' @param password Database password. If omitted, `SFT_MARIADB_PASSWORD` is used.
#' @param ... Additional arguments passed to [DBI::dbConnect()].
#'
#' @return A shinyformtools database configuration.
#' @examples
#' # Builds a configuration object only; it does not connect to a server.
#' db <- db_mariadb(
#'   dbname = "app_data",
#'   host = "127.0.0.1",
#'   port = 3306,
#'   user = "demo",
#'   password = "secret"
#' )
#' db$type
#' @export
db_mariadb <- function(dbname,
                           host = "127.0.0.1",
                           port = 3306L,
                           user = Sys.getenv("SFT_MARIADB_USER", unset = NA_character_),
                           password = Sys.getenv("SFT_MARIADB_PASSWORD", unset = NA_character_),
                           ...) {
  if (!sft_is_scalar_character(dbname)) {
    stop("dbname must be a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_character(host)) {
    stop("host must be a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_number(as.numeric(port))) {
    stop("port must be a numeric scalar.", call. = FALSE)
  }

  if (!is.character(user) || length(user) != 1L) {
    stop("user must be a character scalar or NA.", call. = FALSE)
  }

  if (!is.character(password) || length(password) != 1L) {
    stop("password must be a character scalar or NA.", call. = FALSE)
  }

  structure(
    list(
      type = "mariadb",
      dbname = dbname,
      host = host,
      port = as.integer(port),
      user = user,
      password = password,
      args = list(...)
    ),
    class = c("sft_db_config", "list")
  )
}

sft_validate_db_config <- function(db) {
  if (is.character(db) && length(db) == 1L && !is.na(db)) {
    return(invisible(db_sqlite(db)))
  }

  if (!inherits(db, "sft_db_config")) {
    stop(
      "db must be created with db_sqlite(), db_mariadb() or db_duckdb().",
      call. = FALSE
    )
  }

  if (!db$type %in% c("sqlite", "mariadb", "duckdb")) {
    stop("Unsupported database backend: ", db$type, ".", call. = FALSE)
  }

  invisible(db)
}

sft_redact_db_config <- function(db) {
  if (!inherits(db, "sft_db_config")) {
    return(db)
  }

  out <- db

  if (identical(out$type, "mariadb")) {
    out$password <- if (is.na(out$password) || !nzchar(out$password)) {
      NA_character_
    } else {
      "<redacted>"
    }
  }

  out
}

# How long SQLite retries a locked database before giving up, in milliseconds.
sft_sqlite_busy_timeout_ms <- 5000L

# Without this, SQLite gives up the instant it meets another writer's lock and
# raises "database is locked" - it waits zero milliseconds by default. Writes
# take milliseconds, so waiting briefly turns nearly every collision into a
# successful write. Measured with 4 processes x 25 concurrent inserts into one
# file: 23/100 written as shipped, 100/100 with this pragma (0 duplicate ids
# either way - record ids were never the problem).
#
# Deliberately NOT journal_mode = WAL: it was measured on the same test and
# changed nothing (19/100), because WAL keeps readers from blocking writers
# while our contention is writer against writer, which only patience fixes. WAL
# would add -wal/-shm files and break on network shares for no benefit here.
sft_set_sqlite_busy_timeout <- function(conn) {
  DBI::dbExecute(
    conn,
    paste("PRAGMA busy_timeout =", sft_sqlite_busy_timeout_ms)
  )

  invisible(conn)
}

#' Connect to a database
#'
#' @param db Database configuration created with `db_sqlite()` or
#'   `db_mariadb()` or `db_duckdb()`. A character scalar is
#'   treated as SQLite path for backwards compatibility.
#'
#' @return A DBI connection.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' conn <- db_connect(db)
#' DBI::dbIsValid(conn)
#' db_disconnect(conn)
#' @export
db_connect <- function(db = db_sqlite()) {
  if (is.character(db) && length(db) == 1L && !is.na(db)) {
    db <- db_sqlite(db)
  }

  sft_validate_db_config(db)

  if (identical(db$type, "sqlite")) {
    db_dir <- dirname(db$path)

    if (!identical(db_dir, ".") && !dir.exists(db_dir)) {
      dir.create(db_dir, recursive = TRUE)
    }

    conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db$path)
    sft_set_sqlite_busy_timeout(conn)

    return(conn)
  }

  if (identical(db$type, "duckdb")) {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop(
        "Package 'duckdb' is required for DuckDB connections. ",
        "Install it with install.packages('duckdb').",
        call. = FALSE
      )
    }

    db_dir <- dirname(db$path)

    if (
      !identical(db$path, ":memory:") &&
        !identical(db_dir, ".") &&
        !dir.exists(db_dir)
    ) {
      dir.create(db_dir, recursive = TRUE)
    }

    return(
      DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = db$path,
        read_only = db$read_only,
        config = db$config
      )
    )
  }

  if (identical(db$type, "mariadb")) {
    if (!requireNamespace("RMariaDB", quietly = TRUE)) {
      stop(
        "Package 'RMariaDB' is required for MariaDB connections. ",
        "Install it with install.packages('RMariaDB').",
        call. = FALSE
      )
    }

    user <- if (is.na(db$user)) NULL else db$user
    password <- if (is.na(db$password)) NULL else db$password

    return(
      do.call(
        DBI::dbConnect,
        c(
          list(
            drv = RMariaDB::MariaDB(),
            dbname = db$dbname,
            host = db$host,
            port = db$port,
            username = user,
            password = password
          ),
          db$args
        )
      )
    )
  }

  stop("Unsupported database backend: ", db$type, ".", call. = FALSE)
}

#' Disconnect from a database
#'
#' @param conn A DBI connection.
#'
#' @return Invisibly returns `TRUE` if a connection was closed.
#' @examples
#' conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
#' db_disconnect(conn)
#' @export
db_disconnect <- function(conn) {
  if (DBI::dbIsValid(conn)) {
    if (sft_is_duckdb_connection(conn)) {
      DBI::dbDisconnect(conn, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(conn)
    }

    return(invisible(TRUE))
  }

  invisible(FALSE)
}

# Resolve an optional database connection.
#
# When `conn` is supplied it is returned unchanged (the caller owns it). When
# `conn` is NULL a new connection is opened from `form$db` and its disconnect is
# registered on the calling function's exit, so the connection is owned and
# closed exactly as the previous inline idiom did:
#
#   owns_connection <- is.null(conn)
#   if (owns_connection) {
#     conn <- db_connect(form$db)
#     on.exit(db_disconnect(conn), add = TRUE)
#   }
#
# `on.exit()` is registered in `envir` (the caller's frame by default) rather
# than this helper's frame, otherwise the connection would be closed the moment
# this function returned. Internal helper; not exported.
sft_resolve_connection <- function(form, conn = NULL, envir = parent.frame()) {
  if (!is.null(conn)) {
    return(conn)
  }

  conn <- db_connect(form$db)
  do.call(
    on.exit,
    list(as.call(list(quote(db_disconnect), conn)), add = TRUE),
    envir = envir
  )
  conn
}

# One connection per database and Shiny session, shared by every module of
# the session that opens its own: form_server() and shinygridtools' grid_server() used to open
# one each, so a page with six forms cost six connections per user, and a
# MariaDB server with a connection limit refused sessions. Shiny runs one
# session's observers one at a time, so the modules never use the connection
# at the same moment and a transaction of one cannot interleave with another.
# The connection is closed when the session ends. `entry` is an environment
# with `handle` (NULL when the database refused) and `error`.
# options(shinyformtools.share_connections = FALSE) restores one per module.
sft_share_connections <- function() {
  !identical(getOption("shinyformtools.share_connections", TRUE), FALSE)
}

sft_db_identity <- function(db) {
  values <- unlist(db[setdiff(names(db), "password")])
  paste(names(values), values, sep = "=", collapse = "|")
}

sft_session_connection <- function(session, db) {
  store <- session$userData$sft_connections

  if (is.null(store)) {
    store <- new.env(parent = emptyenv())
    session$userData$sft_connections <- store
    session$onSessionEnded(function() {
      for (key in ls(store, all.names = TRUE)) {
        handle <- store[[key]]$handle
        if (!is.null(handle)) {
          try(db_disconnect(handle), silent = TRUE)
        }
      }
    })
  }

  key <- sft_db_identity(db)
  entry <- store[[key]]

  if (is.null(entry)) {
    entry <- new.env(parent = emptyenv())
    entry$error <- NULL
    entry$handle <- tryCatch(
      db_connect(db),
      error = function(err) {
        entry$error <- conditionMessage(err)
        NULL
      }
    )
    assign(key, entry, envir = store)
  }

  entry
}

# The shared connection, alive: opened if it is not yet, reopened if the
# server dropped it. Every module sees the new handle.
sft_session_connection_get <- function(entry, db) {
  entry$handle <- if (is.null(entry$handle)) db_connect(db) else sft_live_connection(entry$handle, db)
  entry$error <- NULL
  entry$handle
}

# Return a live connection for a caller that OWNS its connection. A connection
# closed by the server (e.g. MariaDB drops a long-idle Shiny session at
# wait_timeout) still looks valid to the client - DBI::dbIsValid() does not
# round-trip - so the only reliable detection is a trivial probe query. When it
# fails, a fresh connection is opened from `db`, which reapplies every
# per-connection setup step (e.g. the SQLite busy_timeout) because it routes
# through db_connect(). "SELECT 1" is backend-neutral (SQLite, DuckDB, MariaDB).
# The caller must reassign its held connection to the return value. Only ever
# call this for a self-opened connection: a caller-supplied one keeps its own
# lifecycle and must not be silently replaced. Internal.
sft_live_connection <- function(conn, db) {
  alive <- tryCatch(
    {
      DBI::dbGetQuery(conn, "SELECT 1")
      TRUE
    },
    error = function(e) FALSE
  )

  if (alive) {
    return(conn)
  }

  # The probe failed. Close the old handle before replacing it: usually it is
  # already dead (db_disconnect is then a no-op), but if the probe failed while
  # the handle was still open, disconnecting it prevents a leaked connection.
  db_disconnect(conn)
  db_connect(db)
}

sft_db_backend <- function(conn) {
  classes <- class(conn)

  if (any(grepl("SQLite", classes, ignore.case = TRUE))) {
    return("sqlite")
  }

  if (any(grepl("MariaDB|MySQL", classes, ignore.case = TRUE))) {
    return("mariadb")
  }

  if (any(grepl("DuckDB|duckdb", classes, ignore.case = TRUE))) {
    return("duckdb")
  }

  "unknown"
}

sft_is_mariadb_connection <- function(conn) {
  identical(sft_db_backend(conn), "mariadb")
}

sft_is_duckdb_connection <- function(conn) {
  identical(sft_db_backend(conn), "duckdb")
}

sft_requires_explicit_integer_id <- function(conn) {
  sft_is_duckdb_connection(conn)
}

# Whether DDL (CREATE/ALTER TABLE, CREATE/DROP INDEX) can roll back inside a
# transaction. SQLite and DuckDB have transactional DDL; MariaDB issues an
# implicit commit per DDL statement, so a transaction there gives false safety.
# Lets apply_migration apply a plan atomically only where it actually holds.
sft_supports_transactional_ddl <- function(conn) {
  !sft_is_mariadb_connection(conn)
}

sft_requires_explicit_sft_id <- function(conn) {
  sft_requires_explicit_integer_id(conn)
}

sft_auto_id_definition <- function(conn) {
  if (sft_is_mariadb_connection(conn)) {
    return("INTEGER AUTO_INCREMENT PRIMARY KEY")
  }

  if (sft_is_duckdb_connection(conn)) {
    return("INTEGER PRIMARY KEY")
  }

  "INTEGER PRIMARY KEY AUTOINCREMENT"
}

sft_short_text_definition <- function(conn) {
  if (sft_is_mariadb_connection(conn)) {
    return("VARCHAR(255)")
  }

  if (sft_is_duckdb_connection(conn)) {
    return("VARCHAR")
  }

  "TEXT"
}

# Column type for unbounded text (JSON payloads: config_json, audit snapshots).
# On SQLite/DuckDB TEXT is unbounded, but MariaDB caps TEXT at 64KB and, with
# the strict sql_mode default (since 10.2), errors on overflow - a form with
# large choice lists or a fat audit snapshot would fail to save. MEDIUMTEXT
# (16MB) removes that ceiling. This matters at CREATE time: the system tables
# are CREATE IF NOT EXISTS and are never migrated, so a database initialised
# before this change keeps TEXT until altered by hand.
sft_long_text_definition <- function(conn) {
  if (sft_is_mariadb_connection(conn)) {
    return("MEDIUMTEXT")
  }

  "TEXT"
}

# Whether the server can put a UNIQUE index on an unbounded TEXT column.
#
# MariaDB >= 10.4 does it transparently by creating a UNIQUE HASH index (live
# check: SHOW INDEX reports Index_type HASH). Everything older - and real MySQL
# at any version - rejects it with "BLOB/TEXT column used in key specification
# without a key length [1170]", reproduced live on MariaDB 10.3.39 and MySQL
# 8.4.10. Where it is not supported, a unique TEXT field's column is created as
# VARCHAR(255) instead (see sft_field_db_definition), which is indexable
# everywhere.
sft_supports_unique_text_index <- function(conn) {
  if (!sft_is_mariadb_connection(conn)) {
    return(TRUE)
  }

  version <- tryCatch(
    as.character(DBI::dbGetQuery(conn, "SELECT VERSION() AS version")$version[1]),
    # An unreadable version means the conservative answer: VARCHAR(255) works on
    # every server, an unindexable TEXT column works on none.
    error = function(e) ""
  )

  sft_mariadb_supports_unique_text(version)
}

# The version test behind sft_supports_unique_text_index(), split out so it can
# be exercised without a server. `version` is what SELECT VERSION() returns,
# e.g. "10.3.39-MariaDB-1:10.3.39+maria~ubu2004" or "8.4.10" for MySQL.
sft_mariadb_supports_unique_text <- function(version) {
  version <- as.character(version %||% "")

  if (!grepl("mariadb", version, ignore.case = TRUE)) {
    return(FALSE)
  }

  # Some MariaDB builds prefix the version with "5.5.5-" so that very old
  # clients keep connecting; strip it before reading the real numbers.
  version <- sub("^5\\.5\\.5-", "", version)

  numbers <- regmatches(version, regexpr("^[0-9]+\\.[0-9]+", version))

  if (length(numbers) != 1L) {
    return(FALSE)
  }

  parts <- as.integer(strsplit(numbers, ".", fixed = TRUE)[[1]])

  isTRUE(parts[1] > 10L || (parts[1] == 10L && parts[2] >= 4L))
}

# Clause appended to every CREATE TABLE the package issues.
#
# Without it, MariaDB/MySQL tables inherit the server's default charset - and
# MariaDB <= 10.5 defaults to latin1. Verified live on 10.3.39: a database
# created with a plain CREATE DATABASE produced latin1_swedish_ci tables, where
# umlauts still round-tripped (they exist in latin1) but CJK and emoji inserts
# were rejected with "Incorrect string value ... [1366]" - and without the strict
# sql_mode they would have been silently truncated instead. Pinning utf8mb4
# makes the schema independent of how the database happened to be created.
#
# This affects tables created from here on. An existing latin1 table is left
# alone: converting one is an ALTER TABLE ... CONVERT TO CHARACTER SET that
# rewrites stored bytes, which is not something to do implicitly behind a CRUD
# call. Empty on SQLite and DuckDB, so their DDL is byte-for-byte unchanged.
sft_create_table_suffix <- function(conn) {
  if (sft_is_mariadb_connection(conn)) {
    return(" DEFAULT CHARSET=utf8mb4")
  }

  ""
}

sft_last_insert_id <- function(conn) {
  if (sft_is_mariadb_connection(conn)) {
    out <- DBI::dbGetQuery(conn, "SELECT LAST_INSERT_ID() AS sft_id")
    return(out$sft_id[1])
  }

  if (sft_is_duckdb_connection(conn)) {
    stop(
      "DuckDB uses explicit shinyformtools record ids; call sft_next_sft_id() before insert.",
      call. = FALSE
    )
  }

  DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS sft_id")$sft_id[1]
}

sft_next_integer_id <- function(conn, table_name, id_column) {
  out <- DBI::dbGetQuery(
    conn,
    paste0(
      "SELECT COALESCE(MAX(",
      sft_quote_identifier(conn, id_column),
      "), 0) + 1 AS next_id FROM ",
      sft_quote_identifier(conn, table_name)
    )
  )

  as.integer(out$next_id[1])
}

sft_next_sft_id <- function(conn, table_name) {
  sft_next_integer_id(conn, table_name, "sft_id")
}

# Prepend an explicit integer primary key to a parallel columns/values pair.
#
# Backends without an auto-increment primary key (currently DuckDB, per
# sft_requires_explicit_integer_id()) need the id supplied by hand. This
# centralises the "compute next id, prepend the id column and value" step that
# the system-table INSERT builders (audit log, schema migrations, user
# preferences) all share. On backends that auto-generate ids the inputs are
# returned unchanged.
#
# Returns the (possibly extended) `columns` and `values` as a list. Internal.
sft_prepend_explicit_id <- function(conn, table_name, id_column, columns, values) {
  if (sft_requires_explicit_integer_id(conn)) {
    columns <- c(id_column, columns)
    values <- c(
      list(sft_next_integer_id(conn, table_name, id_column)),
      values
    )
  }

  list(columns = columns, values = values)
}

# A "database is locked" that came only after SQLite's busy_timeout ran out:
# another connection held the write lock the whole time. Retrying would wait
# the full timeout again on each attempt and block the R process (and with it
# every Shiny session) for attempts x timeout. Only a lock refused at once is
# worth another attempt.
sft_sqlite_lock_waited_out <- function(e, started) {
  grepl("database is locked", conditionMessage(e), ignore.case = TRUE) &&
    as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000 >=
      0.8 * sft_sqlite_busy_timeout_ms
}

# Identify a transient conflict that re-running the transaction can resolve: a
# unique or primary-key violation produced by a racing writer, or an InnoDB
# lock conflict. The remaining MAX(id) + 1 allocations (per-record `version_no`
# on every backend; DuckDB `sft_id` / audit `log_id`) let two concurrent writers
# read the same MAX and pick the same value; the covering unique indexes turn
# that into one of the constraint errors below. Each backend phrases it
# differently:
#   SQLite : "UNIQUE constraint failed", "... must be unique"
#   DuckDB : "Duplicate key", "violates primary key/unique constraint"
#   MariaDB: "Duplicate entry '...' for key"
# MariaDB/InnoDB additionally raises two lock errors whose message literally
# says "try restarting transaction" - which is exactly what this predicate
# enables:
#   error 1213: "Deadlock found when trying to get lock; try restarting
#                transaction" (the victim is rolled back immediately)
#   error 1205: "Lock wait timeout exceeded; try restarting transaction"
# In both cases the transaction has been rolled back, so re-running it against
# the now-committed state is the documented recovery.
# Observed LIVE (MariaDB 11.8 via RMariaDB): a write that lost a row-lock race
# can also surface as error 1020, "Record has changed since last read in
# table '...'". Same treatment - the retry re-runs the whole transaction, so it
# re-reads the row and applies the update to the committed state (and when
# update_record carries an expected_record, the re-run raises the clean
# sft_edit_conflict instead of silently overwriting).
# A genuine business-unique violation that slips past validation also matches,
# but retrying stays safe: the retry re-runs validate_record, which now sees
# the committed duplicate and raises a clean (non-retryable) validation error, so
# the loop stops after one extra attempt rather than spinning.
#
# MATCHING THE PROSE ALONE IS NOT ENOUGH, and this was found the hard way: the
# server translates its error text. With lc_messages = 'de_DE' the very same
# duplicate arrives as "Doppelter Eintrag '1' fuer Schluessel 'PRIMARY' [1062]",
# none of the English phrases match, and the whole retry machinery silently
# stops working - on a server nobody would think to test, because test servers
# run in English. The numeric error code is NOT translated and RMariaDB appends
# it to every message in brackets, so the codes below are the language-proof
# half of this predicate and the prose is the fallback for drivers that omit
# them. Internal.
sft_is_retryable_conflict <- function(e) {
  # A basket lock that waited out its timeout (shinygridtools' cart_input
  # raises sft_lock_timeout).
  if (inherits(e, "sft_lock_timeout")) {
    return(TRUE)
  }

  # The package's own conditions are final. A validation message may well say
  # "must be unique" or name a "Duplicate entry"; matched as text it was
  # retried five times and then reported as a write conflict, and the real
  # message was lost.
  if (inherits(e, c("sft_validation_error", "sft_edit_conflict", "sft_write_conflict"))) {
    return(FALSE)
  }

  msg <- conditionMessage(e)

  if (!is.character(msg) || length(msg) != 1L) {
    return(FALSE)
  }

  grepl(
    paste(
      # Wording, as SQLite, DuckDB and an English-language MariaDB phrase it.
      "UNIQUE constraint failed",
      "must be unique",
      "Duplicate entry",
      "Duplicate key",
      "violates primary key",
      "violates unique",
      "Deadlock found",
      "Lock wait timeout exceeded",
      "Record has changed since last read",
      # SQLite: a writer that read first and then wants to write while
      # another holds the write lock is refused at once (busy_timeout does
      # not wait there, it would deadlock). The transaction was rolled back,
      # so running it again is the recovery.
      "database is locked",
      # MySQL-protocol error numbers, which no locale changes:
      #   1062 / 1586 duplicate entry for a unique or primary key
      #   1213      deadlock, the victim was rolled back
      #   1205      lock wait timeout
      #   1020      record has changed since last read
      "\\[1062\\]",
      "\\[1586\\]",
      "\\[1213\\]",
      "\\[1205\\]",
      "\\[1020\\]",
      sep = "|"
    ),
    msg,
    ignore.case = TRUE
  )
}

# Run `code` inside a database transaction, retrying on a racing-writer conflict
# (see sft_is_retryable_conflict) so both writers succeed instead of one erroring
# out. `code` is captured unevaluated and re-evaluated in the caller's frame on
# each attempt, so a full rollback and retry recomputes any MAX(id) + 1 values
# and re-runs validation against the now-committed state. Non-conflict errors and
# exhausted retries surface unchanged.
# Wait before retry number `attempt` (1 = first retry). Randomised and growing:
# writers that collided once would otherwise retry in lockstep and collide
# again, which is what exhausted the five attempts under contention (measured
# with tools/load-test/write_contention.R). The ceiling doubles per attempt -
# 10, 20, 40, 80 ms - so a writer that loses every round has waited 150 ms at
# most, and one that never conflicts never waits.
sft_retry_wait <- function(attempt) {
  Sys.sleep(stats::runif(1L, min = 0, max = 0.01 * 2^(attempt - 1L)))
}

# The error raised when a write kept losing its race until the attempts ran
# out. The database's own wording ("Record has changed since last read ...
# [1020]", "UNIQUE constraint failed ...") means nothing to a form user, so the
# message says what happened and what to do; the original text stays in it for
# logs and for callers that match on it. Classed, so a caller can tell "busy,
# try again" from a real failure; `parent` is the original condition.
sft_write_conflict_condition <- function(err, attempts) {
  structure(
    class = c("sft_write_conflict", "error", "condition"),
    list(
      message = paste0(
        "Someone else was saving the same data at the same moment, and the ",
        "write did not get through after ", attempts, " attempts. Nothing was ",
        "changed - please save again. (", conditionMessage(err), ")"
      ),
      call = NULL,
      parent = err
    )
  )
}

# Connections that are inside a with_transaction() block right now. A package
# write on such a connection joins that transaction instead of opening its
# own (DBI cannot nest them), and the block as a whole is retried.
.sft_open_transactions <- new.env(parent = emptyenv())
.sft_open_transactions$conns <- list()
.sft_open_transactions$after <- list()
.sft_open_transactions$failed <- list()

# Run `fun` when the transaction open on `conn` ends (commit or rollback), or
# right away when none is open. For locks a write inside with_transaction()
# takes: releasing them at the end of the write would let another writer in
# before the block has committed.
sft_after_transaction <- function(conn, fun) {
  if (sft_in_transaction(conn)) {
    .sft_open_transactions$after[[length(.sft_open_transactions$after) + 1L]] <- list(conn = conn, fun = fun)
  } else {
    fun()
  }
  invisible(NULL)
}

# The first error of a nested write inside an open transaction on `conn`.
sft_mark_inner_failure <- function(conn, e) {
  failures <- .sft_open_transactions$failed
  if (!any(vapply(failures, function(item) identical(item$conn, conn), logical(1)))) {
    .sft_open_transactions$failed <- c(failures, list(list(conn = conn, error = e)))
  }
  invisible(NULL)
}

# Returns and clears the recorded nested failure for `conn` (NULL if none).
sft_take_inner_failure <- function(conn) {
  failures <- .sft_open_transactions$failed
  mine <- vapply(failures, function(item) identical(item$conn, conn), logical(1))
  .sft_open_transactions$failed <- failures[!mine]
  if (any(mine)) failures[mine][[1L]]$error else NULL
}

# Run `fun` once the transaction open on `conn` has COMMITTED; after a
# rollback it is dropped. Right away when no transaction is open. For state
# outside the database that must only change when the write is stored (a
# grid's saved baseline).
sft_after_commit <- function(conn, fun) {
  if (sft_in_transaction(conn)) {
    .sft_open_transactions$after[[length(.sft_open_transactions$after) + 1L]] <-
      list(conn = conn, fun = fun, commit_only = TRUE)
  } else {
    fun()
  }
  invisible(NULL)
}

sft_in_transaction <- function(conn) {
  any(vapply(.sft_open_transactions$conns, identical, logical(1), conn))
}

# Signal warnings held back during a transaction, same class and message.
sft_resignal_warnings <- function(held) {
  for (w in held) {
    warning(w)
  }
  invisible(NULL)
}

sft_db_with_transaction <- function(conn, code, max_attempts = 5L) {
  sft_run_transaction(conn, substitute(code), parent.frame(), max_attempts)
}

sft_run_transaction <- function(conn, code_expr, env, max_attempts = 5L) {
  # Inside with_transaction(): run as part of the caller's transaction. A
  # conflict goes up to it, and it retries the whole block.
  if (sft_in_transaction(conn)) {
    # A failure here may be caught by the caller's code, while the writes that
    # ran before it stay in the open transaction. Remember it, so the outer
    # block rolls back instead of committing half of this part.
    return(withCallingHandlers(
      eval(code_expr, env),
      error = function(e) sft_mark_inner_failure(conn, e)
    ))
  }

  attempt <- 1L

  repeat {
    err <- NULL
    completed <- FALSE
    # Warnings are held back until the transaction has ended. A caller that
    # catches warnings with tryCatch() would otherwise leave the block while
    # the transaction is still open (DBI rolls back on errors only), and every
    # later write on the connection would fail.
    held <- list()
    started <- Sys.time()
    sft_take_inner_failure(conn)
    .sft_open_transactions$conns <- c(.sft_open_transactions$conns, list(conn))
    result <- tryCatch(
      {
        value <- withCallingHandlers(
          DBI::dbWithTransaction(conn, {
            value <- eval(code_expr, env)
            inner <- sft_take_inner_failure(conn)
            if (!is.null(inner)) {
              stop(inner)
            }
            value
          }),
          warning = function(w) {
            held[[length(held) + 1L]] <<- w
            invokeRestart("muffleWarning")
          }
        )
        completed <- TRUE
        value
      },
      error = function(e) {
        err <<- e
        NULL
      },
      finally = {
        # Left by something other than an error or a commit (a condition the
        # caller catches with tryCatch(), such as a message): roll back so the
        # connection is usable again.
        if (!completed && is.null(err)) {
          try(DBI::dbRollback(conn), silent = TRUE)
        }
        open <- .sft_open_transactions$conns
        hit <- which(vapply(open, identical, logical(1), conn))
        if (length(hit) > 0L) {
          .sft_open_transactions$conns <- open[-hit[length(hit)]]
        }

        # What waited for this transaction to end (see sft_after_transaction).
        waiting <- .sft_open_transactions$after
        mine <- vapply(waiting, function(item) identical(item$conn, conn), logical(1))
        .sft_open_transactions$after <- waiting[!mine]
        for (item in waiting[mine]) {
          if (isTRUE(item$commit_only) && !completed) {
            next
          }
          try(item$fun(), silent = TRUE)
        }
      }
    )

    if (is.null(err)) {
      sft_resignal_warnings(held)
      return(result)
    }

    retryable <- sft_is_retryable_conflict(err) &&
      !sft_sqlite_lock_waited_out(err, started)

    if (attempt >= max_attempts || !retryable) {
      sft_resignal_warnings(held)
      # Only an exhausted RETRY is a write conflict; with max_attempts = 1 the
      # caller asked for the raw error.
      if (retryable && max_attempts > 1L) {
        stop(sft_write_conflict_condition(err, max_attempts))
      }
      stop(err)
    }

    sft_retry_wait(attempt)
    attempt <- attempt + 1L
  }
}

#' Run several writes in one transaction
#'
#' Runs `code` in a transaction on `conn`. The package's write functions
#' ([insert_record()], [update_record()], [soft_delete_record()],
#' [restore_record()], [upsert_records()], [attach_shapes()]) called inside
#' with the same connection join it instead of opening their own, and so do
#' your own `DBI` statements on `conn`: either everything is written or
#' nothing. An error rolls the whole block back and is re-raised. When two
#' writers collide (a unique key taken at the same moment, a deadlock), the
#' whole block runs again, up to `max_attempts` times, and then fails with an
#' error of class `"sft_write_conflict"`.
#'
#' Because the block may run more than once, keep side effects other than
#' database writes (sending mail, writing files) outside it. The forms'
#' schema has to be current before the block starts: a migration cannot run
#' inside another transaction, so a form whose table needs one raises an
#' error there; call [init_db()] beforehand. On MariaDB, [upsert_records()]
#' guards against two sessions inserting the same new rows only if the block
#' has not read from the table before it.
#'
#' @param conn A DBI connection, for example from [db_connect()].
#' @param code The writes, as a block `{ ... }`.
#' @param max_attempts How often the block runs when writers collide.
#'
#' @return The value of `code`.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name"))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn)
#'
#' with_transaction(conn, {
#'   insert_record(contacts, list(name = "Ada"), conn = conn)
#'   insert_record(contacts, list(name = "Grace"), conn = conn)
#' })
#' nrow(fetch_records(contacts, conn = conn))
#'
#' db_disconnect(conn)
#' @export
with_transaction <- function(conn, code, max_attempts = 5L) {
  sft_run_transaction(conn, substitute(code), parent.frame(), max_attempts)
}
