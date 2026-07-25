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
#' The same form behaves slightly differently on a MariaDB or MySQL server than
#' it does on SQLite. None of this needs configuring, but it is worth knowing:
#'
#' \itemize{
#'   \item **Uniqueness ignores case and accents.** MariaDB's default collations
#'     are accent- and case-insensitive, so a field declared `unique = TRUE`
#'     treats `"MULLER@example.com"` and `"muller@example.com"` as the same
#'     value -- and likewise two spellings that differ only by an umlaut. SQLite
#'     compares exactly, so the identical form accepts both there. Uniqueness is
#'     therefore stricter on MariaDB, never looser.
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
#' }
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
sft_db_with_transaction <- function(conn, code, max_attempts = 5L) {
  code_expr <- substitute(code)
  env <- parent.frame()

  attempt <- 1L

  repeat {
    err <- NULL
    result <- tryCatch(
      DBI::dbWithTransaction(conn, eval(code_expr, env)),
      error = function(e) {
        err <<- e
        NULL
      }
    )

    if (is.null(err)) {
      return(result)
    }

    if (attempt >= max_attempts || !sft_is_retryable_conflict(err)) {
      stop(err)
    }

    attempt <- attempt + 1L
  }
}
