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

    return(DBI::dbConnect(RSQLite::SQLite(), dbname = db$path))
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
# unique or primary-key violation produced by a racing writer. The remaining
# MAX(id) + 1 allocations (per-record `version_no` on every backend; DuckDB
# `sft_id` / audit `log_id`) let two concurrent writers read the same MAX and
# pick the same value; the covering unique indexes turn that into one of the
# constraint errors below. Each backend phrases it differently:
#   SQLite : "UNIQUE constraint failed", "... must be unique"
#   DuckDB : "Duplicate key", "violates primary key/unique constraint"
#   MariaDB: "Duplicate entry '...' for key"
# A genuine business-unique violation that slips past validation also matches,
# but retrying stays safe: the retry re-runs validate_record, which now sees
# the committed duplicate and raises a clean (non-retryable) validation error, so
# the loop stops after one extra attempt rather than spinning. Internal.
sft_is_retryable_conflict <- function(e) {
  msg <- conditionMessage(e)

  if (!is.character(msg) || length(msg) != 1L) {
    return(FALSE)
  }

  grepl(
    paste(
      "UNIQUE constraint failed",
      "must be unique",
      "Duplicate entry",
      "Duplicate key",
      "violates primary key",
      "violates unique",
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
