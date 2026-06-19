sft_system_columns <- function(conn = NULL) {
  short_text <- if (is.null(conn)) "TEXT" else sft_short_text_definition(conn)
  # The schema signature is the full expected-schema JSON, which can exceed the
  # short-text limit (VARCHAR(255) on MariaDB); store it as long text like
  # config_json. TEXT on every backend, so the signature (computed with
  # conn = NULL) is unchanged and existing databases see no drift.
  long_text <- "TEXT"
  id_def <- if (is.null(conn)) {
    "INTEGER PRIMARY KEY AUTOINCREMENT"
  } else {
    sft_auto_id_definition(conn)
  }

  c(
    sft_id = id_def,
    sft_uuid = paste(short_text, "UNIQUE"),
    sft_easy_id = short_text,
    sft_form_id = short_text,
    sft_form_version = "INTEGER",
    sft_schema_hash = long_text,
    sft_created_at = short_text,
    sft_created_by = short_text,
    sft_updated_at = short_text,
    sft_updated_by = short_text,
    sft_deleted_at = short_text,
    sft_deleted_by = short_text,
    sft_is_deleted = "INTEGER NOT NULL DEFAULT 0",
    sft_unique_slot = "INTEGER NOT NULL DEFAULT 0"
  )
}

sft_field_db_definition <- function(field, conn = NULL) {
  definition <- field$db_type

  if (!is.null(field$db_default)) {
    if (is.null(conn)) {
      definition <- paste(definition, "DEFAULT", field$db_default)
    } else {
      definition <- paste(definition, "DEFAULT", sft_sql_literal(conn, field$db_default))
    }
  }

  definition
}

sft_expected_columns <- function(form, conn = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  stored_fields <- Filter(sft_is_stored_field, form$fields)

  field_definitions <- vapply(
    stored_fields,
    sft_field_db_definition,
    character(1),
    conn = conn
  )

  names(field_definitions) <- vapply(
    stored_fields,
    function(x) x$db_column,
    character(1)
  )

  c(sft_system_columns(conn), field_definitions)
}

sft_column_definition <- function(conn, column_name, definition) {
  paste(sft_quote_identifier(conn, column_name), definition)
}

sft_create_table_sql <- function(conn, table_name, columns) {
  column_sql <- vapply(
    names(columns),
    function(column_name) {
      sft_column_definition(
        conn = conn,
        column_name = column_name,
        definition = columns[[column_name]]
      )
    },
    character(1)
  )

  paste0(
    "CREATE TABLE IF NOT EXISTS ",
    sft_quote_identifier(conn, table_name),
    " (",
    paste(column_sql, collapse = ", "),
    ")"
  )
}

sft_system_table_types <- function(conn) {
  list(
    id = sft_auto_id_definition(conn),
    short_text = sft_short_text_definition(conn),
    long_text = "TEXT"
  )
}

#' Initialize shinyformtools system tables
#'
#' @param conn A DBI connection.
#'
#' @return Invisibly returns the connection.
#' @examples
#' conn <- db_connect(db_sqlite(tempfile(fileext = ".sqlite")))
#' init_system_tables(conn)
#' DBI::dbExistsTable(conn, "sft_forms")
#' db_disconnect(conn)
#' @keywords internal
init_system_tables <- function(conn) {
  types <- sft_system_table_types(conn)

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE IF NOT EXISTS sft_forms (",
      "form_id ", types$short_text, " PRIMARY KEY, ",
      "form_name ", types$short_text, " NOT NULL, ",
      "table_name ", types$short_text, " NOT NULL, ",
      "active_version INTEGER NOT NULL, ",
      "status ", types$short_text, " NOT NULL DEFAULT 'active', ",
      "config_json ", types$long_text, ", ",
      "schema_hash ", types$long_text, ", ",
      "created_at ", types$short_text, ", ",
      "updated_at ", types$short_text,
      ")"
    )
  )

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE IF NOT EXISTS sft_fields (",
      "form_id ", types$short_text, " NOT NULL, ",
      "field_id ", types$short_text, " NOT NULL, ",
      "db_column ", types$short_text, " NOT NULL, ",
      "label ", types$short_text, ", ",
      "input_type ", types$short_text, ", ",
      "db_type ", types$short_text, ", ",
      "status ", types$short_text, " NOT NULL, ",
      "first_version INTEGER, ",
      "last_version INTEGER, ",
      "mandatory INTEGER NOT NULL DEFAULT 0, ",
      "unique_field INTEGER NOT NULL DEFAULT 0, ",
      "editable INTEGER NOT NULL DEFAULT 1, ",
      "show_field INTEGER NOT NULL DEFAULT 1, ",
      "tab INTEGER, ",
      "slide INTEGER, ",
      "col INTEGER, ",
      "pos INTEGER, ",
      "args_json ", types$long_text, ", ",
      "renamed_from ", types$short_text, ", ",
      "created_at ", types$short_text, ", ",
      "retired_at ", types$short_text, ", ",
      "PRIMARY KEY (form_id, field_id)",
      ")"
    )
  )

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE IF NOT EXISTS sft_schema_migrations (",
      "migration_id ", types$id, ", ",
      "form_id ", types$short_text, " NOT NULL, ",
      "table_name ", types$short_text, " NOT NULL, ",
      "from_version INTEGER, ",
      "to_version INTEGER, ",
      "action ", types$short_text, " NOT NULL, ",
      "field_id ", types$short_text, ", ",
      "db_column ", types$short_text, ", ",
      "details_json ", types$long_text, ", ",
      "applied_at ", types$short_text, ", ",
      "applied_by ", types$short_text,
      ")"
    )
  )

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE IF NOT EXISTS sft_audit_log (",
      "log_id ", types$id, ", ",
      "form_id ", types$short_text, " NOT NULL, ",
      "table_name ", types$short_text, " NOT NULL, ",
      "record_id INTEGER, ",
      "record_uuid ", types$short_text, ", ",
      "action ", types$short_text, " NOT NULL, ",
      "version_no INTEGER, ",
      "changed_at ", types$short_text, ", ",
      "changed_by ", types$short_text, ", ",
      "old_data_json ", types$long_text, ", ",
      "new_data_json ", types$long_text, ", ",
      "changed_fields_json ", types$long_text, ", ",
      "reason ", types$long_text,
      ")"
    )
  )

  sft_ensure_column(conn, "sft_forms", "schema_hash", types$long_text)

  sft_init_preferences_table(conn)

  sft_ensure_audit_version_index(conn)

  invisible(conn)
}

# The audit log holds one row per record version. `version_no` is computed as
# MAX(version_no) + 1 per record, so two concurrent writers can read the same
# MAX and produce a duplicate version. A unique index on
# (form_id, table_name, record_id, version_no) turns that collision into a
# constraint error and transaction rollback instead of a silently duplicated
# audit entry; sft_db_with_transaction then retries the rolled-back writer, which
# recomputes MAX + 1 and succeeds, so both concurrent writers commit. Created
# idempotently. Rows with a NULL record_id (uuid-only writes) are not covered,
# matching standard SQL NULL-distinctness; the common insert/update/delete/
# restore paths all carry a record_id.
sft_ensure_audit_version_index <- function(conn) {
  index_name <- "sft_audit_log_version_idx"

  if (index_name %in% sft_list_index_names(conn, "sft_audit_log")) {
    return(invisible(FALSE))
  }

  columns_sql <- sft_sql_quoted_columns(
    conn,
    c("form_id", "table_name", "record_id", "version_no")
  )

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE UNIQUE INDEX ",
      sft_quote_identifier(conn, index_name),
      " ON ", sft_quote_identifier(conn, "sft_audit_log"),
      " (", columns_sql, ")"
    )
  )

  invisible(TRUE)
}

sft_register_form_schema <- function(conn,
                                     form,
                                     orphaned_columns = character(),
                                     user = NULL) {
  sft_validate_form(form)

  now <- sft_now()
  form_config <- form
  form_config$server <- NULL
  form_config$header <- NULL
  form_config$footer <- NULL
  form_config$db <- sft_redact_db_config(form_config$db)
  config_json <- as.character(sft_as_json(form_config))
  schema_hash <- sft_schema_signature(form)

  existing_form <- DBI::dbGetQuery(
    conn,
    "SELECT form_id FROM sft_forms WHERE form_id = ?",
    params = list(form$form_id)
  )

  if (nrow(existing_form) == 0L) {
    DBI::dbExecute(
      conn,
      "
      INSERT INTO sft_forms (
        form_id, form_name, table_name, active_version, status,
        config_json, schema_hash, created_at, updated_at
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ",
      params = list(
        form$form_id,
        form$form_name,
        form$table_name,
        form$version,
        "active",
        config_json,
        schema_hash,
        now,
        now
      )
    )
  } else {
    DBI::dbExecute(
      conn,
      "
      UPDATE sft_forms
      SET form_name = ?,
          table_name = ?,
          active_version = ?,
          status = ?,
          config_json = ?,
          schema_hash = ?,
          updated_at = ?
      WHERE form_id = ?
      ",
      params = list(
        form$form_name,
        form$table_name,
        form$version,
        "active",
        config_json,
        schema_hash,
        now,
        form$form_id
      )
    )
  }

  field_ids <- vapply(form$fields, function(x) x$id, character(1))

  for (field in form$fields) {
    existing_field <- DBI::dbGetQuery(
      conn,
      "
      SELECT form_id, field_id
      FROM sft_fields
      WHERE form_id = ? AND field_id = ?
      ",
      params = list(form$form_id, field$id)
    )

    args_json <- as.character(sft_as_json(field$args))

    if (nrow(existing_field) == 0L) {
      DBI::dbExecute(
        conn,
        "
        INSERT INTO sft_fields (
          form_id, field_id, db_column, label, input_type, db_type,
          status, first_version, last_version, mandatory, unique_field,
          editable, show_field, tab, slide, col, pos, args_json,
          renamed_from, created_at, retired_at
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ",
        params = list(
          form$form_id,
          field$id,
          field$db_column,
          field$label,
          field$input_type,
          field$db_type,
          field$status,
          form$version,
          form$version,
          as.integer(field$mandatory),
          as.integer(field$unique),
          sft_field_editable_storage(field),
          as.integer(field$show),
          field$tab,
          field$slide,
          field$col,
          field$pos,
          args_json,
          sft_db_param(field$renamed_from),
          now,
          if (identical(field$status, "active")) NA_character_ else now
        )
      )
    } else {
      DBI::dbExecute(
        conn,
        "
        UPDATE sft_fields
        SET db_column = ?,
            label = ?,
            input_type = ?,
            db_type = ?,
            status = ?,
            last_version = ?,
            mandatory = ?,
            unique_field = ?,
            editable = ?,
            show_field = ?,
            tab = ?,
            slide = ?,
            col = ?,
            pos = ?,
            args_json = ?,
            renamed_from = ?,
            retired_at = ?
        WHERE form_id = ? AND field_id = ?
        ",
        params = list(
          field$db_column,
          field$label,
          field$input_type,
          field$db_type,
          field$status,
          form$version,
          as.integer(field$mandatory),
          as.integer(field$unique),
          sft_field_editable_storage(field),
          as.integer(field$show),
          field$tab,
          field$slide,
          field$col,
          field$pos,
          args_json,
          sft_db_param(field$renamed_from),
          if (identical(field$status, "active")) NA_character_ else now,
          form$form_id,
          field$id
        )
      )
    }
  }

  active_registered_fields <- DBI::dbGetQuery(
    conn,
    "
    SELECT field_id
    FROM sft_fields
    WHERE form_id = ? AND status = 'active'
    ",
    params = list(form$form_id)
  )

  retired_field_ids <- setdiff(active_registered_fields$field_id, field_ids)

  for (field_id in retired_field_ids) {
    DBI::dbExecute(
      conn,
      "
      UPDATE sft_fields
      SET status = 'retired',
          last_version = ?,
          retired_at = ?
      WHERE form_id = ? AND field_id = ?
      ",
      params = list(
        form$version,
        now,
        form$form_id,
        field_id
      )
    )
  }

  for (column_name in orphaned_columns) {
    existing_orphan <- DBI::dbGetQuery(
      conn,
      "
      SELECT form_id, field_id
      FROM sft_fields
      WHERE form_id = ? AND db_column = ?
      ",
      params = list(form$form_id, column_name)
    )

    if (nrow(existing_orphan) == 0L) {
      DBI::dbExecute(
        conn,
        "
        INSERT INTO sft_fields (
          form_id, field_id, db_column, label, input_type, db_type,
          status, first_version, last_version, mandatory, unique_field,
          editable, show_field, tab, slide, col, pos, args_json,
          renamed_from, created_at, retired_at
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ",
        params = list(
          form$form_id,
          column_name,
          column_name,
          column_name,
          NA_character_,
          NA_character_,
          "orphaned",
          form$version,
          form$version,
          0L,
          0L,
          0L,
          0L,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          "{}",
          NA_character_,
          now,
          now
        )
      )
    }
  }

  invisible(TRUE)
}

#' Initialize a database for a form
#'
#' Initializes system tables and optionally applies a safe migration for the
#' form's main data table.
#'
#' @param form Object created with [form()].
#' @param conn Optional existing DBI connection.
#' @param apply Logical. Whether to apply the migration plan.
#' @param user Optional user name for schema migration logs.
#'
#' @return Invisibly returns the migration plan.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(
#'     form_field(id = "name", label = "Name", mandatory = TRUE),
#'     form_field(id = "email", label = "Email", unique = TRUE)
#'   )
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' DBI::dbExistsTable(conn, "contacts")
#' db_disconnect(conn)
#' @export
init_db <- function(form,
                        conn = NULL,
                        apply = TRUE,
                        user = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  init_system_tables(conn)

  plan <- plan_migration(conn, form)

  if (isTRUE(apply)) {
    apply_migration(
      conn = conn,
      form = form,
      plan = plan,
      user = user
    )
  }

  invisible(plan)
}

# Backend-neutral signature of the schema a form expects. Deterministic for a
# given form and identical across backends, because sft_expected_columns() with
# conn = NULL uses the generic type defaults. Lets us detect schema drift even
# when the form version was not bumped.
sft_schema_signature <- function(form) {
  payload <- list(
    columns = sft_expected_columns(form, conn = NULL),
    indexes = lapply(
      sft_expected_indexes(form, conn = NULL),
      function(index) index$columns
    )
  )
  as.character(sft_as_json(payload))
}

# Cheap check whether the database already reflects the form's current schema.
# Performs only table/column probes plus a single primary-key lookup; performs no
# writes. Older databases without sft_forms$schema_hash deliberately return
# FALSE so sft_ensure_schema() can add the missing system column via
# init_system_tables().
sft_schema_is_current <- function(conn, form) {
  tables <- DBI::dbListTables(conn)
  required_tables <- c(
    "sft_forms",
    "sft_fields",
    "sft_schema_migrations",
    "sft_audit_log",
    "sft_user_preferences",
    form$table_name
  )

  if (!all(required_tables %in% tables)) {
    return(FALSE)
  }

  forms_info <- sft_table_info(conn, "sft_forms")
  if (!all(c("active_version", "schema_hash") %in% forms_info$name)) {
    return(FALSE)
  }

  row <- DBI::dbGetQuery(
    conn,
    "SELECT active_version, schema_hash FROM sft_forms WHERE form_id = ?",
    params = list(form$form_id)
  )
  if (nrow(row) == 0L) {
    return(FALSE)
  }

  signature_ok <- isTRUE(row$active_version[1] == form$version) &&
    identical(as.character(row$schema_hash[1]), sft_schema_signature(form))
  if (!signature_ok) {
    return(FALSE)
  }

  # The stored schema hash can match while index reconciliation is incomplete,
  # for example after a failed DDL statement on a backend where DDL is not fully
  # transactional. Verify that expected unique indexes exist and obsolete
  # sft-managed unique indexes are absent.
  expected_indexes <- sft_expected_indexes(form, conn)
  existing_indexes <- sft_list_index_names(conn, form$table_name)
  expected_index_names <- vapply(
    expected_indexes,
    function(index) index$name,
    character(1)
  )

  index_prefix <- paste0("uq_", form$table_name, "__")
  obsolete_indexes <- existing_indexes[
    startsWith(existing_indexes, index_prefix) &
      !(existing_indexes %in% expected_index_names)
  ]
  if (length(obsolete_indexes) > 0L) {
    return(FALSE)
  }

  if (!all(expected_index_names %in% existing_indexes)) {
    return(FALSE)
  }

  TRUE
}

# Reconcile the schema only when it is not already current. Replaces the
# unconditional init_db() calls in the CRUD hot path: a cheap probe on every
# call, a full migration only on first contact or genuine drift. Still self-heals
# a cold database when a CRUD function is used standalone (probe fails -> init).
sft_ensure_schema <- function(conn, form, user = NULL) {
  if (sft_schema_is_current(conn, form)) {
    return(invisible(FALSE))
  }
  init_db(form, conn = conn, apply = TRUE, user = user)
  invisible(TRUE)
}

# Deterministic name for a unique index, prefixed with the table name so it is
# unique across the database (SQLite index names are database-global).
sft_unique_index_name <- function(table_name, column) {
  paste0("uq_", table_name, "__", column)
}

# Unique indexes a form expects: one composite UNIQUE index per active unique
# field over (db_column, sft_unique_slot). Live rows share slot 0, so uniqueness
# is enforced among them; soft-deleted rows carry their sft_id as the slot and
# therefore never collide, which frees the value for reuse by a new record.
sft_expected_indexes <- function(form, conn = NULL) {
  unique_fields <- Filter(
    function(field) isTRUE(field$unique),
    sft_active_input_fields(form)
  )

  lapply(unique_fields, function(field) {
    list(
      name = sft_unique_index_name(form$table_name, field$db_column),
      columns = c(field$db_column, "sft_unique_slot")
    )
  })
}

# Idempotently create the unique indexes a form expects. Backfills sft_unique_slot
# for already soft-deleted rows first, so index creation does not fail on a
# database that already contains deleted duplicates. Creation still fails by
# design when *active* rows already hold duplicate values for a unique field.
# Create one unique index, backfilling sft_unique_slot for soft-deleted rows
# first so creation does not fail on a database that already contains deleted
# duplicates. Creation still fails by design when *active* rows already hold
# duplicate values for a unique field.
sft_create_unique_index <- function(conn, form, index) {
  DBI::dbExecute(
    conn,
    paste0(
      "UPDATE ", sft_quote_identifier(conn, form$table_name),
      " SET sft_unique_slot = sft_id WHERE sft_is_deleted = 1"
    )
  )

  columns_sql <- sft_sql_quoted_columns(conn, index$columns)

  sql <- paste0(
    "CREATE UNIQUE INDEX ",
    sft_quote_identifier(conn, index$name),
    " ON ", sft_quote_identifier(conn, form$table_name),
    " (", columns_sql, ")"
  )

  tryCatch(
    DBI::dbExecute(conn, sql),
    error = function(e) {
      stop(
        paste0(
          "Could not create unique index '", index$name, "' on ",
          form$table_name, ". Active records probably already contain ",
          "duplicate values for a unique field. Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  invisible(TRUE)
}

# List index names defined on a table, across backends. Returns character(0)
# when the table has no indexes or the backend is unknown.
sft_list_index_names <- function(conn, table_name) {
  backend <- sft_db_backend(conn)

  if (backend == "sqlite") {
    rows <- DBI::dbGetQuery(
      conn,
      paste0("PRAGMA index_list(", sft_quote_identifier(conn, table_name), ")")
    )
    if (nrow(rows) == 0L) {
      return(character())
    }
    return(as.character(rows$name))
  }

  if (backend == "duckdb") {
    rows <- DBI::dbGetQuery(
      conn,
      "SELECT index_name FROM duckdb_indexes() WHERE table_name = ?",
      params = list(table_name)
    )
    if (nrow(rows) == 0L) {
      return(character())
    }
    return(as.character(rows$index_name))
  }

  if (backend == "mariadb") {
    rows <- DBI::dbGetQuery(
      conn,
      paste0("SHOW INDEX FROM ", sft_quote_identifier(conn, table_name))
    )
    if (nrow(rows) == 0L) {
      return(character())
    }
    return(unique(as.character(rows$Key_name)))
  }

  character()
}

# Drop an index, across backends. MariaDB requires the ON <table> clause.
sft_drop_index <- function(conn, table_name, index_name) {
  if (sft_db_backend(conn) == "mariadb") {
    DBI::dbExecute(
      conn,
      paste0(
        "DROP INDEX IF EXISTS ", sft_quote_identifier(conn, index_name),
        " ON ", sft_quote_identifier(conn, table_name)
      )
    )
  } else {
    DBI::dbExecute(
      conn,
      paste0("DROP INDEX IF EXISTS ", sft_quote_identifier(conn, index_name))
    )
  }
  invisible(TRUE)
}
