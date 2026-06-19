sft_empty_migration_actions <- function() {
  data.frame(
    action = character(),
    field_id = character(),
    db_column = character(),
    db_type = character(),
    safe = logical(),
    details_json = character()
  )
}

sft_table_exists <- function(conn, table_name) {
  table_name %in% DBI::dbListTables(conn)
}

# Add a column to an existing table only if it is missing. Used to evolve the
# shinyformtools system tables themselves, since CREATE TABLE IF NOT EXISTS
# never adds columns to a table that already exists.
sft_ensure_column <- function(conn, table_name, column, definition) {
  info <- sft_table_info(conn, table_name)
  if (!column %in% info$name) {
    DBI::dbExecute(
      conn,
      paste0(
        "ALTER TABLE ", sft_quote_identifier(conn, table_name),
        " ADD COLUMN ", sft_quote_identifier(conn, column), " ", definition
      )
    )
  }
  invisible(TRUE)
}

sft_table_info <- function(conn, table_name) {
  if (!sft_table_exists(conn, table_name)) {
    return(
      data.frame(
        cid = integer(),
        name = character(),
        type = character(),
        notnull = integer(),
        dflt_value = character(),
        pk = integer()
      )
    )
  }

  if (sft_is_mariadb_connection(conn)) {
    raw <- DBI::dbGetQuery(
      conn,
      paste0("SHOW COLUMNS FROM ", sft_quote_identifier(conn, table_name))
    )

    return(
      data.frame(
        cid = seq_len(nrow(raw)) - 1L,
        name = raw$Field,
        type = toupper(as.character(raw$Type)),
        notnull = as.integer(toupper(as.character(raw$Null)) == "NO"),
        dflt_value = as.character(raw$Default),
        pk = as.integer(toupper(as.character(raw$Key)) == "PRI"),
        stringsAsFactors = FALSE
      )
    )
  }

  if (sft_is_duckdb_connection(conn)) {
    return(
      DBI::dbGetQuery(
        conn,
        paste0("PRAGMA table_info(", sft_sql_literal(conn, table_name), ")")
      )
    )
  }

  DBI::dbGetQuery(
    conn,
    paste0("PRAGMA table_info(", sft_quote_identifier(conn, table_name), ")")
  )
}

sft_normalize_db_type_for_compare <- function(type) {
  type <- toupper(trimws(as.character(type %||% "")))
  type <- sub("\\(.*$", "", type)
  type <- trimws(type)

  if (type %in% c("CHAR", "CHARACTER", "VARCHAR", "STRING", "TEXT")) {
    return("TEXT")
  }

  if (type %in% c("INT", "INTEGER", "BIGINT", "SMALLINT", "TINYINT")) {
    return("INTEGER")
  }

  if (type %in% c("DOUBLE", "DOUBLE PRECISION", "FLOAT", "REAL", "DECIMAL", "NUMERIC")) {
    return("REAL")
  }

  if (type %in% c("BOOL", "BOOLEAN")) {
    return("INTEGER")
  }

  type
}

sft_inactive_columns <- function(conn, form_id) {
  if (!sft_table_exists(conn, "sft_fields")) {
    return(character())
  }

  inactive_fields <- DBI::dbGetQuery(
    conn,
    "
    SELECT db_column
    FROM sft_fields
    WHERE form_id = ?
      AND status IN ('retired', 'orphaned')
    ",
    params = list(form_id)
  )

  inactive_fields$db_column
}

sft_field_id_for_column <- function(form, column_name) {
  input_fields <- Filter(sft_is_input_field, form$fields)

  matches <- vapply(
    input_fields,
    function(field) identical(field$db_column, column_name),
    logical(1)
  )

  if (!any(matches)) {
    return(NA_character_)
  }

  input_fields[[which(matches)[1L]]]$id
}

sft_add_migration_action <- function(actions,
                                     action,
                                     field_id = NA_character_,
                                     db_column = NA_character_,
                                     db_type = NA_character_,
                                     safe = TRUE,
                                     details = list()) {
  row <- data.frame(
    action = action,
    field_id = field_id,
    db_column = db_column,
    db_type = db_type,
    safe = safe,
    details_json = as.character(sft_as_json(details))
  )

  rbind(actions, row)
}

#' Inspect the database schema for a form
#'
#' @param conn A DBI connection.
#' @param form Object created with [form()].
#'
#' @return A list describing the current and expected schema.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#'
#' # Before the table is created, it reports the expected columns as missing.
#' inspection <- inspect_schema(conn, contacts)
#' inspection$table_exists
#' inspection$missing_columns
#'
#' db_disconnect(conn)
#' @export
inspect_schema <- function(conn, form) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  expected_columns <- sft_expected_columns(form, conn = conn)
  table_exists <- sft_table_exists(conn, form$table_name)
  table_info <- sft_table_info(conn, form$table_name)

  current_columns <- table_info$name
  current_types <- table_info$type
  names(current_types) <- table_info$name

  inactive_columns <- sft_inactive_columns(conn, form$form_id)

  missing_columns <- setdiff(names(expected_columns), current_columns)
  extra_columns <- setdiff(current_columns, names(expected_columns))
  unregistered_extra_columns <- setdiff(extra_columns, inactive_columns)

 input_fields <- Filter(sft_is_input_field, form$fields)

comparable_columns <- intersect(
  vapply(input_fields, function(x) x$db_column, character(1)),
  current_columns
)

  type_warnings <- data.frame(
    db_column = character(),
    expected_type = character(),
    current_type = character()
  )

  for (column_name in comparable_columns) {
    expected_type <- toupper(expected_columns[[column_name]])
    current_type <- toupper(current_types[[column_name]])

    expected_type_base <- sft_normalize_db_type_for_compare(
      strsplit(expected_type, "\\s+")[[1]][1]
    )
    current_type_base <- sft_normalize_db_type_for_compare(
      strsplit(current_type, "\\s+")[[1]][1]
    )

    if (
      nzchar(current_type_base) &&
        !identical(expected_type_base, current_type_base)
    ) {
      type_warnings <- rbind(
        type_warnings,
        data.frame(
          db_column = column_name,
          expected_type = expected_type,
          current_type = current_type
        )
      )
    }
  }

  list(
    table_exists = table_exists,
    table_info = table_info,
    expected_columns = expected_columns,
    current_columns = current_columns,
    missing_columns = missing_columns,
    extra_columns = extra_columns,
    inactive_columns = inactive_columns,
    unregistered_extra_columns = unregistered_extra_columns,
    type_warnings = type_warnings
  )
}

#' Plan a database schema migration
#'
#' @param conn A DBI connection.
#' @param form Object created with [form()].
#'
#' @return A migration plan object.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#'
#' # On a fresh database the plan contains a single create_table action.
#' plan <- plan_migration(conn, contacts)
#' plan$actions$action
#'
#' db_disconnect(conn)
#' @export
plan_migration <- function(conn, form) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  inspection <- inspect_schema(conn, form)
  actions <- sft_empty_migration_actions()

  if (!isTRUE(inspection$table_exists)) {
    actions <- sft_add_migration_action(
      actions = actions,
      action = "create_table",
      field_id = NA_character_,
      db_column = form$table_name,
      db_type = NA_character_,
      safe = TRUE,
      details = list(
        table_name = form$table_name,
        columns = inspection$expected_columns
      )
    )

    return(
      structure(
        list(
          form = form,
          inspection = inspection,
          actions = actions
        ),
        class = c("sft_migration_plan", "list")
      )
    )
  }

  for (column_name in inspection$missing_columns) {
    column_definition <- inspection$expected_columns[[column_name]]

    if (identical(column_name, "sft_id")) {
      actions <- sft_add_migration_action(
        actions = actions,
        action = "manual_add_system_id",
        field_id = NA_character_,
        db_column = column_name,
        db_type = column_definition,
        safe = FALSE,
        details = list(
          reason = "Adding an autoincrement primary key to an existing SQLite table requires a manual table rebuild."
        )
      )

      next
    }

    actions <- sft_add_migration_action(
      actions = actions,
      action = "add_column",
      field_id = sft_field_id_for_column(form, column_name),
      db_column = column_name,
      db_type = column_definition,
      safe = TRUE,
      details = list(
        column = column_name,
        definition = column_definition
      )
    )
  }

  for (column_name in inspection$unregistered_extra_columns) {
    actions <- sft_add_migration_action(
      actions = actions,
      action = "retire_column",
      field_id = NA_character_,
      db_column = column_name,
      db_type = NA_character_,
      safe = TRUE,
      details = list(
        column = column_name,
        behavior = "Column remains in the database and is registered as retired/orphaned metadata."
      )
    )
  }

  expected_indexes <- sft_expected_indexes(form, conn)
  expected_index_names <- vapply(
    expected_indexes,
    function(index) index$name,
    character(1)
  )
  existing_indexes <- sft_list_index_names(conn, form$table_name)
  index_prefix <- paste0("uq_", form$table_name, "__")

  for (index in expected_indexes) {
    if (!(index$name %in% existing_indexes)) {
      actions <- sft_add_migration_action(
        actions = actions,
        action = "create_index",
        field_id = NA_character_,
        db_column = index$name,
        db_type = NA_character_,
        safe = TRUE,
        details = list(index_name = index$name, columns = index$columns)
      )
    }
  }

  obsolete_indexes <- existing_indexes[
    startsWith(existing_indexes, index_prefix) &
      !(existing_indexes %in% expected_index_names)
  ]
  for (name in obsolete_indexes) {
    actions <- sft_add_migration_action(
      actions = actions,
      action = "drop_index",
      field_id = NA_character_,
      db_column = name,
      db_type = NA_character_,
      safe = TRUE,
      details = list(index_name = name)
    )
  }

  if (nrow(inspection$type_warnings) > 0L) {
    for (i in seq_len(nrow(inspection$type_warnings))) {
      actions <- sft_add_migration_action(
        actions = actions,
        action = "type_warning",
        field_id = sft_field_id_for_column(
          form,
          inspection$type_warnings$db_column[i]
        ),
        db_column = inspection$type_warnings$db_column[i],
        db_type = inspection$type_warnings$expected_type[i],
        safe = FALSE,
        details = list(
          expected_type = inspection$type_warnings$expected_type[i],
          current_type = inspection$type_warnings$current_type[i],
          behavior = "Type changes require manual migration."
        )
      )
    }
  }

  structure(
    list(
      form = form,
      inspection = inspection,
      actions = actions
    ),
    class = c("sft_migration_plan", "list")
  )
}

sft_log_schema_migration <- function(conn, form, action_row, user = NULL) {
  columns <- c(
    "form_id",
    "table_name",
    "from_version",
    "to_version",
    "action",
    "field_id",
    "db_column",
    "details_json",
    "applied_at",
    "applied_by"
  )

  values <- list(
    form$form_id,
    form$table_name,
    NA_integer_,
    form$version,
    action_row$action,
    sft_db_param(action_row$field_id),
    sft_db_param(action_row$db_column),
    action_row$details_json,
    sft_now(),
    sft_db_param(user)
  )

  prepared <- sft_prepend_explicit_id(
    conn, "sft_schema_migrations", "migration_id", columns, values
  )
  columns <- prepared$columns
  values <- prepared$values

  DBI::dbExecute(
    conn,
    paste0(
      "INSERT INTO sft_schema_migrations (",
      sft_sql_quoted_columns(conn, columns),
      ") VALUES (",
      paste(rep("?", length(values)), collapse = ", "),
      ")"
    ),
    params = values
  )

  invisible(TRUE)
}

# Apply the safe actions of a migration plan, then register the resulting
# schema. Factored out of apply_migration so it can run either directly or
# inside a transaction depending on whether the backend has transactional DDL.
sft_run_migration_actions <- function(conn, form, actions, user = NULL) {
  for (i in seq_len(nrow(actions))) {
    action_row <- actions[i, , drop = FALSE]

    if (identical(action_row$action, "create_table")) {
      expected_columns <- sft_expected_columns(form, conn = conn)

      DBI::dbExecute(
        conn,
        sft_create_table_sql(
          conn = conn,
          table_name = form$table_name,
          columns = expected_columns
        )
      )

      for (index in sft_expected_indexes(form, conn)) {
        sft_create_unique_index(conn, form, index)
      }
    }

    if (identical(action_row$action, "add_column")) {
      DBI::dbExecute(
        conn,
        paste0(
          "ALTER TABLE ",
          sft_quote_identifier(conn, form$table_name),
          " ADD COLUMN ",
          sft_column_definition(
            conn = conn,
            column_name = action_row$db_column,
            definition = action_row$db_type
          )
        )
      )
    }

    if (identical(action_row$action, "retire_column")) {
      # No SQL is executed here. The column remains in the database.
      # The metadata registration below records it as inactive/orphaned.
      invisible(TRUE)
    }

    if (identical(action_row$action, "create_index")) {
      index <- Find(
        function(candidate) identical(candidate$name, action_row$db_column),
        sft_expected_indexes(form, conn)
      )
      if (!is.null(index)) {
        sft_create_unique_index(conn, form, index)
      }
    }

    if (identical(action_row$action, "drop_index")) {
      sft_drop_index(conn, form$table_name, action_row$db_column)
    }

    sft_log_schema_migration(
      conn = conn,
      form = form,
      action_row = action_row,
      user = user
    )
  }

  orphaned_columns <- actions$db_column[actions$action == "retire_column"]

  sft_register_form_schema(
    conn = conn,
    form = form,
    orphaned_columns = orphaned_columns,
    user = user
  )

  invisible(TRUE)
}

#' Apply a safe database schema migration
#'
#' @param conn A DBI connection.
#' @param form Object created with [form()].
#' @param plan Optional migration plan from [plan_migration()].
#' @param user Optional user name for schema migration logs.
#'
#' @return Invisibly returns the migration plan.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#'
#' # Apply the safe actions (here: create the table).
#' apply_migration(conn, contacts)
#'
#' # A second plan now has nothing left to do.
#' nrow(plan_migration(conn, contacts)$actions)
#'
#' db_disconnect(conn)
#' @export
apply_migration <- function(conn,
                                form,
                                plan = NULL,
                                user = NULL) {
  if (is.null(plan)) {
    plan <- plan_migration(conn, form)
  }

  if (!inherits(plan, "sft_migration_plan")) {
    stop("plan must be an sft_migration_plan object.", call. = FALSE)
  }

  init_system_tables(conn)

  actions <- plan$actions

  unsafe_actions <- actions[!actions$safe, , drop = FALSE]

  if (nrow(unsafe_actions) > 0L) {
    stop(
      "Migration contains unsafe/manual actions: ",
      paste(unique(unsafe_actions$action), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (nrow(actions) == 0L) {
    sft_register_form_schema(conn, form, user = user)
    return(invisible(plan))
  }

  if (sft_supports_transactional_ddl(conn)) {
    # Apply the whole plan atomically, so a failure midway rolls back instead of
    # leaving a half-migrated schema. A plain transaction (not
    # sft_db_with_transaction) is used because migration failures are not
    # retryable id/version conflicts.
    DBI::dbWithTransaction(
      conn,
      sft_run_migration_actions(conn, form, actions, user = user)
    )
  } else {
    # MariaDB implicitly commits each DDL statement, so a transaction cannot
    # roll the plan back; apply directly.
    sft_run_migration_actions(conn, form, actions, user = user)
  }

  invisible(plan)
}

#' Print a migration plan
#'
#' @param x A migration plan from [plan_migration()].
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#'
#' plan <- plan_migration(conn, contacts)
#' print(plan)
#'
#' db_disconnect(conn)
#' @export
print.sft_migration_plan <- function(x, ...) {
  cat("<sft_migration_plan>\n")
  cat("  form_id:    ", x$form$form_id, "\n", sep = "")
  cat("  table_name: ", x$form$table_name, "\n", sep = "")
  cat("  actions:    ", nrow(x$actions), "\n", sep = "")

  if (nrow(x$actions) > 0L) {
    print(x$actions)
  }

  invisible(x)
}