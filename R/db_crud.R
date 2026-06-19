sft_record_field_values <- function(form,
                                    record,
                                    include_missing = FALSE,
                                    editable_only = FALSE) {
  if (is.data.frame(record)) {
    record <- sft_row_to_list(record)
  }

  fields <- if (isTRUE(editable_only)) {
    sft_editable_input_fields(form)
  } else {
    sft_active_input_fields(form)
  }
  values <- list()

  for (field in fields) {
    if (sft_record_has_value(record, field)) {
      value <- sft_field_db_value(
        field = field,
        value = sft_record_get_value(record, field)
      )
      # Unique fields: store empty input as NULL so the composite unique index
      # (db_column, sft_unique_slot) treats it as distinct, matching the
      # application check which exempts empty values from uniqueness.
      if (isTRUE(field$unique) && sft_is_empty_value(value)) {
        value <- NA
      }
      values[[field$db_column]] <- value
    } else if (isTRUE(include_missing)) {
      values[[field$db_column]] <- NA_character_
    }
  }

  values
}

sft_get_record <- function(conn,
                           form,
                           record_id = NULL,
                           record_uuid = NULL,
                           include_deleted = FALSE) {
  if (is.null(record_id) && is.null(record_uuid)) {
    stop("record_id or record_uuid must be supplied.", call. = FALSE)
  }

  if (!is.null(record_id)) {
    sql <- paste0(
      "SELECT * FROM ",
      sft_quote_identifier(conn, form$table_name),
      " WHERE sft_id = ?"
    )

    params <- list(record_id)
  } else {
    sql <- paste0(
      "SELECT * FROM ",
      sft_quote_identifier(conn, form$table_name),
      " WHERE sft_uuid = ?"
    )

    params <- list(record_uuid)
  }

  if (!isTRUE(include_deleted)) {
    sql <- paste0(sql, " AND sft_is_deleted = 0")
  }

  out <- DBI::dbGetQuery(conn, sql, params = params)

  if (nrow(out) == 0L) {
    stop("Record not found.", call. = FALSE)
  }

  if (nrow(out) > 1L) {
    stop("Record lookup returned more than one row.", call. = FALSE)
  }

  out
}

#' Fetch form records
#'
#' @param form Object created with [form()].
#' @param conn Optional DBI connection.
#' @param include_deleted Logical. Whether soft-deleted records are included.
#'
#' @return A data frame.
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
#' insert_record(contacts, list(name = "Ada", email = "ada@example.org"),
#'               conn = conn, user = "demo")
#' fetch_records(contacts, conn = conn)
#' db_disconnect(conn)
#' @export
fetch_records <- function(form,
                              conn = NULL,
                              include_deleted = FALSE) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  sft_ensure_schema(conn, form)

  sql <- paste0(
    "SELECT * FROM ",
    sft_quote_identifier(conn, form$table_name)
  )

  if (!isTRUE(include_deleted)) {
    sql <- paste0(sql, " WHERE sft_is_deleted = 0")
  }

  sql <- paste0(sql, " ORDER BY sft_id")

  DBI::dbGetQuery(conn, sql)
}

#' Insert a form record
#'
#' @param form Object created with [form()].
#' @param record Named list or one-row data frame.
#' @param conn Optional DBI connection.
#' @param user Optional user identifier.
#' @param reason Optional reason for audit log.
#'
#' @return The inserted record as a one-row data frame.
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
#' insert_record(contacts, list(name = "Ada", email = "ada@example.org"),
#'               conn = conn, user = "demo")
#' db_disconnect(conn)
#' @export
insert_record <- function(form,
                              record,
                              conn = NULL,
                              user = NULL,
                              reason = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  sft_ensure_schema(conn, form, user = user)

  sft_db_with_transaction(conn, {
    validate_record(
      form = form,
      record = record,
      conn = conn,
      require_all_mandatory = TRUE
    )

    now <- sft_now()

    field_values <- sft_record_field_values(
      form = form,
      record = record,
      include_missing = FALSE
    )

    explicit_sft_id <- if (sft_requires_explicit_sft_id(conn)) {
      sft_next_sft_id(conn, form$table_name)
    } else {
      NULL
    }

    system_values <- list(
      sft_uuid = uuid::UUIDgenerate(),
      sft_form_id = form$form_id,
      sft_form_version = form$version,
      sft_schema_hash = sft_schema_signature(form),
      sft_created_at = now,
      sft_created_by = sft_db_param(user),
      sft_updated_at = now,
      sft_updated_by = sft_db_param(user),
      sft_is_deleted = 0L,
      sft_unique_slot = 0L
    )

    if (!is.null(explicit_sft_id)) {
      system_values <- c(
        list(sft_id = explicit_sft_id),
        system_values
      )
    }

    values <- c(system_values, field_values)

    columns_sql <- sft_sql_quoted_columns(conn, names(values))

    placeholders <- paste(rep("?", length(values)), collapse = ", ")

    sql <- paste0(
      "INSERT INTO ",
      sft_quote_identifier(conn, form$table_name),
      " (",
      columns_sql,
      ") VALUES (",
      placeholders,
      ")"
    )

    DBI::dbExecute(conn, sql, params = unname(values))

    new_id <- explicit_sft_id %||% sft_last_insert_id(conn)
    new_easy_id <- paste0(new_id, "-", sft_random_letters())

    DBI::dbExecute(
      conn,
      paste0(
        "UPDATE ",
        sft_quote_identifier(conn, form$table_name),
        " SET sft_easy_id = ? WHERE sft_id = ?"
      ),
      params = list(new_easy_id, new_id)
    )

    new_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = new_id,
      include_deleted = TRUE
    )

    write_audit_log(
      conn = conn,
      form = form,
      action = "insert",
      record_id = new_record$sft_id[1],
      record_uuid = new_record$sft_uuid[1],
      old_data = NULL,
      new_data = new_record,
      changed_fields = names(values),
      changed_by = user,
      reason = reason
    )

    new_record
  })
}

#' Update a form record
#'
#' @param form Object created with [form()].
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param values Named list of values to update.
#' @param conn Optional DBI connection.
#' @param user Optional user identifier.
#' @param reason Optional reason for audit log.
#'
#' @return The updated record as a one-row data frame.
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
#' added <- insert_record(contacts, list(name = "Ada", email = "ada@example.org"),
#'                        conn = conn, user = "demo")
#' update_record(contacts, list(email = "ada@new.org"),
#'               record_id = added$sft_id, conn = conn, user = "demo")
#' db_disconnect(conn)
#' @export
update_record <- function(form,
                              values,
                              record_id = NULL,
                              record_uuid = NULL,
                              conn = NULL,
                              user = NULL,
                              reason = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  sft_ensure_schema(conn, form, user = user)

  sft_db_with_transaction(conn, {
    old_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = record_id,
      record_uuid = record_uuid,
      include_deleted = FALSE
    )

    editable_field_ids <- vapply(
      sft_editable_input_fields(form),
      function(field) field$id,
      character(1)
    )

    editable_field_columns <- vapply(
      sft_editable_input_fields(form),
      function(field) field$db_column,
      character(1)
    )

    if (is.data.frame(values)) {
      values <- sft_row_to_list(values)
    }

    values <- as.list(values)
    values <- values[intersect(names(values), c(editable_field_ids, editable_field_columns))]

    empty_supplied_mandatory <- sft_supplied_empty_mandatory_fields(
      form = form,
      record = values
    )

    if (length(empty_supplied_mandatory) > 0L) {
      stop(
        sft_message(
          form = form,
          key = "mandatory_empty",
          values = list(fields = paste(empty_supplied_mandatory, collapse = ", "))
        ),
        call. = FALSE
      )
    }

    field_values <- sft_record_field_values(
      form = form,
      record = values,
      include_missing = FALSE,
      editable_only = TRUE
    )

    if (length(field_values) == 0L) {
      stop(
        sft_message(
          form = form,
          key = "no_active_fields_for_update"
        ),
        call. = FALSE
      )
    }

    merged_record <- sft_row_to_list(old_record)

    for (column_name in names(field_values)) {
      merged_record[[column_name]] <- field_values[[column_name]]
    }

    if (identical(form$on_edit_missing_required, "require")) {
      validate_record(
        form = form,
        record = merged_record,
        conn = conn,
        current_id = old_record$sft_id[1],
        require_all_mandatory = TRUE
      )
    } else {
      missing <- sft_missing_mandatory_fields(form, merged_record)

      if (
        length(missing) > 0L &&
          identical(form$on_edit_missing_required, "warn")
      ) {
        warning(
          sft_message(
            form = form,
            key = "mandatory_missing",
            values = list(fields = paste(missing, collapse = ", "))
          ),
          call. = FALSE
        )
      }

      validate_record(
        form = form,
        record = merged_record,
        conn = conn,
        current_id = old_record$sft_id[1],
        require_all_mandatory = FALSE
      )
    }

    now <- sft_now()

    update_values <- c(
      field_values,
      list(
        # Mirror the insert path: the row is now written under the current
        # schema, so refresh its structural-conformance marker. sft_form_version
        # is intentionally left as creation provenance.
        sft_schema_hash = sft_schema_signature(form),
        sft_updated_at = now,
        sft_updated_by = sft_db_param(user)
      )
    )

    set_sql <- paste(
      vapply(
        names(update_values),
        function(column_name) {
          paste0(sft_quote_identifier(conn, column_name), " = ?")
        },
        character(1)
      ),
      collapse = ", "
    )

    sql <- paste0(
      "UPDATE ",
      sft_quote_identifier(conn, form$table_name),
      " SET ",
      set_sql,
      " WHERE sft_id = ?"
    )

    DBI::dbExecute(
      conn,
      sql,
      params = c(
        unname(update_values),
        list(old_record$sft_id[1])
      )
    )

    new_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = old_record$sft_id[1],
      include_deleted = TRUE
    )

    write_audit_log(
      conn = conn,
      form = form,
      action = "update",
      record_id = new_record$sft_id[1],
      record_uuid = new_record$sft_uuid[1],
      old_data = old_record,
      new_data = new_record,
      changed_fields = names(field_values),
      changed_by = user,
      reason = reason
    )

    new_record
  })
}

#' Soft-delete a form record
#'
#' @param form Object created with [form()].
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param conn Optional DBI connection.
#' @param user Optional user identifier.
#' @param reason Optional reason for audit log.
#'
#' @return The deleted record as a one-row data frame.
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
#' added <- insert_record(contacts, list(name = "Ada", email = "ada@example.org"),
#'                        conn = conn, user = "demo")
#' soft_delete_record(contacts, record_id = added$sft_id,
#'                    conn = conn, user = "demo", reason = "duplicate")
#' nrow(fetch_records(contacts, conn = conn))
#' db_disconnect(conn)
#' @export
soft_delete_record <- function(form,
                                   record_id = NULL,
                                   record_uuid = NULL,
                                   conn = NULL,
                                   user = NULL,
                                   reason = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  sft_ensure_schema(conn, form, user = user)

  sft_db_with_transaction(conn, {
    old_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = record_id,
      record_uuid = record_uuid,
      include_deleted = FALSE
    )

    now <- sft_now()

    DBI::dbExecute(
      conn,
      paste0(
        "UPDATE ",
        sft_quote_identifier(conn, form$table_name),
        " SET sft_is_deleted = 1,
              sft_deleted_at = ?,
              sft_deleted_by = ?,
              sft_updated_at = ?,
              sft_updated_by = ?,
              sft_unique_slot = ?
          WHERE sft_id = ?"
      ),
      params = list(
        now,
        sft_db_param(user),
        now,
        sft_db_param(user),
        old_record$sft_id[1],
        old_record$sft_id[1]
      )
    )

    new_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = old_record$sft_id[1],
      include_deleted = TRUE
    )

    write_audit_log(
      conn = conn,
      form = form,
      action = "delete",
      record_id = new_record$sft_id[1],
      record_uuid = new_record$sft_uuid[1],
      old_data = old_record,
      new_data = new_record,
      changed_fields = c(
        "sft_is_deleted",
        "sft_deleted_at",
        "sft_deleted_by"
      ),
      changed_by = user,
      reason = reason
    )

    new_record
  })
}
