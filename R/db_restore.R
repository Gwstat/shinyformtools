sft_json_to_record <- function(json) {
  if (is.null(json) || length(json) == 0L || is.na(json) || !nzchar(json)) {
    stop("JSON snapshot is empty.", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)

  if (is.data.frame(parsed)) {
    if (nrow(parsed) != 1L) {
      stop("JSON snapshot must contain exactly one record.", call. = FALSE)
    }

    return(sft_row_to_list(parsed))
  }

  if (is.list(parsed)) {
    return(parsed)
  }

  stop("JSON snapshot could not be converted to a record.", call. = FALSE)
}

sft_restore_values_from_snapshot <- function(conn,
                                             form,
                                             snapshot,
                                             current_record,
                                             reactivate = TRUE,
                                             user = NULL) {
  table_info <- sft_table_info(conn, form$table_name)
  current_columns <- table_info$name

  restore_values <- snapshot[
    intersect(names(snapshot), current_columns)
  ]

  restore_values[["sft_id"]] <- NULL

  if (isTRUE(reactivate)) {
    restore_values[["sft_is_deleted"]] <- 0L
    restore_values[["sft_deleted_at"]] <- NA_character_
    restore_values[["sft_deleted_by"]] <- NA_character_
    restore_values[["sft_unique_slot"]] <- 0L
  } else {
    # Keep the record in its current deleted state. The chosen snapshot is a
    # non-deleted version, so it carries the live delete flags and unique slot
    # (sft_is_deleted = 0, sft_unique_slot = 0); writing those back would
    # silently revive the record and reclaim the live unique slot, contradicting
    # reactivate = FALSE. Drop them so the UPDATE leaves the delete-state alone.
    restore_values[["sft_is_deleted"]] <- NULL
    restore_values[["sft_deleted_at"]] <- NULL
    restore_values[["sft_deleted_by"]] <- NULL
    restore_values[["sft_unique_slot"]] <- NULL
  }

  restore_values[["sft_updated_at"]] <- sft_now()
  restore_values[["sft_updated_by"]] <- sft_db_param(user)

  restore_values
}

sft_get_restore_audit_row <- function(conn,
                                      form,
                                      record_id,
                                      record_uuid,
                                      version_no = NULL) {
  if (!is.null(version_no)) {
    out <- DBI::dbGetQuery(
      conn,
      "
      SELECT *
      FROM sft_audit_log
      WHERE form_id = ?
        AND table_name = ?
        AND record_id = ?
        AND version_no = ?
      ORDER BY log_id DESC
      LIMIT 1
      ",
      params = list(
        form$form_id,
        form$table_name,
        record_id,
        version_no
      )
    )

    if (nrow(out) == 0L && !is.null(record_uuid)) {
      out <- DBI::dbGetQuery(
        conn,
        "
        SELECT *
        FROM sft_audit_log
        WHERE form_id = ?
          AND table_name = ?
          AND record_uuid = ?
          AND version_no = ?
        ORDER BY log_id DESC
        LIMIT 1
        ",
        params = list(
          form$form_id,
          form$table_name,
          record_uuid,
          version_no
        )
      )
    }

    return(out)
  }

  out <- DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM sft_audit_log
    WHERE form_id = ?
      AND table_name = ?
      AND record_id = ?
      AND action IN ('insert', 'update', 'restore')
      AND new_data_json IS NOT NULL
    ORDER BY version_no DESC, log_id DESC
    LIMIT 1
    ",
    params = list(
      form$form_id,
      form$table_name,
      record_id
    )
  )

  if (nrow(out) == 0L && !is.null(record_uuid)) {
    out <- DBI::dbGetQuery(
      conn,
      "
      SELECT *
      FROM sft_audit_log
      WHERE form_id = ?
        AND table_name = ?
        AND record_uuid = ?
        AND action IN ('insert', 'update', 'restore')
        AND new_data_json IS NOT NULL
      ORDER BY version_no DESC, log_id DESC
      LIMIT 1
      ",
      params = list(
        form$form_id,
        form$table_name,
        record_uuid
      )
    )
  }

  out
}

#' Fetch audit log entries
#'
#' @param form Object created with [form()].
#' @param conn Optional DBI connection.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#'
#' @return A data frame with audit log entries.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#'
#' fetch_audit_log(contacts, conn = conn)
#'
#' db_disconnect(conn)
#' @export
fetch_audit_log <- function(form,
                                conn = NULL,
                                record_id = NULL,
                                record_uuid = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  init_db(form, conn = conn, apply = TRUE)

  sql <- "
    SELECT *
    FROM sft_audit_log
    WHERE form_id = ?
      AND table_name = ?
  "

  params <- list(form$form_id, form$table_name)

  if (!is.null(record_id)) {
    sql <- paste0(sql, " AND record_id = ?")
    params <- c(params, list(record_id))
  }

  if (!is.null(record_uuid)) {
    sql <- paste0(sql, " AND record_uuid = ?")
    params <- c(params, list(record_uuid))
  }

  sql <- paste0(sql, " ORDER BY record_id, version_no, log_id")

  DBI::dbGetQuery(conn, sql, params = params)
}

#' List available record versions
#'
#' @param form Object created with [form()].
#' @param conn Optional DBI connection.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#'
#' @return A data frame with available versions.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#' update_record(
#'   contacts, list(name = "Ada Lovelace"),
#'   record_id = rec$sft_id[1], conn = conn, user = "demo"
#' )
#'
#' list_versions(contacts, conn = conn, record_id = rec$sft_id[1])
#'
#' db_disconnect(conn)
#' @export
list_versions <- function(form,
                              conn = NULL,
                              record_id = NULL,
                              record_uuid = NULL) {
  if (is.null(record_id) && is.null(record_uuid)) {
    stop("record_id or record_uuid must be supplied.", call. = FALSE)
  }

  audit <- fetch_audit_log(
    form = form,
    conn = conn,
    record_id = record_id,
    record_uuid = record_uuid
  )

  if (nrow(audit) == 0L) {
    return(audit)
  }

  audit[
    ,
    c(
      "log_id",
      "record_id",
      "record_uuid",
      "action",
      "version_no",
      "changed_at",
      "changed_by",
      "new_data_json",
      "changed_fields_json",
      "reason"
    ),
    drop = FALSE
  ]
}

#' Restore a record from the audit log
#'
#' Restores a record from a stored audit snapshot. If `version_no` is omitted,
#' the latest non-deleted snapshot from an insert, update or restore action is
#' used.
#'
#' @param form Object created with [form()].
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param version_no Optional audit version number to restore.
#' @param conn Optional DBI connection.
#' @param user Optional user identifier.
#' @param reason Optional reason for audit log.
#' @param reactivate Logical. If `TRUE` (default), the restored record is marked
#'   live (not deleted) and reclaims the live unique slot. If `FALSE`, the
#'   record's field values are restored but its current deleted state is left
#'   untouched, so a soft-deleted record stays soft-deleted.
#'
#' @return The restored record as a one-row data frame.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#' update_record(
#'   contacts, list(name = "Ada Lovelace"),
#'   record_id = rec$sft_id[1], conn = conn, user = "demo"
#' )
#'
#' # Restore the original (version 1) field values.
#' restore_record(
#'   contacts,
#'   record_id = rec$sft_id[1], version_no = 1,
#'   conn = conn, user = "demo"
#' )
#'
#' db_disconnect(conn)
#' @export
restore_record <- function(form,
                               record_id = NULL,
                               record_uuid = NULL,
                               version_no = NULL,
                               conn = NULL,
                               user = NULL,
                               reason = NULL,
                               reactivate = TRUE) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (is.null(record_id) && is.null(record_uuid)) {
    stop("record_id or record_uuid must be supplied.", call. = FALSE)
  }

  conn <- sft_resolve_connection(form, conn)

  init_db(form, conn = conn, apply = TRUE, user = user)

  sft_db_with_transaction(conn, {
    current_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = record_id,
      record_uuid = record_uuid,
      include_deleted = TRUE
    )

    resolved_record_id <- current_record$sft_id[1]
    resolved_record_uuid <- current_record$sft_uuid[1]

    audit_row <- sft_get_restore_audit_row(
      conn = conn,
      form = form,
      record_id = resolved_record_id,
      record_uuid = resolved_record_uuid,
      version_no = version_no
    )

    if (nrow(audit_row) == 0L) {
      stop("No audit snapshot found for restore.", call. = FALSE)
    }

    if (
      is.na(audit_row$new_data_json[1]) ||
        !nzchar(audit_row$new_data_json[1])
    ) {
      stop("Selected audit version has no restorable snapshot.", call. = FALSE)
    }

    snapshot <- sft_json_to_record(audit_row$new_data_json[1])

    restore_values <- sft_restore_values_from_snapshot(
      conn = conn,
      form = form,
      snapshot = snapshot,
      current_record = current_record,
      reactivate = reactivate,
      user = user
    )

    if (length(restore_values) == 0L) {
      stop("No values available for restore.", call. = FALSE)
    }

    # Friendly pre-check: when the record is being reactivated (slot back to 0),
    # reject the restore up front if a unique field's restored value is already
    # held by a live row, instead of surfacing the raw database constraint error
    # from the UPDATE below. Excludes this record itself. A non-reactivating
    # restore leaves the row soft-deleted (slot = sft_id), so it cannot collide.
    if (isTRUE(reactivate)) {
      unique_errors <- sft_validate_unique_fields(
        form = form,
        record = restore_values,
        conn = conn,
        current_id = resolved_record_id
      )

      if (length(unique_errors) > 0L) {
        stop(
          paste0(
            "Cannot restore: a unique field's value is already held by an ",
            "active record. ",
            paste(unique_errors, collapse = " ")
          ),
          call. = FALSE
        )
      }
    }

    set_sql <- paste(
      vapply(
        names(restore_values),
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

    params <- unname(lapply(
      restore_values,
      function(value) {
        sft_db_param(sft_clean_db_value(value))
      }
    ))

    params <- c(params, list(resolved_record_id))

    tryCatch(
      DBI::dbExecute(conn, sql, params = params),
      error = function(e) {
        stop(
          paste0(
            "Restore failed. If a unique field's value is now used by an ",
            "active record, the restore is rejected. Original error: ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )

    restored_record <- sft_get_record(
      conn = conn,
      form = form,
      record_id = resolved_record_id,
      include_deleted = TRUE
    )

    write_audit_log(
      conn = conn,
      form = form,
      action = "restore",
      record_id = restored_record$sft_id[1],
      record_uuid = restored_record$sft_uuid[1],
      old_data = current_record,
      new_data = restored_record,
      changed_fields = names(restore_values),
      changed_by = user,
      reason = reason %||% paste0(
        "Restored from version ",
        audit_row$version_no[1],
        "."
      )
    )

    restored_record
  })
}


#' List restorable record versions
#'
#' Returns audit versions that contain a restorable non-delete snapshot.
#'
#' @param form Object created with [form()].
#' @param conn Optional DBI connection.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#'
#' @return A data frame with restorable versions.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#' update_record(
#'   contacts, list(name = "Ada Lovelace"),
#'   record_id = rec$sft_id[1], conn = conn, user = "demo"
#' )
#'
#' list_restorable_versions(contacts, conn = conn, record_id = rec$sft_id[1])
#'
#' db_disconnect(conn)
#' @export
list_restorable_versions <- function(form,
                                         conn = NULL,
                                         record_id = NULL,
                                         record_uuid = NULL) {
  versions <- list_versions(
    form = form,
    conn = conn,
    record_id = record_id,
    record_uuid = record_uuid
  )

  if (nrow(versions) == 0L) {
    return(versions)
  }

  versions[
    versions$action %in% c("insert", "update", "restore"),
    ,
    drop = FALSE
  ]
}