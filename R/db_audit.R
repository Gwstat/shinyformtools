sft_next_version_no <- function(conn, form, record_id = NULL, record_uuid = NULL) {
  if (!is.null(record_id)) {
    res <- DBI::dbGetQuery(
      conn,
      "
      SELECT COALESCE(MAX(version_no), 0) + 1 AS version_no
      FROM sft_audit_log
      WHERE form_id = ? AND table_name = ? AND record_id = ?
      ",
      params = list(form$form_id, form$table_name, record_id)
    )

    return(res$version_no[1])
  }

  if (!is.null(record_uuid)) {
    res <- DBI::dbGetQuery(
      conn,
      "
      SELECT COALESCE(MAX(version_no), 0) + 1 AS version_no
      FROM sft_audit_log
      WHERE form_id = ? AND table_name = ? AND record_uuid = ?
      ",
      params = list(form$form_id, form$table_name, record_uuid)
    )

    return(res$version_no[1])
  }

  1L
}

sft_changed_fields <- function(old_data, new_data) {
  if (is.null(old_data) || is.null(new_data)) {
    return(character())
  }

  if (is.data.frame(old_data)) {
    old_data <- sft_row_to_list(old_data)
  }

  if (is.data.frame(new_data)) {
    new_data <- sft_row_to_list(new_data)
  }

  fields <- union(names(old_data), names(new_data))

  changed <- vapply(
    fields,
    function(field) {
      old_value <- old_data[[field]]
      new_value <- new_data[[field]]

      !identical(
        as.character(old_value),
        as.character(new_value)
      )
    },
    logical(1)
  )

  fields[changed]
}

#' Write an audit log entry
#'
#' @param conn A DBI connection.
#' @param form Object created with [form()].
#' @param action Audit action, for example `"insert"`, `"update"` or
#'   `"delete"`.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param old_data Optional old record snapshot.
#' @param new_data Optional new record snapshot.
#' @param changed_fields Optional character vector of changed fields.
#' @param changed_by Optional user identifier.
#' @param reason Optional reason.
#'
#' @return Invisibly returns `TRUE`.
#' @examples
#' \dontrun{
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#'
#' # CRUD helpers write audit entries for you; this shows the lower-level call.
#' write_audit_log(
#'   conn = conn, form = contacts, action = "note",
#'   record_id = rec$sft_id[1],
#'   new_data = list(name = "Ada"),
#'   changed_by = "demo", reason = "Manual audit entry."
#' )
#'
#' db_disconnect(conn)
#' }
#' @keywords internal
write_audit_log <- function(conn,
                                form,
                                action,
                                record_id = NULL,
                                record_uuid = NULL,
                                old_data = NULL,
                                new_data = NULL,
                                changed_fields = NULL,
                                changed_by = NULL,
                                reason = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!sft_is_scalar_character(action)) {
    stop("action must be a non-empty character scalar.", call. = FALSE)
  }

  if (is.null(changed_fields)) {
    changed_fields <- sft_changed_fields(old_data, new_data)
  }

  version_no <- sft_next_version_no(
    conn = conn,
    form = form,
    record_id = record_id,
    record_uuid = record_uuid
  )

  columns <- c(
    "form_id",
    "table_name",
    "record_id",
    "record_uuid",
    "action",
    "version_no",
    "changed_at",
    "changed_by",
    "old_data_json",
    "new_data_json",
    "changed_fields_json",
    "reason"
  )

  values <- list(
    form$form_id,
    form$table_name,
    sft_db_param(record_id, default = NA_integer_),
    sft_db_param(record_uuid),
    action,
    version_no,
    sft_now(),
    sft_db_param(changed_by),
    if (is.null(old_data)) NA_character_ else as.character(sft_as_json(old_data)),
    if (is.null(new_data)) NA_character_ else as.character(sft_as_json(new_data)),
    as.character(sft_as_json(changed_fields)),
    sft_db_param(reason)
  )

  prepared <- sft_prepend_explicit_id(
    conn, "sft_audit_log", "log_id", columns, values
  )
  columns <- prepared$columns
  values <- prepared$values

  DBI::dbExecute(
    conn,
    paste0(
      "INSERT INTO sft_audit_log (",
      sft_sql_quoted_columns(conn, columns),
      ") VALUES (",
      paste(rep("?", length(values)), collapse = ", "),
      ")"
    ),
    params = values
  )

  invisible(TRUE)
}
