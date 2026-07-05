# Compact changelog helpers for modal headers and linked workflows.

sft_context_form <- function(context) {
  if (is.null(context) || is.null(context$form)) {
    return(NULL)
  }

  context$form
}

sft_context_conn <- function(context) {
  if (is.null(context) || is.null(context$conn)) {
    return(NULL)
  }

  context$conn
}

sft_record_identifier <- function(record = NULL,
                                  record_id = NULL,
                                  record_uuid = NULL) {
  if (!is.null(record_id)) {
    return(list(record_id = record_id, record_uuid = record_uuid))
  }

  if (!is.null(record_uuid)) {
    return(list(record_id = record_id, record_uuid = record_uuid))
  }

  if (is.null(record)) {
    return(list(record_id = NULL, record_uuid = NULL))
  }

  if (is.data.frame(record) && nrow(record) == 1L) {
    record <- sft_row_to_list(record)
  }

  if (!is.list(record)) {
    return(list(record_id = NULL, record_uuid = NULL))
  }

  list(
    record_id = record$sft_id %||% NULL,
    record_uuid = record$sft_uuid %||% NULL
  )
}

sft_parse_changed_fields <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) {
    return(character())
  }

  out <- tryCatch(
    jsonlite::fromJSON(x),
    error = function(err) character()
  )

  if (is.null(out) || length(out) == 0L) {
    return(character())
  }

  as.character(out)
}

sft_compact_changed_fields <- function(x, max_fields = 4L) {
  fields <- sft_parse_changed_fields(x)

  if (length(fields) == 0L) {
    return("")
  }

  if (length(fields) <= max_fields) {
    return(paste(fields, collapse = ", "))
  }

  paste0(
    paste(fields[seq_len(max_fields)], collapse = ", "),
    ", +",
    length(fields) - max_fields,
    " more"
  )
}

#' Fetch compact audit history
#'
#' @param context Optional form server context as supplied to hooks.
#' @param form Optional form object. Ignored when supplied by `context`.
#' @param conn Optional DBI connection. Ignored when supplied by `context`.
#' @param record Optional one-row record containing `sft_id` or `sft_uuid`.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param limit Maximum number of entries returned.
#' @param newest_first Logical. Whether newest entries should be returned first.
#'
#' @return A data frame with compact audit history entries.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#'
#' # Pass form + conn directly (in a form_server hook you pass `context`).
#' audit_history(form = contacts, conn = conn, record_id = rec$sft_id[1])
#'
#' db_disconnect(conn)
#' @export
audit_history <- function(context = NULL,
                              form = NULL,
                              conn = NULL,
                              record = NULL,
                              record_id = NULL,
                              record_uuid = NULL,
                              limit = 5L,
                              newest_first = TRUE) {
  form <- form %||% sft_context_form(context)
  conn <- conn %||% sft_context_conn(context)

  if (is.null(form) || is.null(conn)) {
    return(data.frame())
  }

  ids <- sft_record_identifier(
    record = record,
    record_id = record_id,
    record_uuid = record_uuid
  )

  if (is.null(ids$record_id) && is.null(ids$record_uuid)) {
    return(data.frame())
  }

  audit <- fetch_audit_log(
    form = form,
    conn = conn,
    record_id = ids$record_id,
    record_uuid = ids$record_uuid
  )

  if (nrow(audit) == 0L) {
    return(audit)
  }

  order_index <- order(audit$version_no, audit$log_id, decreasing = isTRUE(newest_first))
  audit <- audit[order_index, , drop = FALSE]

  if (!is.null(limit) && is.numeric(limit) && length(limit) == 1L && !is.na(limit)) {
    audit <- audit[seq_len(min(nrow(audit), as.integer(limit))), , drop = FALSE]
  }

  audit
}

#' Create a changelog box for modal headers
#'
#' @param context Form server context as supplied to `modal_header` hooks.
#' @param record Optional one-row record containing `sft_id` or `sft_uuid`.
#' @param record_id Optional `sft_id`.
#' @param record_uuid Optional `sft_uuid`.
#' @param title Box title.
#' @param limit Maximum number of entries to show.
#' @param empty_text Text shown when no history is available.
#' @param datetime_format Timestamp display format.
#' @param show_version Logical. Whether to show the version number in the compact history.
#' @param show_fields Logical. Whether to show compact changed-field names.
#'
#' @return Shiny UI.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' conn <- db_connect(db)
#' init_db(contacts, conn = conn, user = "demo")
#' rec <- insert_record(contacts, list(name = "Ada"), conn = conn, user = "demo")
#'
#' # `context` is normally supplied by a form_server() modal_header hook;
#' # here a plain list(form, conn) stands in for it.
#' context <- list(form = contacts, conn = conn)
#' changelog_box(context, record_id = rec$sft_id[1], title = "History")
#'
#' db_disconnect(conn)
#' @export
changelog_box <- function(context,
                              record = NULL,
                              record_id = NULL,
                              record_uuid = NULL,
                              title = "Changelog",
                              limit = 5L,
                              empty_text = "No changes yet.",
                              datetime_format = sft_default_datetime_format(),
                              show_version = FALSE,
                              show_fields = FALSE) {
  audit <- audit_history(
    context = context,
    record = record,
    record_id = record_id,
    record_uuid = record_uuid,
    limit = limit,
    newest_first = TRUE
  )

  if (!is.data.frame(audit) || nrow(audit) == 0L) {
    return(
      shiny::div(
        class = "sft-changelog-box",
        shiny::tags$strong(title),
        shiny::tags$p(empty_text)
      )
    )
  }

  tl <- sft_table_labels()

  rows <- lapply(seq_len(nrow(audit)), function(i) {
    action <- sft_format_action_value(audit$action[[i]])
    changed_at <- sft_format_datetime_value(audit$changed_at[[i]], datetime_format)
    changed_at <- if (is.na(changed_at)) "" else changed_at
    changed_by <- audit$changed_by[[i]] %||% ""
    changed_by <- if (is.na(changed_by)) "" else changed_by
    changed_fields <- sft_compact_changed_fields(audit$changed_fields_json[[i]])

    shiny::tags$li(
      shiny::tags$span(
        if (nzchar(changed_at)) changed_at else "",
        if (nzchar(changed_by)) paste0(" \u00b7 ", changed_by) else "",
        if (nzchar(action)) paste0(" \u00b7 ", action) else "",
        if (isTRUE(show_version)) paste0(" \u00b7 ", tl$changelog_version, " ", audit$version_no[[i]]) else ""
      ),
      if (isTRUE(show_fields) && nzchar(changed_fields)) {
        shiny::tagList(
          shiny::tags$br(),
          shiny::tags$small(paste0(tl$changelog_fields, ": ", changed_fields))
        )
      }
    )
  })

  shiny::div(
    class = "sft-changelog-box",
    style = paste(
      "max-height: 160px;",
      "overflow-y: auto;",
      "padding: 0.5rem 0.75rem;",
      "font-size: 0.9em;",
      "border: 1px solid #ddd;",
      "border-radius: 4px;",
      "background: #fff;"
    ),
    shiny::tags$strong(title),
    do.call(
      shiny::tags$ul,
      c(
        list(style = "margin: 0.5rem 0 0 1rem; padding-left: 0.5rem;"),
        rows
      )
    )
  )
}
