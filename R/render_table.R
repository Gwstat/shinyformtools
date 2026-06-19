sft_system_column_labels <- function() {
  tl <- sft_table_labels()
  cols <- c(
    "sft_id", "sft_uuid", "sft_easy_id", "sft_form_id", "sft_form_version",
    "sft_schema_hash", "sft_created_at", "sft_created_by", "sft_updated_at",
    "sft_updated_by", "sft_deleted_at", "sft_deleted_by", "sft_is_deleted"
  )
  out <- vapply(cols, function(key) as.character(tl[[key]]), character(1))
  names(out) <- cols
  out
}

sft_field_column_labels <- function(form) {
  fields <- sft_active_input_fields(form)

  labels <- vapply(
    fields,
    function(field) field$label,
    character(1)
  )

  names(labels) <- vapply(
    fields,
    function(field) field$db_column,
    character(1)
  )

  labels
}

sft_column_labels <- function(form) {
  c(
    sft_system_column_labels(),
    sft_field_column_labels(form)
  )
}


sft_default_datetime_format <- function() {
  "%d.%m.%Y %H:%M"
}

sft_format_datetime_value <- function(x, datetime_format = sft_default_datetime_format()) {
  if (is.null(x)) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(format(x, datetime_format))
  }

  x_chr <- as.character(x)
  out <- x_chr
  needs_format <- !is.na(x_chr) & nzchar(x_chr)

  if (!any(needs_format)) {
    return(out)
  }

  parsed <- suppressWarnings(
    as.POSIXct(
      x_chr[needs_format],
      tz = "UTC",
      tryFormats = c(
        "%Y-%m-%d %H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%S%z",
        "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%OS",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d"
      )
    )
  )

  formatted <- ifelse(
    is.na(parsed),
    x_chr[needs_format],
    format(parsed, datetime_format)
  )

  out[needs_format] <- formatted
  out
}



sft_format_field_display_columns <- function(data, form) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(data)
  }

  fields <- sft_active_input_fields(form)

  for (field in fields) {
    column <- field$db_column

    if (!column %in% names(data)) {
      next
    }

    data[[column]] <- vapply(
      data[[column]],
      function(value) {
        out <- sft_format_field_display_value(field = field, value = value)

        if (is.null(out) || length(out) == 0L || is.na(out[1L])) {
          return(NA_character_)
        }

        as.character(out[1L])
      },
      character(1)
    )

    if (isTRUE(field$markdown)) {
      data[[column]] <- sft_render_markdown(data[[column]])
    }
  }

  data
}

sft_format_display_data <- function(data, datetime_format = sft_default_datetime_format()) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(data)
  }

  datetime_columns <- grep("(^|_)at$", names(data), value = TRUE)

  for (column in datetime_columns) {
    data[[column]] <- sft_format_datetime_value(
      data[[column]],
      datetime_format = datetime_format
    )
  }

  if ("sft_is_deleted" %in% names(data)) {
    tl <- sft_table_labels()
    deleted <- data$sft_is_deleted == 1L | as.character(data$sft_is_deleted) == "1"
    deleted[is.na(deleted)] <- FALSE
    data$sft_is_deleted <- ifelse(deleted, as.character(tl$flag_yes), as.character(tl$flag_no))
  }

  data
}

sft_action_labels <- function() {
  tl <- sft_table_labels()
  c(
    insert = as.character(tl$action_insert),
    update = as.character(tl$action_update),
    delete = as.character(tl$action_delete),
    restore = as.character(tl$action_restore)
  )
}

# Relabel keys via a named label map, leaving keys without a mapping unchanged.
sft_relabel <- function(keys, labels) {
  replacement <- labels[keys]
  replacement[is.na(replacement)] <- keys[is.na(replacement)]
  unname(replacement)
}

sft_format_action_value <- function(x) {
  sft_relabel(as.character(x), sft_action_labels())
}

sft_display_column_labels <- function(form, display_column_labels = NULL) {
  labels <- sft_column_labels(form)

  if (is.null(display_column_labels)) {
    return(labels)
  }

  if (!is.character(display_column_labels) || is.null(names(display_column_labels))) {
    stop(
      "display_column_labels must be NULL or a named character vector.",
      call. = FALSE
    )
  }

  if (any(!nzchar(names(display_column_labels)))) {
    stop(
      "display_column_labels must have non-empty names.",
      call. = FALSE
    )
  }

  if (any(is.na(display_column_labels))) {
    stop(
      "display_column_labels must not contain NA values.",
      call. = FALSE
    )
  }

  labels[names(display_column_labels)] <- display_column_labels
  labels
}

sft_apply_column_labels <- function(data, form, display_column_labels = NULL) {
  labels <- sft_display_column_labels(
    form = form,
    display_column_labels = display_column_labels
  )
  names(data) <- sft_relabel(names(data), labels)

  data
}


sft_call_display_transform <- function(display_transform, data, context) {
  args <- list(data = data, context = context)
  formals_names <- names(formals(display_transform))

  if ("..." %in% formals_names) {
    return(do.call(display_transform, args))
  }

  do.call(display_transform, args[intersect(names(args), formals_names)])
}

sft_apply_display_transform <- function(data,
                                        display_transform = NULL,
                                        context = list()) {
  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  if (is.null(display_transform)) {
    return(data)
  }

  if (!is.function(display_transform)) {
    stop("display_transform must be NULL or a function.", call. = FALSE)
  }

  out <- sft_call_display_transform(
    display_transform = display_transform,
    data = data,
    context = context
  )

  if (!is.data.frame(out)) {
    stop("display_transform must return a data frame.", call. = FALSE)
  }

  if ("sft_id" %in% names(data) && !"sft_id" %in% names(out)) {
    stop(
      "display_transform must preserve the sft_id column so table selections can be mapped back to raw records.",
      call. = FALSE
    )
  }

  out
}

# DT escape argument that escapes every column except the given markdown-column
# positions (which already hold sanitized HTML). TRUE (escape all) when there are
# none. Negative indices mean "all columns except these", per DT::datatable.
sft_table_escape <- function(markdown_positions) {
  if (length(markdown_positions) == 0L) {
    return(TRUE)
  }

  -markdown_positions
}

sft_dt_options <- function(options = list()) {
  base <- list(
    pageLength = 10,
    scrollX = FALSE,
    autoWidth = FALSE
  )

  # German (or any) DataTables chrome (search box, pagination, info line) comes
  # from the DataTables `language` option, set globally by use_german(). A
  # per-table `language` in `options` still wins via modifyList below.
  dt_language <- getOption("shinyformtools.dt_language", NULL)
  if (is.list(dt_language) && length(dt_language) > 0L) {
    base$language <- dt_language
  }

  utils::modifyList(base, options)
}

sft_check_table_format <- function(table_format) {
  if (!is.null(table_format) && !is.function(table_format)) {
    stop("table_format must be NULL or a function.", call. = FALSE)
  }

  invisible(table_format)
}

sft_call_table_format <- function(table_format, table, data, context) {
  args <- list(table = table, data = data, context = context)
  formals_names <- names(formals(table_format))

  if ("..." %in% formals_names) {
    return(do.call(table_format, args))
  }

  do.call(table_format, args[intersect(names(args), formals_names)])
}

sft_apply_table_format <- function(table, data, table_format = NULL, context = list()) {
  if (is.null(table_format)) {
    return(table)
  }

  out <- sft_call_table_format(
    table_format = table_format,
    table = table,
    data = data,
    context = context
  )

  if (!inherits(out, "datatables")) {
    stop("table_format must return a DT table widget.", call. = FALSE)
  }

  out
}



sft_records_table_data <- function(data,
                                   form,
                                   columns = NULL,
                                   show_system_columns = FALSE,
                                   datetime_format = sft_default_datetime_format(),
                                   display_column_labels = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  columns <- sft_resolve_record_columns(
    form = form,
    data = data,
    columns = columns,
    show_system_columns = show_system_columns
  )

  out <- data[, columns, drop = FALSE]
  out <- sft_format_field_display_columns(out, form = form)
  out <- sft_format_display_data(out, datetime_format = datetime_format)
  out <- sft_apply_column_labels(
    data = out,
    form = form,
    display_column_labels = display_column_labels
  )

  # 1-based positions of markdown columns in the (resolved) column order, so the
  # caller can exclude them from DT escaping. Relabelling preserves order, so the
  # positions are valid against the labelled output too.
  attr(out, "sft_markdown_positions") <- which(columns %in% sft_markdown_columns(form))

  out
}

#' Create a DT table for form records
#'
#' @param data Data frame returned by [fetch_records()].
#' @param form Object created with [form()].
#' @param columns Optional character vector of columns to display.
#' @param show_system_columns Logical. Whether extended `sft_` columns are shown.
#' @param selection DT row selection mode.
#' @param options Additional DT options.
#' @param class CSS class passed to [DT::datatable()].
#' @param datetime_format Format used for displayed timestamps.
#' @param display_column_labels Optional named character vector with labels for
#'   additional display-only columns created by a display transform.
#' @param filter Passed to [DT::datatable()]'s `filter` argument. `"none"`
#'   (default), `"top"` or `"bottom"` add per-column search controls that adapt
#'   to each column's type (range slider for numeric columns, a select for
#'   factors, a text box otherwise).
#'
#' @return A DT table widget.
#' @examples
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name"),
#'     form_field(id = "city", label = "City")
#'   )
#' )
#' records <- data.frame(
#'   name = c("Ada", "Linus"),
#'   city = c("London", "Helsinki")
#' )
#' records_datatable(records, form = contacts)
#' @export
records_datatable <- function(data,
                                  form,
                                  columns = NULL,
                                  show_system_columns = FALSE,
                                  selection = "single",
                                  options = list(),
                                  class = "display compact stripe hover nowrap",
                                  datetime_format = sft_default_datetime_format(),
                                  display_column_labels = NULL,
                                  filter = "none") {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  out <- sft_records_table_data(
    data = data,
    form = form,
    columns = columns,
    show_system_columns = show_system_columns,
    datetime_format = datetime_format,
    display_column_labels = display_column_labels
  )

  escape <- sft_table_escape(attr(out, "sft_markdown_positions"))

  options <- utils::modifyList(
    list(stateSave = TRUE),
    options
  )

  DT::datatable(
    out,
    rownames = FALSE,
    selection = selection,
    class = class,
    escape = escape,
    width = "100%",
    filter = filter,
    options = sft_dt_options(options)
  )
}

#' Create a DT table for audit log entries
#'
#' @param data Data frame returned by [fetch_audit_log()].
#' @param columns Optional character vector of columns to display.
#' @param selection DT row selection mode.
#' @param options Additional DT options.
#' @param datetime_format Format string for datetime columns, as used by
#'   [format()]. Defaults to the package datetime format
#'   (`"%d.%m.%Y %H:%M"`).
#'
#' @return A DT table widget.
#' @examples
#' audit <- data.frame(
#'   action = c("insert", "update"),
#'   version_no = c(1L, 2L),
#'   changed_at = Sys.time(),
#'   changed_by = "demo"
#' )
#' audit_datatable(audit)
#' @export
audit_datatable <- function(data,
                                columns = NULL,
                                selection = "none",
                                options = list(),
                                datetime_format = sft_default_datetime_format()) {
  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  if (is.null(columns)) {
    columns <- sft_default_audit_columns(data)
  } else {
    columns <- intersect(columns, names(data))
  }

  out <- data[, columns, drop = FALSE]
  out <- sft_format_display_data(out, datetime_format = datetime_format)

  if ("action" %in% names(out)) {
    out$action <- sft_format_action_value(out$action)
  }

  tl <- sft_table_labels()
  audit_labels <- c(
    log_id = as.character(tl$audit_log_id),
    record_id = as.character(tl$audit_record_id),
    record_uuid = as.character(tl$audit_record_uuid),
    action = as.character(tl$audit_action),
    version_no = as.character(tl$audit_version_no),
    changed_at = as.character(tl$audit_changed_at),
    changed_by = as.character(tl$audit_changed_by),
    changed_fields_json = as.character(tl$audit_changed_fields),
    reason = as.character(tl$audit_reason)
  )
  names(out) <- sft_relabel(names(out), audit_labels)

  DT::datatable(
    out,
    rownames = FALSE,
    selection = selection,
    options = sft_dt_options(options)
  )
}

sft_audit_snapshot_to_row <- function(json, columns) {
  out <- as.list(rep(NA_character_, length(columns)))
  names(out) <- columns

  if (is.null(json) || length(json) == 0L || is.na(json) || !nzchar(json)) {
    return(out)
  }

  snapshot <- tryCatch(
    sft_json_to_record(json),
    error = function(err) NULL
  )

  if (is.null(snapshot)) {
    return(out)
  }

  for (column in intersect(columns, names(snapshot))) {
    value <- snapshot[[column]]

    if (is.null(value) || length(value) == 0L) {
      out[[column]] <- NA_character_
    } else {
      out[[column]] <- paste(as.character(value), collapse = ", ")
    }
  }

  out
}

sft_versions_datatable <- function(data,
                                   form,
                                   selection = "single",
                                   options = list(),
                                   datetime_format = sft_default_datetime_format()) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(
      DT::datatable(
        data.frame(),
        rownames = FALSE,
        selection = "none",
        options = sft_dt_options(options)
      )
    )
  }

  field_columns <- vapply(
    sft_active_input_fields(form),
    function(field) field$db_column,
    character(1)
  )

  snapshot_rows <- lapply(
    data$new_data_json,
    sft_audit_snapshot_to_row,
    columns = field_columns
  )

  snapshot_data <- as.data.frame(
    do.call(rbind, lapply(snapshot_rows, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
  snapshot_data <- sft_format_field_display_columns(snapshot_data, form = form)

  meta <- data[, intersect(
    c("changed_by", "changed_at", "version_no", "action", "reason"),
    names(data)
  ), drop = FALSE]

  if ("changed_at" %in% names(meta)) {
    meta$changed_at <- sft_format_datetime_value(
      meta$changed_at,
      datetime_format = datetime_format
    )
  }

  if ("action" %in% names(meta)) {
    meta$action <- sft_format_action_value(meta$action)
  }

  out <- cbind(meta, snapshot_data)
  out <- sft_apply_column_labels(out, form)

  tl <- sft_table_labels()
  version_labels <- c(
    changed_by = as.character(tl$audit_changed_by),
    changed_at = as.character(tl$audit_changed_at),
    version_no = as.character(tl$audit_version_no),
    action = as.character(tl$audit_action),
    reason = as.character(tl$audit_reason)
  )
  names(out) <- sft_relabel(names(out), version_labels)

  # Markdown columns sit inside snapshot_data, after the metadata columns.
  markdown_positions <- ncol(meta) + which(field_columns %in% sft_markdown_columns(form))
  escape <- sft_table_escape(markdown_positions)

  options <- utils::modifyList(
    list(
      pageLength = 5,
      scrollX = TRUE,
      autoWidth = FALSE
    ),
    options
  )

  DT::datatable(
    out,
    rownames = FALSE,
    selection = selection,
    escape = escape,
    width = "100%",
    options = sft_dt_options(options)
  )
}
