# Export of form records to CSV and Excel: export_records() for scripts, and
# the download buttons of the form module (sft_register_export).

sft_export_formats <- function() {
  c("csv", "csv2", "xlsx")
}

sft_export_extension <- function(format) {
  if (identical(format, "xlsx")) "xlsx" else "csv"
}

# The file format: named explicitly, or taken from the file extension.
sft_resolve_export_format <- function(file, format = NULL) {
  if (is.null(format)) {
    extension <- tolower(tools::file_ext(file))

    if (!extension %in% c("csv", "xlsx")) {
      stop(
        "Cannot tell the export format from '", basename(file), "'. ",
        "Use a .csv or .xlsx file name, or pass format = \"csv\", \"csv2\" or \"xlsx\".",
        call. = FALSE
      )
    }

    return(extension)
  }

  if (!sft_is_scalar_character(format) || !format %in% sft_export_formats()) {
    stop(
      "format must be one of ",
      paste0("\"", sft_export_formats(), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  format
}

# One field's column as it should appear in an export. The starting point is
# the value the records table shows (choice lists joined, IBANs grouped, a
# registered input's own `format`), WITHOUT Markdown rendering - an export
# carries the source text. Two things stay typed so that a spreadsheet can
# calculate with them: numbers stay numbers, and a checkbox becomes TRUE / FALSE.
sft_export_field_column <- function(field, values) {
  decoded_logical <- vapply(
    values,
    function(value) is.logical(sft_ui_value(field, value)),
    logical(1)
  )

  if (length(values) > 0L && all(decoded_logical | is.na(values)) && any(decoded_logical)) {
    return(vapply(
      values,
      function(value) {
        decoded <- sft_ui_value(field, value)
        if (is.null(decoded)) NA else isTRUE(decoded)
      },
      logical(1)
    ))
  }

  formatted <- lapply(
    values,
    function(value) {
      out <- sft_format_field_display_value(field = field, value = value)

      if (is.null(out) || length(out) == 0L) NA else out[1L]
    }
  )

  if (is.numeric(values) && all(vapply(formatted, is.numeric, logical(1)))) {
    return(values)
  }

  vapply(
    formatted,
    function(value) if (is.na(value)) NA_character_ else as.character(value),
    character(1)
  )
}

# Records as they go into an export file: the columns the records table would
# show (same resolution, so shape columns and hidden system columns stay out),
# values prepared per field, timestamps in the display format, and the field
# labels as column names unless `labels = FALSE` asks for the database names.
sft_export_data <- function(data,
                            form,
                            columns = NULL,
                            show_system_columns = FALSE,
                            labels = TRUE,
                            display_column_labels = NULL,
                            datetime_format = sft_default_datetime_format()) {
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
  rownames(out) <- NULL

  if (nrow(out) > 0L) {
    for (field in sft_active_input_fields(form)) {
      column <- field$db_column

      if (column %in% names(out)) {
        out[[column]] <- unname(sft_export_field_column(field, out[[column]]))
      }
    }

    out <- sft_format_display_data(out, datetime_format = datetime_format)
  }

  if (isTRUE(labels)) {
    out <- sft_apply_column_labels(
      data = out,
      form = form,
      display_column_labels = display_column_labels
    )
  }

  out
}

# Write prepared export data. CSV files start with a UTF-8 byte order mark:
# without it Excel reads the file in the local code page and turns every umlaut
# into two wrong characters. "csv2" is the semicolon / decimal-comma dialect
# that Excel expects on a German (and most European) systems.
sft_write_export <- function(data, file, format) {
  if (identical(format, "xlsx")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "Excel export requires the openxlsx package. ",
        "Install it with install.packages('openxlsx').",
        call. = FALSE
      )
    }

    openxlsx::write.xlsx(data, file = file, overwrite = TRUE)
    return(invisible(file))
  }

  text_columns <- vapply(data, is.character, logical(1))
  data[text_columns] <- lapply(data[text_columns], enc2utf8)
  names(data) <- enc2utf8(names(data))

  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)

  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  utils::write.table(
    data,
    file = con,
    sep = if (identical(format, "csv2")) ";" else ",",
    dec = if (identical(format, "csv2")) "," else ".",
    qmethod = "double",
    row.names = FALSE,
    na = "",
    eol = "\r\n"
  )

  invisible(file)
}

#' Export form records to CSV or Excel
#'
#' Writes the records of a form to a file that opens cleanly in a spreadsheet:
#' field labels as column headers, multi-value fields as readable text instead
#' of JSON arrays, numbers as numbers, checkboxes as `TRUE` / `FALSE`, and
#' timestamps in local time. Shape columns and the internal system columns are
#' left out, like in the records table.
#'
#' CSV files are written as UTF-8 with a byte order mark, which is what Excel
#' needs to read umlauts and other non-ASCII text correctly. Use
#' `format = "csv2"` for the semicolon-separated dialect with a decimal comma
#' that Excel expects on German and most other European systems.
#'
#' @param form Object created with [form()].
#' @param file Path of the file to write. `NULL` writes nothing and only
#'   returns the prepared data, for example to hand it to another writer.
#' @param conn Optional DBI connection; see [connections].
#' @param format `"csv"`, `"csv2"` or `"xlsx"`. The default `NULL` takes it
#'   from the extension of `file`. `"xlsx"` needs the \pkg{openxlsx} package.
#' @param include_deleted Logical. Whether soft-deleted records are included.
#'   `"only"` exports just the soft-deleted records.
#' @param columns Optional character vector of columns (database names) to
#'   export, in this order. The default is what the records table shows.
#' @param labels Logical. `TRUE` uses the field labels as column headers,
#'   `FALSE` the database column names, which is the better choice when the
#'   file is read by a program again.
#' @param system_columns Logical. Whether the extended `sft_` columns (created
#'   and updated by / at, deletion state) are exported too.
#' @param datetime_format Format for timestamps, as in [records_datatable()].
#'
#' @return The exported data frame, invisibly.
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts", db = db,
#'   fields = list(
#'     form_field(id = "name", label = "Name"),
#'     form_field(id = "age", label = "Age", input_type = "numericInput")
#'   )
#' )
#' insert_record(contacts, list(name = "Ada", age = 36))
#'
#' target <- tempfile(fileext = ".csv")
#' export_records(contacts, target)
#' readLines(target)
#' @export
export_records <- function(form,
                           file = NULL,
                           conn = NULL,
                           format = NULL,
                           include_deleted = FALSE,
                           columns = NULL,
                           labels = TRUE,
                           system_columns = FALSE,
                           datetime_format = sft_default_datetime_format()) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!is.null(file)) {
    if (!sft_is_scalar_character(file)) {
      stop("file must be NULL or a single file path.", call. = FALSE)
    }

    format <- sft_resolve_export_format(file, format)
  }

  data <- sft_export_data(
    data = fetch_records(form, conn = conn, include_deleted = include_deleted),
    form = form,
    columns = columns,
    show_system_columns = system_columns,
    labels = labels,
    datetime_format = datetime_format
  )

  if (!is.null(file)) {
    sft_write_export(data, file = file, format = format)
  }

  invisible(data)
}

# Which download buttons form_ui() / form_buttons() draw for `show_export`:
# FALSE none, TRUE every format this installation can write, or a character
# vector naming them.
sft_resolve_show_export <- function(show_export) {
  if (is.null(show_export) || identical(show_export, FALSE)) {
    return(character())
  }

  if (isTRUE(show_export)) {
    formats <- c("csv", "xlsx")

    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      formats <- "csv"
    }

    return(formats)
  }

  if (!is.character(show_export) || !all(show_export %in% sft_export_formats())) {
    stop(
      "show_export must be TRUE, FALSE or a subset of ",
      paste0("\"", sft_export_formats(), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  unique(show_export)
}

sft_export_button_id <- function(format) {
  paste0("export_", format)
}

sft_export_buttons <- function(ns, labels, show_export, button_options) {
  lapply(
    sft_resolve_show_export(show_export),
    function(format) {
      input_id <- sft_export_button_id(format)
      label <- sft_ui_label(labels, input_id)

      if (is.null(label)) {
        return(NULL)
      }

      shiny::downloadButton(
        outputId = ns(input_id),
        label = label,
        class = sft_button_class(input_id, button_options)
      )
    }
  )
}

# Download handlers behind the export buttons. The file holds what the records
# table holds: the same records (after display_transform), the same visible
# columns and labels, and - when the user has typed into the table's search
# box - the same filtered rows. A handler is always registered; Shiny runs it
# only when form_ui() drew the button. Registrar.
sft_register_export <- function(input, output, session, state) {
  form <- state$form

  export_data <- function() {
    if (!state$permission("can_export") || !state$permission("can_view_table")) {
      stop(sft_ui_label(state$labels, "export_not_allowed"), call. = FALSE)
    }

    data <- state$display_records()
    visible_rows <- input$records_rows_all

    if (is.data.frame(data) && !is.null(visible_rows) &&
        all(visible_rows %in% seq_len(nrow(data)))) {
      data <- data[visible_rows, , drop = FALSE]
    }

    sft_export_data(
      data = data,
      form = form,
      columns = state$current_record_columns(),
      show_system_columns = state$columns$show_system,
      display_column_labels = state$columns$labels,
      datetime_format = state$table$datetime_format
    )
  }

  for (fmt in sft_export_formats()) {
    local({
      export_format <- fmt

      output[[sft_export_button_id(export_format)]] <- shiny::downloadHandler(
        filename = function() {
          paste0(
            form$table_name, "_", format(Sys.Date(), "%Y-%m-%d"), ".",
            sft_export_extension(export_format)
          )
        },
        content = function(file) {
          sft_with_language(
            state$language,
            sft_write_export(export_data(), file = file, format = export_format)
          )
        }
      )
    })
  }

  invisible(list())
}
