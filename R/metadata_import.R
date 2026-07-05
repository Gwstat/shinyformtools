sft_metadata_pick_name <- function(data, candidates) {
  match <- candidates[candidates %in% names(data)]

  if (length(match) == 0L) {
    return(NULL)
  }

  match[[1L]]
}

sft_metadata_value <- function(row, candidates, default = NULL) {
  name <- sft_metadata_pick_name(row, candidates)

  if (is.null(name)) {
    return(default)
  }

  value <- row[[name]]

  if (length(value) == 0L) {
    return(default)
  }

  if (length(value) > 1L && !is.list(value)) {
    return(value)
  }

  if (!is.list(value) && length(value) == 1L && is.na(value)) {
    return(default)
  }

  if (is.list(value) && !is.data.frame(value) && length(value) == 1L) {
    return(value[[1L]])
  }

  value
}

sft_metadata_chr <- function(row, candidates, default = NULL) {
  value <- sft_metadata_value(row, candidates, default = default)

  if (is.null(value)) {
    return(default)
  }

  value <- as.character(value)

  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    return(default)
  }

  trimws(value)
}

sft_metadata_int <- function(row, candidates, default = 0L) {
  value <- sft_metadata_value(row, candidates, default = default)

  if (is.null(value)) {
    return(default)
  }

  value <- suppressWarnings(as.integer(value))

  if (length(value) != 1L || is.na(value)) {
    return(default)
  }

  value
}


sft_coerce_lgl <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(default)
  }

  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }

  if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
    return(value != 0)
  }

  value <- tolower(trimws(as.character(value)))

  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(default)
  }

  if (value %in% c("true", "t", "1", "yes", "ja", "j", "x")) {
    return(TRUE)
  }

  if (value %in% c("false", "f", "0", "no", "nein", "n")) {
    return(FALSE)
  }

  default
}

sft_metadata_lgl <- function(row, candidates, default = FALSE) {
  value <- sft_metadata_value(row, candidates, default = default)

  if (is.null(value)) {
    return(default)
  }

  sft_coerce_lgl(value, default = default)
}

sft_simplify_json_value <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    return(value)
  }

  if (length(value) == 0L) {
    return(value)
  }

  value <- lapply(value, sft_simplify_json_value)
  value_names <- names(value)

  if (is.null(value_names) || all(!nzchar(value_names))) {
    is_scalar_atomic <- vapply(
      value,
      function(item) is.atomic(item) && length(item) == 1L,
      logical(1)
    )

    if (length(is_scalar_atomic) > 0L && all(is_scalar_atomic)) {
      return(unlist(value, use.names = FALSE))
    }
  }

  names(value) <- value_names
  value
}

sft_metadata_parse_json <- function(value, what) {
  if (is.null(value)) {
    return(list())
  }

  if (is.list(value) && !is.data.frame(value)) {
    return(value)
  }

  if (!is.character(value) || length(value) != 1L || !nzchar(trimws(value))) {
    return(list())
  }

  value <- trimws(value)

  if (!startsWith(value, "{") && !startsWith(value, "[")) {
    stop(
      what,
      " must be a JSON object/list or a list-column. ",
      "Legacy R argument strings are intentionally not evaluated.",
      call. = FALSE
    )
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(value, simplifyVector = FALSE),
    error = function(err) {
      stop(what, " contains invalid JSON: ", conditionMessage(err), call. = FALSE)
    }
  )

  if (is.null(parsed)) {
    return(list())
  }

  parsed <- sft_simplify_json_value(parsed)

  parsed
}

sft_metadata_parse_vector <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }

  if (is.list(value) && !is.data.frame(value)) {
    return(unlist(value, use.names = TRUE))
  }

  if (length(value) == 0L || all(is.na(value))) {
    return(NULL)
  }

  if (!is.character(value)) {
    return(value)
  }

  value <- trimws(value)

  if (length(value) == 1L && (!nzchar(value) || is.na(value))) {
    return(NULL)
  }

  if (length(value) == 1L && (startsWith(value, "[") || startsWith(value, "{"))) {
    return(unlist(sft_metadata_parse_json(value, "choices"), use.names = TRUE))
  }

  if (length(value) == 1L && grepl(";", value, fixed = TRUE)) {
    return(trimws(strsplit(value, ";", fixed = TRUE)[[1L]]))
  }

  value
}

sft_metadata_parse_scalar <- function(value) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    return(NULL)
  }

  if (is.logical(value) || is.numeric(value)) {
    return(value[[1L]])
  }

  value_chr <- trimws(as.character(value[[1L]]))

  if (!nzchar(value_chr) || is.na(value_chr)) {
    return(NULL)
  }

  if (tolower(value_chr) %in% c("true", "false")) {
    return(tolower(value_chr) == "true")
  }

  value_num <- suppressWarnings(as.numeric(value_chr))

  if (!is.na(value_num) && grepl("^-?[0-9]+(\\.[0-9]+)?$", value_chr)) {
    return(value_num)
  }

  value_chr
}

sft_metadata_field_args <- function(row) {
  args_value <- sft_metadata_value(row, c("args_json", "args"), default = NULL)
  args <- sft_metadata_parse_json(args_value, "args")

  if (length(args) > 0L && (!is.list(args) || is.null(names(args)))) {
    stop("args must decode to a JSON object/list.", call. = FALSE)
  }

  choices <- sft_metadata_value(row, c("choices", "Choices", "auswahl", "Auswahlwerte"), default = NULL)
  choices <- sft_metadata_parse_vector(choices)

  if (!is.null(choices) && is.null(args$choices)) {
    args$choices <- choices
  }

  for (name in c("selected", "value", "min", "max", "step", "placeholder", "width", "multiple", "timeFormat")) {
    value <- sft_metadata_value(row, name, default = NULL)
    value <- sft_metadata_parse_scalar(value)

    if (!is.null(value) && is.null(args[[name]])) {
      args[[name]] <- value
    }
  }

  args
}

sft_metadata_input_type <- function(row) {
  input_type <- sft_metadata_chr(
    row,
    c("input_type", "inputObj", "input", "Input", "input_type_old"),
    default = "textInput"
  )

  aliases <- c(
    text = "textInput",
    password = "passwordInput",
    textarea = "textAreaInput",
    textArea = "textAreaInput",
    numeric = "numericInput",
    select = "selectInput",
    selectize = "selectizeInput",
    slider = "sliderInput",
    date = "dateInput",
    dateRange = "dateRangeInput",
    checkbox = "checkboxInput",
    checkboxGroup = "checkboxGroupInput",
    radio = "radioButtons",
    multi = "multiInput",
    time = "timeInput"
  )

  if (input_type %in% names(aliases)) {
    input_type <- unname(aliases[[input_type]])
  }

  input_type
}

sft_make_identifier <- function(value, fallback = "form") {
  value <- as.character(value %||% fallback)
  value <- value[[1L]]
  value <- iconv(value, from = "", to = "ASCII//TRANSLIT")
  value <- gsub("[^A-Za-z0-9_]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^_+|_+$", "", value)

  if (!nzchar(value)) {
    value <- fallback
  }

  if (!grepl("^[A-Za-z]", value)) {
    value <- paste0("form_", value)
  }

  if (startsWith(value, sft_reserved_prefix)) {
    value <- paste0("form_", value)
  }

  value
}

sft_metadata_filter_inputs <- function(inputs, questionnaire = NULL) {
  if (!is.data.frame(inputs)) {
    stop("inputs must be a data frame.", call. = FALSE)
  }

  data <- inputs

  if (!is.null(questionnaire)) {
    form_column <- sft_metadata_pick_name(data, c("Fragebogen", "questionnaire", "questionaire", "form_id"))

    if (!is.null(form_column)) {
      keep <- as.character(data[[form_column]]) == as.character(questionnaire)
      keep[is.na(keep)] <- FALSE
      data <- data[keep, , drop = FALSE]
    }
  }

  selection_column <- sft_metadata_pick_name(data, c("Auswahl", "selected", "include", "active"))

  if (!is.null(selection_column)) {
    keep <- vapply(
      data[[selection_column]],
      sft_coerce_lgl,
      logical(1),
      default = TRUE
    )
    data <- data[keep, , drop = FALSE]
  }

  data
}

sft_metadata_settings_row <- function(settings, questionnaire = NULL) {
  if (is.null(settings)) {
    return(NULL)
  }

  if (!is.data.frame(settings)) {
    stop("settings must be NULL or a data frame.", call. = FALSE)
  }

  data <- settings

  if (!is.null(questionnaire)) {
    form_column <- sft_metadata_pick_name(data, c("questionnaire", "questionaire", "Fragebogen", "form_id"))

    if (!is.null(form_column)) {
      keep <- as.character(data[[form_column]]) == as.character(questionnaire)
      keep[is.na(keep)] <- FALSE
      data <- data[keep, , drop = FALSE]
    }
  }

  if (nrow(data) == 0L) {
    return(NULL)
  }

  data[1L, , drop = FALSE]
}

sft_field_from_metadata_row <- function(row) {
  id <- sft_metadata_chr(row, c("id", "Id", "ID"), default = NULL)

  if (is.null(id)) {
    stop("metadata inputs must contain a non-empty id/Id/ID column.", call. = FALSE)
  }

  type <- sft_metadata_chr(row, c("type", "field_type"), default = "input")
  label <- sft_metadata_chr(row, c("label", "Label", "name", "Name"), default = id)
  show <- sft_metadata_lgl(row, c("show", "visible", "Auswahl"), default = TRUE)
  tab <- sft_metadata_int(row, c("tab", "Tab"), default = 0L)
  slide <- sft_metadata_int(row, c("slide", "Slide"), default = 0L)
  col <- sft_metadata_int(row, c("col", "Col", "column"), default = 0L)
  pos <- sft_metadata_int(row, c("pos", "Pos", "position", "Nr"), default = 0L)
  tab_label <- sft_metadata_chr(row, c("tab_label", "tab_name", "Tabname"), default = NULL)
  slide_label <- sft_metadata_chr(row, c("slide_label", "slide_name", "Slidename"), default = NULL)

  if (identical(type, "html")) {
    html <- sft_metadata_chr(row, c("html", "content", "HTML"), default = "")

    return(
      html_field(
        id = id,
        html = html,
        label = label,
        tab = tab,
        tab_label = tab_label,
        slide = slide,
        slide_label = slide_label,
        col = col,
        pos = pos,
        show = show
      )
    )
  }

  if (type %in% c("text_output", "plot_output", "ui_output")) {
    output_type <- switch(
      type,
      text_output = "text",
      plot_output = "plot",
      ui_output = "ui"
    )

    return(
      output_field(
        id = id,
        output_type = output_type,
        label = label,
        args = sft_metadata_field_args(row),
        tab = tab,
        tab_label = tab_label,
        slide = slide,
        slide_label = slide_label,
        col = col,
        pos = pos,
        show = show
      )
    )
  }

  disabled <- sft_metadata_lgl(row, c("disabled", "Disabled"), default = FALSE)

  form_field(
    id = id,
    label = label,
    type = "input",
    input_type = sft_metadata_input_type(row),
    args = sft_metadata_field_args(row),
    db_column = sft_metadata_chr(row, c("db_column", "db_col", "column_name"), default = NULL),
    db_type = sft_metadata_chr(row, c("db_type", "sql_type"), default = NULL),
    mandatory = sft_metadata_lgl(row, c("mandatory", "required", "pflicht", "Pflicht"), default = FALSE),
    unique = sft_metadata_lgl(row, c("unique", "toggleunique"), default = FALSE),
    editable = !disabled && sft_metadata_lgl(row, c("editable", "bearbeitbar"), default = TRUE),
    show = show,
    tab = tab,
    tab_label = tab_label,
    slide = slide,
    slide_label = slide_label,
    col = col,
    pos = pos,
    status = sft_metadata_chr(row, c("status"), default = "active"),
    renamed_from = sft_metadata_chr(row, c("renamed_from", "renamedFrom", "old_id"), default = NULL)
  )
}

#' Create a form schema from metadata tables
#'
#' Converts questionnaire metadata to a `shinyformtools` form schema. This is a
#' safe replacement path for legacy generators that created Shiny code with
#' `eval(parse())`.
#'
#' @param inputs Data frame describing fields. Supported columns include `Id`,
#'   `label`, `inputObj`/`input_type`, `args_json`, `mandatory`,
#'   `toggleunique`/`unique`, `renamed_from`, `db_column`, `db_type`, `slide`,
#'   `tab`, `col` and `pos`.
#' @param settings Optional one-row or multi-row settings data frame. If a
#'   questionnaire column is present, it is filtered by `questionnaire`.
#' @param questionnaire Optional questionnaire/form name used to filter legacy
#'   metadata columns such as `Fragebogen` or `questionaire`.
#' @param form_id Optional form id. Defaults to a safe identifier derived from
#'   `questionnaire`.
#' @param table_name Optional database table name. Defaults to `form_id`.
#' @param db_path SQLite path used when `db` is omitted.
#' @param db Database backend created with [db_sqlite()] or
#'   [db_mariadb()].
#' @param form_name User-facing form name.
#' @param version Schema version.
#' @param schema_policy Schema handling policy passed to [form()].
#' @param on_edit_missing_required Handling of new mandatory fields when old
#'   records are edited.
#' @param messages Optional validation messages.
#' @param header Optional form header.
#' @param footer Optional form footer.
#' @param server Optional Shiny server hook.
#'
#' @return A form definition object of class `form`.
#' @examples
#' # Describe the fields as a metadata data frame, then derive a form from it.
#' inputs <- data.frame(
#'   id = c("name", "age"),
#'   label = c("Name", "Age"),
#'   input_type = c("text", "numeric"),
#'   mandatory = c(TRUE, FALSE),
#'   stringsAsFactors = FALSE
#' )
#' frm <- form_from_metadata(
#'   inputs,
#'   form_id = "people",
#'   db = db_sqlite(tempfile(fileext = ".sqlite"))
#' )
#' frm$form_id
#' vapply(frm$fields, function(f) f$id, character(1))
#' @export
form_from_metadata <- function(inputs,
                                   settings = NULL,
                                   questionnaire = NULL,
                                   form_id = NULL,
                                   table_name = NULL,
                                   db_path = "form_data.sqlite",
                                   db = NULL,
                                   form_name = NULL,
                                   version = 1L,
                                   schema_policy = c("safe", "manual"),
                                   on_edit_missing_required = c("warn", "require", "ignore"),
                                   messages = list(),
                                   header = NULL,
                                   footer = NULL,
                                   server = NULL) {
  schema_policy <- match.arg(schema_policy)
  on_edit_missing_required <- match.arg(on_edit_missing_required)

  settings_row <- sft_metadata_settings_row(settings, questionnaire = questionnaire)
  inputs <- sft_metadata_filter_inputs(inputs, questionnaire = questionnaire)

  if (nrow(inputs) == 0L) {
    stop("No metadata input rows remain after filtering.", call. = FALSE)
  }

  form_id <- form_id %||%
    sft_metadata_chr(settings_row, c("form_id", "id"), default = NULL) %||%
    sft_make_identifier(questionnaire %||% "form")

  table_name <- table_name %||%
    sft_metadata_chr(settings_row, c("table_name", "table", "db_table"), default = NULL) %||%
    form_id

  form_name <- form_name %||%
    sft_metadata_chr(settings_row, c("form_name", "savename", "title"), default = NULL) %||%
    as.character(questionnaire %||% form_id)

  version <- sft_metadata_int(settings_row, c("version"), default = version)

  fields <- lapply(
    seq_len(nrow(inputs)),
    function(i) {
      sft_field_from_metadata_row(inputs[i, , drop = FALSE])
    }
  )

  form(
    form_id = form_id,
    fields = fields,
    table_name = table_name,
    db_path = db_path,
    db = db,
    form_name = form_name,
    version = version,
    schema_policy = schema_policy,
    on_edit_missing_required = on_edit_missing_required,
    messages = messages,
    header = header,
    footer = footer,
    server = server
  )
}

#' Create a form schema from an Excel metadata workbook
#'
#' Reads legacy-style metadata sheets and converts them to a declarative
#' `shinyformtools` form schema.
#'
#' @param filename Path to the Excel workbook.
#' @param questionnaire Optional questionnaire/form name used to filter metadata.
#' @param inputs_sheet Sheet containing input metadata.
#' @param settings_sheet Sheet containing questionnaire settings.
#' @param ... Passed to [form_from_metadata()].
#'
#' @return A form definition object of class `form`.
#' @examples
#' # Reads metadata sheets from an .xlsx workbook (requires the openxlsx package
#' # and an existing file), then builds a form via form_from_metadata().
#' \dontrun{
#' frm <- form_from_excel(
#'   "questionnaire.xlsx",
#'   questionnaire = "survey1",
#'   db = db_sqlite("survey.sqlite")
#' )
#' }
#' @export
form_from_excel <- function(filename,
                                questionnaire = NULL,
                                inputs_sheet = "Inputs",
                                settings_sheet = "Einstellungen",
                                ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Reading Excel metadata requires the openxlsx package. ",
      "Install it with install.packages('openxlsx').",
      call. = FALSE
    )
  }

  sheets <- openxlsx::getSheetNames(filename)

  if (!inputs_sheet %in% sheets) {
    stop("Sheet not found: ", inputs_sheet, call. = FALSE)
  }

  inputs <- openxlsx::read.xlsx(filename, sheet = inputs_sheet)

  settings <- NULL

  if (settings_sheet %in% sheets) {
    settings <- openxlsx::read.xlsx(filename, sheet = settings_sheet)
  }

  form_from_metadata(
    inputs = inputs,
    settings = settings,
    questionnaire = questionnaire,
    ...
  )
}
