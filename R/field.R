sft_supported_field_types <- function() {
  c(
    "input",
    "shape",
    "html",
    "text_output",
    "plot_output",
    "ui_output"
  )
}

sft_is_input_field <- function(field) {
  is.list(field) && identical(field$type, "input")
}

sft_is_shape_field <- function(field) {
  is.list(field) && identical(field$type, "shape")
}

# Fields that map to a stored database column. Input fields hold editable data;
# shape fields hold a fixed, non-editable geometry attached out-of-band (see
# attach_shapes). Everything editing-related keys off sft_is_input_field, so
# a shape column is part of the schema and is fetched with the record, but is
# never collected from a form input, validated, or written by insert/update.
sft_is_stored_field <- function(field) {
  sft_is_input_field(field) || sft_is_shape_field(field)
}

sft_supported_shape_encodings <- function() {
  c("geojson", "wkt")
}

sft_default_db_type <- function(input_type) {
  if (input_type %in% c("numericInput", "sliderInput")) {
    return("REAL")
  }

  if (identical(input_type, "checkboxInput")) {
    return("INTEGER")
  }

  "TEXT"
}

sft_check_optional_label <- function(value, name) {
  if (is.null(value)) {
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be NULL or a character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Define a form field
#'
#' Creates a field definition for a shinyformtools form.
#'
#' @param id Stable field identifier.
#' @param label User-facing field label.
#' @param type Field type. One of `"input"`, `"html"`, `"text_output"`,
#'   `"plot_output"` or `"ui_output"`.
#' @param input_type Shiny input function name for input fields.
#' @param args List of arguments passed to the input/output function.
#' @param db_column Database column name. Defaults to `renamed_from` when
#'   supplied, otherwise to `id`.
#' @param db_type Database column type. If `NULL`, a conservative default
#'   is derived from `input_type`.
#' @param db_default Optional database default value.
#' @param mandatory Logical. Whether the field is required.
#' @param unique Logical. Whether values in this field must be unique.
#' @param editable Logical, or a function of the current user
#'   (`function(user)`) returning a logical. When `FALSE` (or the function
#'   returns `FALSE` for the current user) the field is shown disabled and is
#'   dropped server-side on both add and edit, so it cannot be written. A
#'   function is resolved per user at render and save time; it must be on an
#'   input field. Errors in the function are treated as not editable
#'   (fail-closed).
#' @param markdown Logical. When `TRUE`, the field's stored value is rendered as
#'   Markdown in the records and versions tables instead of being shown as plain
#'   text. The value is HTML-escaped before rendering and the output is
#'   URL-sanitized so that user-entered content is not rendered as active scripts
#'   or unsafe links. Editing still shows the raw Markdown source. Only input
#'   fields may be markdown. Requires the \pkg{commonmark} package.
#' @param show Logical. Whether the field is shown in the form.
#' @param tab Integer-like layout tab position.
#' @param tab_label Optional label for the tab this field belongs to.
#' @param slide Integer-like layout slide position.
#' @param slide_label Optional label for the slide this field belongs to.
#' @param col Integer-like layout column position.
#' @param pos Integer-like position inside tab/slide/column.
#' @param status Field lifecycle status. One of `"active"`, `"retired"` or
#'   `"orphaned"`.
#' @param renamed_from Optional previous field id if this field is an explicit
#'   rename. If `db_column` is omitted, the previous id is reused as database
#'   column so existing values remain available.
#'
#' @return A field definition object of class `"sft_field"`.
#' @examples
#' # A plain text input field.
#' name <- form_field(id = "name", label = "Name", mandatory = TRUE)
#'
#' # A select input with a unique constraint.
#' team <- form_field(
#'   id = "team", label = "Team", input_type = "selectInput",
#'   args = list(choices = c("Sales", "Support")), unique = TRUE
#' )
#' name$db_column
#' @export
form_field <- function(id,
                      label = NULL,
                      type = "input",
                      input_type = NULL,
                      args = list(),
                      db_column = NULL,
                      db_type = NULL,
                      db_default = NULL,
                      mandatory = FALSE,
                      unique = FALSE,
                      editable = TRUE,
                      markdown = FALSE,
                      show = TRUE,
                      tab = 0L,
                      tab_label = NULL,
                      slide = 0L,
                      slide_label = NULL,
                      col = 0L,
                      pos = 0L,
                      status = "active",
                      renamed_from = NULL) {
  type <- match.arg(type, sft_supported_field_types())

  if (is.null(input_type)) {
    input_type <- if (identical(type, "input")) {
      "textInput"
    } else {
      type
    }
  }

  db_column <- db_column %||% renamed_from %||% id
  db_type <- db_type %||% sft_default_db_type(input_type)

  field <- list(
    id = id,
    label = label %||% id,
    type = type,
    input_type = input_type,
    args = args,
    db_column = db_column,
    db_type = toupper(db_type),
    db_default = db_default,
    mandatory = mandatory,
    unique = unique,
    editable = editable,
    markdown = markdown,
    show = show,
    tab = as.integer(tab),
    tab_label = tab_label,
    slide = as.integer(slide),
    slide_label = slide_label,
    col = as.integer(col),
    pos = as.integer(pos),
    status = status,
    renamed_from = renamed_from
  )

  sft_validate_field(field)

  structure(field, class = c("sft_field", "list"))
}

#' Define a static HTML form element
#'
#' @param id Stable field identifier.
#' @param html HTML string.
#' @param label Optional label.
#' @param tab Integer-like layout tab position.
#' @param tab_label Optional label for the tab this field belongs to.
#' @param slide Integer-like layout slide position.
#' @param slide_label Optional label for the slide this field belongs to.
#' @param col Integer-like layout column position.
#' @param pos Integer-like position inside tab/slide/column.
#' @param show Logical. Whether the element is shown.
#'
#' @return A field definition object of class `"sft_field"`.
#' @examples
#' note <- html_field(
#'   id = "note",
#'   html = "<p>Please fill in all required fields.</p>"
#' )
#' note$type
#' @export
html_field <- function(id,
                           html,
                           label = "",
                           tab = 0L,
                           tab_label = NULL,
                           slide = 0L,
                           slide_label = NULL,
                           col = 0L,
                           pos = 0L,
                           show = TRUE) {
  form_field(
    id = id,
    label = label,
    type = "html",
    input_type = "html",
    args = list(content = html),
    mandatory = FALSE,
    unique = FALSE,
    editable = FALSE,
    show = show,
    tab = tab,
    tab_label = tab_label,
    slide = slide,
    slide_label = slide_label,
    col = col,
    pos = pos
  )
}

#' Define a fixed-geometry (shape) field
#'
#' Declares a non-editable column that stores a geometry per record, for example
#' the boundary of an election district. The geometry is loaded out of band with
#' [attach_shapes()] and is never collected from a form input, validated, or
#' written by insert/update, so editing a record's other (input) fields never
#' disturbs its shape. Stored as serialized text (GeoJSON or WKT) in a `TEXT`
#' column, identical across all backends.
#'
#' @param id Stable field identifier and, by default, the database column name.
#' @param label User-facing label.
#' @param crs Coordinate reference system the geometry is reprojected to on
#'   attach, as anything [sf::st_crs()] accepts (default EPSG:4326, which GeoJSON
#'   requires).
#' @param encoding Serialization for the stored text. One of `"geojson"`
#'   (default) or `"wkt"`.
#' @param db_column Database column name. Defaults to `id`.
#' @param show Logical. Whether the column is offered in the records table.
#'   Defaults to `FALSE`, since a serialized geometry is not a useful table cell.
#' @param tab,tab_label,slide,slide_label,col,pos Layout position, as in
#'   [form_field()].
#'
#' @return A field definition object of class `"sft_field"`.
#' @examples
#' # Declare a geometry column; the geometry itself is loaded later with
#' # attach_shapes(). Building the field does not require the sf package.
#' boundary <- shape_field(id = "boundary", label = "District boundary")
#' boundary$encoding
#' @export
shape_field <- function(id,
                            label = "",
                            crs = 4326,
                            encoding = c("geojson", "wkt"),
                            db_column = NULL,
                            show = FALSE,
                            tab = 0L,
                            tab_label = NULL,
                            slide = 0L,
                            slide_label = NULL,
                            col = 0L,
                            pos = 0L) {
  encoding <- match.arg(encoding)

  field <- form_field(
    id = id,
    label = label,
    type = "shape",
    input_type = "shape",
    db_column = db_column,
    db_type = "TEXT",
    mandatory = FALSE,
    unique = FALSE,
    editable = FALSE,
    show = show,
    tab = tab,
    tab_label = tab_label,
    slide = slide,
    slide_label = slide_label,
    col = col,
    pos = pos
  )

  field$crs <- crs
  field$encoding <- encoding

  field
}

#' Define a reactive output form element
#'
#' @param id Stable output identifier.
#' @param output_type Output type. One of `"text"`, `"plot"` or `"ui"`.
#' @param label Optional label shown above the output.
#' @param args List of arguments passed to the output function.
#' @param tab Integer-like layout tab position.
#' @param tab_label Optional label for the tab this field belongs to.
#' @param slide Integer-like layout slide position.
#' @param slide_label Optional label for the slide this field belongs to.
#' @param col Integer-like layout column position.
#' @param pos Integer-like position inside tab/slide/column.
#' @param show Logical. Whether the element is shown.
#'
#' @return A field definition object of class `"sft_field"`.
#' @examples
#' # A reactive text output rendered inside the form.
#' summary_out <- output_field(id = "summary", output_type = "text")
#' summary_out$type
#' @export
output_field <- function(id,
                             output_type = c("text", "plot", "ui"),
                             label = "",
                             args = list(),
                             tab = 0L,
                             tab_label = NULL,
                             slide = 0L,
                             slide_label = NULL,
                             col = 0L,
                             pos = 0L,
                             show = TRUE) {
  output_type <- match.arg(output_type)

  type <- switch(
    output_type,
    text = "text_output",
    plot = "plot_output",
    ui = "ui_output"
  )

  form_field(
    id = id,
    label = label,
    type = type,
    input_type = type,
    args = args,
    mandatory = FALSE,
    unique = FALSE,
    editable = FALSE,
    show = show,
    tab = tab,
    tab_label = tab_label,
    slide = slide,
    slide_label = slide_label,
    col = col,
    pos = pos
  )
}

sft_validate_field <- function(field) {
  sft_check_identifier(field$id, "field id")
  sft_check_identifier(field$db_column, "database column")

  if (!field$type %in% sft_supported_field_types()) {
    stop(
      "type must be one of: ",
      paste(sft_supported_field_types(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.character(field$label) || length(field$label) != 1L || is.na(field$label)) {
    stop("field label must be a character scalar.", call. = FALSE)
  }

  if (sft_is_input_field(field) && !nzchar(trimws(field$label))) {
    stop("input field label must be a non-empty character scalar.", call. = FALSE)
  }

  if (!sft_is_scalar_character(field$input_type)) {
    stop("input_type must be a non-empty character scalar.", call. = FALSE)
  }

  if (sft_is_input_field(field)) {
    sft_validate_input_type(field$input_type)
  }

  if (!is.list(field$args)) {
    stop("args must be a list.", call. = FALSE)
  }

  if (!sft_is_scalar_character(field$db_type)) {
    stop("db_type must be a non-empty character scalar.", call. = FALSE)
  }

  for (flag in c("mandatory", "unique", "markdown", "show")) {
    if (!sft_is_scalar_logical(field[[flag]])) {
      stop(flag, " must be TRUE or FALSE.", call. = FALSE)
    }
  }

  # editable may also be a function of the current user, resolved at render and
  # save time (see sft_resolve_editable).
  if (!sft_is_scalar_logical(field$editable) && !is.function(field$editable)) {
    stop("editable must be TRUE, FALSE, or a function of the user.", call. = FALSE)
  }

  if (!sft_is_input_field(field)) {
    if (isTRUE(field$mandatory)) {
      stop("Only input fields can be mandatory.", call. = FALSE)
    }

    if (isTRUE(field$unique)) {
      stop("Only input fields can be unique.", call. = FALSE)
    }

    if (isTRUE(field$markdown)) {
      stop("Only input fields can be markdown.", call. = FALSE)
    }

    if (is.function(field$editable)) {
      stop("Only input fields can have a function for editable.", call. = FALSE)
    }
  }

  for (layout_value in c("tab", "slide", "col", "pos")) {
    if (!sft_is_scalar_number(field[[layout_value]])) {
      stop(layout_value, " must be a numeric scalar.", call. = FALSE)
    }

    if (field[[layout_value]] < 0L) {
      stop(layout_value, " must be greater than or equal to 0.", call. = FALSE)
    }
  }

  sft_check_optional_label(field$tab_label, "tab_label")
  sft_check_optional_label(field$slide_label, "slide_label")

  allowed_status <- c("active", "retired", "orphaned")

  if (!field$status %in% allowed_status) {
    stop(
      "status must be one of: ",
      paste(allowed_status, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(field$renamed_from)) {
    sft_check_identifier(field$renamed_from, "renamed_from")

    if (identical(field$renamed_from, field$id)) {
      stop("renamed_from must not be identical to id.", call. = FALSE)
    }
  }

  invisible(field)
}

#' Print a form field
#'
#' @param x Object created with [form_field()].
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @examples
#' print(form_field(id = "name", label = "Name"))
#' @export
print.sft_field <- function(x, ...) {
  cat("<form_field>\n")
  cat("  id:         ", x$id, "\n", sep = "")
  cat("  label:      ", x$label, "\n", sep = "")
  cat("  type:       ", x$type, "\n", sep = "")
  cat("  input_type: ", x$input_type, "\n", sep = "")
  cat("  db_column:  ", x$db_column, "\n", sep = "")
  cat("  db_type:    ", x$db_type, "\n", sep = "")
  cat("  status:     ", x$status, "\n", sep = "")
  invisible(x)
}
