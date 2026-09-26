# A grid of number cells as a Shiny input: rows and columns from the caller,
# keyboard navigation, block paste from a spreadsheet, live sums, and a
# matrix as the value. Assets in inst/assets/grid.

sft_grid_dependency <- function() {
  htmltools::htmlDependency(
    name = "sft-grid",
    version = as.character(utils::packageVersion("shinyformtools")),
    src = c(file = system.file("assets", "grid", package = "shinyformtools")),
    script = "sft-grid.js",
    stylesheet = "sft-grid.css"
  )
}

# A numeric matrix with the given row and column names, from whatever the
# caller passed as `value` (NULL fills with NA).
sft_grid_matrix <- function(value, rows, cols) {
  if (is.null(value)) {
    value <- matrix(NA_real_, nrow = length(rows), ncol = length(cols))
  }

  value <- as.matrix(value)
  storage.mode(value) <- "double"

  if (nrow(value) != length(rows) || ncol(value) != length(cols)) {
    stop(
      "value must be a ", length(rows), " x ", length(cols), " matrix, got ",
      nrow(value), " x ", ncol(value), ".",
      call. = FALSE
    )
  }

  dimnames(value) <- list(rows, cols)
  value
}

# A matrix as the client expects it: one array per row, NA as null.
sft_grid_json_rows <- function(value) {
  lapply(seq_len(nrow(value)), function(i) {
    unname(lapply(value[i, ], function(x) if (is.na(x)) NULL else x))
  })
}

# Stored representation: one JSON object holding names and values, so a
# stored grid decodes to the same matrix regardless of the field's current
# `rows` / `cols` arguments.
sft_grid_encode <- function(value) {
  if (is.null(value) || length(value) == 0L || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }

  value <- as.matrix(value)
  storage.mode(value) <- "double"

  as.character(jsonlite::toJSON(
    list(
      rows = rownames(value) %||% character(),
      cols = colnames(value) %||% character(),
      values = unname(value)
    ),
    digits = NA,
    na = "null"
  ))
}

sft_grid_decode <- function(stored) {
  if (is.null(stored) || length(stored) == 0L || is.na(stored[1L]) || !nzchar(stored[1L])) {
    return(NULL)
  }

  parsed <- jsonlite::fromJSON(stored[1L], simplifyVector = TRUE, simplifyMatrix = TRUE)
  rows <- as.character(parsed$rows)
  cols <- as.character(parsed$cols)
  values <- parsed$values

  if (is.null(values) || length(values) == 0L) {
    values <- matrix(NA_real_, nrow = length(rows), ncol = length(cols))
  } else if (is.list(values)) {
    # Ragged or null-holding rows come back as a list of rows.
    values <- do.call(rbind, lapply(values, function(line) {
      vapply(line, function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x), numeric(1))
    }))
  }

  # jsonlite hands a numeric matrix back in row order already; only its
  # storage mode and names are set here.
  values <- matrix(as.numeric(values), nrow = length(rows), ncol = length(cols))
  dimnames(values) <- list(rows, cols)
  values
}

# What the records table shows for a stored grid: the column totals.
sft_grid_format <- function(value, sep = "; ") {
  m <- sft_grid_decode(value)

  if (is.null(m)) {
    return("")
  }

  totals <- colSums(m, na.rm = TRUE)
  paste(sprintf("%s %s", colnames(m), format(totals, trim = TRUE)), collapse = sep)
}

# Shiny input handler: the client's {rows, cols, values} becomes a matrix.
sft_grid_input_handler <- function(value, shinysession = NULL, name = NULL) {
  if (is.null(value)) {
    return(NULL)
  }

  rows <- as.character(unlist(value$rows))
  cols <- as.character(unlist(value$cols))
  out <- matrix(NA_real_, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))

  for (r in seq_along(value$values)) {
    line <- value$values[[r]]

    for (c in seq_along(line)) {
      cell <- line[[c]]

      if (!is.null(cell) && length(cell) == 1L && !is.na(cell)) {
        out[r, c] <- as.numeric(cell)
      }
    }
  }

  out
}

#' A grid of number cells as a Shiny input
#'
#' Renders a table whose rows and columns are fixed and whose cells are
#' number inputs, for entering many related values at once (a cross table of
#' counts, say). Enter and the arrow keys walk the cells, a block copied from
#' a spreadsheet is pasted from the focused cell onwards, and row, column and
#' total sums follow the entries. The input's value is a numeric matrix with
#' the row and column names; an empty cell is `NA`. The value updates half a
#' second after the last keystroke.
#'
#' `grid_input` is also a built-in `input_type` for [form_field()]: the matrix
#' is stored as one JSON text and shown in the records table as its column
#' totals. Pass `rows`, `cols` and the other arguments through
#' `form_field(args = list(...))`.
#'
#' @param inputId Input id.
#' @param label Optional label above the grid.
#' @param value Numeric matrix of starting values, or `NULL` for empty cells.
#'   Its dimnames supply `rows` and `cols` when those are not given.
#' @param rows,cols Row and column labels.
#' @param hint Optional matrix of the same shape shown greyed inside each cell,
#'   for example yesterday's values. It is a hint only, never the value.
#' @param min,max,step Passed to the number cells.
#' @param sums Logical. Show row, column and total sums.
#' @param sum_label Header of the sum column and row.
#' @param width Optional CSS width of the whole grid.
#'
#' @return A Shiny input tag.
#' @seealso [update_grid_input()]
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   grid_input("votes", "Votes", rows = c("Party A", "Party B"),
#'              cols = c("First", "Second")),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output, session) {
#'   output$value <- renderPrint(input$votes)
#' }
#' shinyApp(ui, server)
#' }
#' @export
grid_input <- function(inputId,
                       label = NULL,
                       value = NULL,
                       rows = rownames(value),
                       cols = colnames(value),
                       hint = NULL,
                       min = 0,
                       max = NULL,
                       step = 1,
                       sums = TRUE,
                       sum_label = "Sum",
                       width = NULL) {
  if (is.null(rows) || is.null(cols)) {
    stop("rows and cols are needed, either directly or as the dimnames of value.", call. = FALSE)
  }

  rows <- as.character(rows)
  cols <- as.character(cols)
  value <- sft_grid_matrix(value, rows, cols)
  hint <- if (!is.null(hint)) sft_grid_matrix(hint, rows, cols)

  cell <- function(r, c) {
    x <- value[r, c]
    h <- if (!is.null(hint)) hint[r, c] else NA_real_

    shiny::tags$td(
      class = "sft-grid-cell-wrap",
      shiny::tags$input(
        type = "number",
        class = paste(c("sft-grid-cell", if (!is.na(x) && x != 0) "sft-grid-filled"), collapse = " "),
        value = if (is.na(x)) "" else x,
        min = min, max = max, step = step,
        `data-r` = r - 1L, `data-c` = c - 1L,
        `aria-label` = paste(rows[r], cols[c])
      ),
      if (!is.na(h)) shiny::tags$span(class = "sft-grid-hint", h)
    )
  }

  header <- shiny::tags$thead(shiny::tags$tr(
    shiny::tags$th(class = "sft-grid-row-label", ""),
    lapply(cols, shiny::tags$th),
    if (isTRUE(sums)) shiny::tags$th(class = "sft-grid-sum", sum_label)
  ))

  body <- shiny::tags$tbody(lapply(seq_along(rows), function(r) {
    shiny::tags$tr(
      shiny::tags$td(class = "sft-grid-row-label", rows[r]),
      lapply(seq_along(cols), function(c) cell(r, c)),
      if (isTRUE(sums)) shiny::tags$td(class = "sft-grid-sum", `data-sum-row` = r - 1L, "")
    )
  }))

  footer <- if (isTRUE(sums)) {
    shiny::tags$tfoot(shiny::tags$tr(
      shiny::tags$td(class = "sft-grid-row-label", sum_label),
      lapply(seq_along(cols), function(c) shiny::tags$td(class = "sft-grid-sum", `data-sum-col` = c - 1L, "")),
      shiny::tags$td(class = "sft-grid-sum sft-grid-total", "")
    ))
  }

  tag <- shiny::tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label)) shiny::tags$label(class = "control-label", `for` = inputId, label),
    shiny::tags$div(
      id = inputId,
      class = "sft-grid-input",
      `data-rows` = as.character(jsonlite::toJSON(rows)),
      `data-cols` = as.character(jsonlite::toJSON(cols)),
      `data-sums` = tolower(isTRUE(sums)),
      shiny::tags$table(class = "sft-grid", header, body, footer)
    )
  )

  htmltools::attachDependencies(tag, sft_grid_dependency())
}

#' Update a grid input from the server
#'
#' @param session The Shiny session.
#' @param inputId Input id.
#' @param value Optional numeric matrix of the grid's shape; `NA` empties a
#'   cell.
#' @param hint Optional matrix of the same shape shown greyed inside the cells.
#'
#' @return Called for its side effect.
#' @seealso [grid_input()]
#' @export
update_grid_input <- function(session, inputId, value = NULL, hint = NULL) {
  message <- list()

  if (!is.null(value)) {
    value <- as.matrix(value)
    storage.mode(value) <- "double"
    message$values <- sft_grid_json_rows(value)
  }

  if (!is.null(hint)) {
    hint <- as.matrix(hint)
    storage.mode(hint) <- "double"
    message$hint <- sft_grid_json_rows(hint)
  }

  if (length(message) > 0L) {
    session$sendInputMessage(inputId, message)
  }

  invisible(NULL)
}
