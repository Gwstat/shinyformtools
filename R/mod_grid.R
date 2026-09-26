# Grid entry over several records: grid_ui() / grid_server(). One stored
# record per row of the grid, matched by a row key (and a group key that
# names the set of records the grid shows); the value columns are numeric
# fields of the form. Saving goes through upsert_records() in one
# transaction.

sft_grid_empty_row <- function(values) {
  all(unlist(values) %in% c(0, NA))
}

# A reactive, a function or a plain value.
sft_grid_resolve <- function(x) {
  if (is.function(x)) x() else x
}

# The rows of the grid as a data frame with `value` (the row key stored in
# the record) and `label` (shown in the grid).
sft_grid_row_spec <- function(rows) {
  if (is.data.frame(rows)) {
    if (!"value" %in% names(rows)) {
      stop("rows given as a data frame need a `value` column.", call. = FALSE)
    }
    label <- if ("label" %in% names(rows)) rows$label else rows$value
    return(data.frame(value = as.character(rows$value), label = as.character(label), stringsAsFactors = FALSE))
  }

  rows <- as.character(rows)
  labels <- names(rows)

  if (is.null(labels)) {
    labels <- rows
  } else {
    labels[!nzchar(labels)] <- rows[!nzchar(labels)]
  }

  data.frame(value = unname(rows), label = unname(labels), stringsAsFactors = FALSE)
}

# The value fields (columns) of the grid: named explicitly, or every numeric
# input field that is neither the row key nor a group field.
sft_grid_value_fields <- function(form, cols, exclude) {
  fields <- sft_active_input_fields(form)
  ids <- vapply(fields, function(field) field$id, character(1))

  if (is.null(cols)) {
    numeric_types <- c("numericInput", "sliderInput")
    keep <- vapply(fields, function(field) field$input_type %in% numeric_types, logical(1)) & !ids %in% exclude
    fields <- fields[keep]
  } else {
    unknown <- setdiff(cols, ids)
    if (length(unknown) > 0L) {
      stop("cols names fields the form does not have: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    fields <- fields[match(cols, ids)]
  }

  if (length(fields) == 0L) {
    stop("The grid needs at least one value field (numeric input fields, or `cols`).", call. = FALSE)
  }

  fields
}

# The stored records of the group as the grid's matrix (rows x columns, NA
# where no record exists).
sft_grid_load <- function(conn, form, group_values, row_spec, key_field, value_fields) {
  encoded <- lapply(names(group_values), function(id) {
    sft_field_db_value(sft_key_fields(form, id)[[1L]], group_values[[id]])
  })
  names(encoded) <- vapply(names(group_values), function(id) sft_key_fields(form, id)[[1L]]$db_column, character(1))

  stored <- sft_find_live_records(conn, form, encoded)
  out <- matrix(
    NA_real_, nrow = nrow(row_spec), ncol = length(value_fields),
    dimnames = list(row_spec$label, vapply(value_fields, function(field) field$label, character(1)))
  )

  if (nrow(stored) == 0L) {
    return(out)
  }

  position <- match(as.character(stored[[key_field$db_column]]), row_spec$value)

  for (j in seq_along(value_fields)) {
    column <- value_fields[[j]]$db_column
    ok <- !is.na(position) & !is.na(stored[[column]])
    out[position[ok], j] <- as.numeric(stored[[column]][ok])
  }

  out
}

# The grid's matrix as the records upsert_records() expects.
sft_grid_records <- function(value, group_values, row_spec, key, value_ids) {
  lapply(seq_len(nrow(row_spec)), function(i) {
    cells <- as.list(value[i, ])
    names(cells) <- value_ids
    c(group_values, stats::setNames(list(row_spec$value[i]), key), cells)
  })
}

#' Grid entry over several records
#'
#' A grid whose rows are records of a form and whose columns are the form's
#' numeric fields, for entering many related records at once: the parties of
#' one polling station, the items of one order. Each row of the grid is one
#' stored record, identified by a row key (`key`) and, usually, a group key
#' (`group`) that names the set of records the grid shows. Saving writes the
#' whole grid through [upsert_records()] in one transaction: rows that are
#' new are inserted, changed rows updated, unchanged rows left alone, empty
#' rows soft-deleted (so the table holds only what was entered), and stored
#' rows of the group that the grid no longer lists are soft-deleted too.
#'
#' The grid itself is [grid_input()]: Enter and the arrow keys walk the cells,
#' a block from a spreadsheet can be pasted, sums follow the entries. It is
#' rendered once per group (and whenever `rows` change), never on save, so the
#' cursor stays where it is while values are written half a second after the
#' last keystroke (`autosave = TRUE`) or on a Save button.
#'
#' Two people editing the same group at the same time both write; the last
#' save of a row wins. Every write is audit-logged per record like any other.
#'
#' @param id Module id.
#' @param label Optional label above the grid.
#' @param autosave Logical. `TRUE` saves half a second after the last change;
#'   `FALSE` shows a Save button instead. Give `grid_ui()` and `grid_server()`
#'   the same value.
#' @param width Optional CSS width of the grid.
#' @param form Object created with [form()]. Its fields hold the row key, the
#'   group fields and the numeric value fields.
#' @param rows The rows of the grid: a character vector of row keys (names,
#'   when given, are the labels shown), a data frame with `value` and `label`
#'   columns, or a function / reactive returning one of these.
#' @param key Field id of the row key, for example `"party"`.
#' @param group Named list of field values that identify the group the grid
#'   shows, for example `list(day = "fri", station = "12")`, or a function /
#'   reactive returning one. `NULL` means the grid covers the whole table.
#' @param cols Field ids of the value columns, in this order. Default: every
#'   numeric input field that is neither the row key nor a group field.
#' @param conn Optional DBI connection; see [connections]. Without one the
#'   module opens its own for the session.
#' @param user Optional user identifier or function returning it, for the
#'   audit log.
#' @param hint Optional function of the group values returning a matrix of the
#'   grid's shape (or `NULL`) shown greyed inside the cells, for example the
#'   previous day's entries. A hint is never saved.
#' @param empty Function of a row's values (the value columns only) returning
#'   `TRUE` when the row counts as empty. Default: every cell 0 or `NA`.
#' @param sums Logical. Show row, column and total sums.
#' @param language Optional [language()] object or function / reactive
#'   returning one.
#' @param labels Optional named list overriding the module's labels
#'   (`grid_save`, `grid_saving`, `grid_saved`, `grid_sum`).
#'
#' @return `grid_ui()` returns the UI. `grid_server()` returns a list with
#'   `changed` (a reactive counter incremented on every save that wrote
#'   something), `value` (the grid's current matrix) and `reload()` (re-read
#'   the group from the database).
#' @examples
#' \dontrun{
#' library(shiny)
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' counts <- form(
#'   form_id = "counts", table_name = "counts", db = db,
#'   fields = list(
#'     form_field(id = "station", label = "Station"),
#'     form_field(id = "party", label = "Party"),
#'     form_field(id = "posters", label = "Posters", input_type = "numericInput"),
#'     form_field(id = "flyers", label = "Flyers", input_type = "numericInput")
#'   )
#' )
#' ui <- fluidPage(
#'   selectInput("station", "Station", c("North", "South")),
#'   grid_ui("counts")
#' )
#' server <- function(input, output, session) {
#'   grid_server(
#'     "counts", counts,
#'     rows = c("Party A", "Party B", "Party C"), key = "party",
#'     group = reactive(list(station = input$station)), user = "demo"
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' @name grid_module
NULL

#' @rdname grid_module
#' @export
grid_ui <- function(id, label = NULL, autosave = TRUE, width = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    if (!is.null(label)) shiny::tags$label(class = "control-label", label),
    shiny::uiOutput(ns("grid")),
    shiny::tags$div(
      class = "sft-grid-bar",
      style = "margin-top: .4rem; display: flex; gap: .75rem; align-items: center;",
      if (!isTRUE(autosave)) shiny::uiOutput(ns("save_button"), inline = TRUE),
      shiny::textOutput(ns("status"), inline = TRUE)
    )
  )
}

#' @rdname grid_module
#' @export
grid_server <- function(id,
                        form,
                        rows,
                        key,
                        group = NULL,
                        cols = NULL,
                        conn = NULL,
                        user = NULL,
                        hint = NULL,
                        empty = sft_grid_empty_row,
                        autosave = TRUE,
                        sums = TRUE,
                        language = NULL,
                        labels = list()) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  key_field <- sft_key_fields(form, key)[[1L]]

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    owns_connection <- is.null(conn)
    handle <- if (owns_connection) db_connect(form$db) else conn

    if (owns_connection) {
      session$onSessionEnded(function() {
        try(db_disconnect(handle), silent = TRUE)
      })
    }

    live <- function() {
      if (owns_connection) {
        handle <<- sft_live_connection(handle, form$db)
      }
      handle
    }

    ui_labels <- function() sft_with_language(language, sft_ui_labels(labels))
    current_user <- function() if (is.function(user)) user() else user

    group_values <- shiny::reactive({
      values <- sft_grid_resolve(group)

      if (!is.null(values) && (!is.list(values) || is.null(names(values)))) {
        stop("group must be NULL or a named list of field values.", call. = FALSE)
      }

      values
    })

    row_spec <- shiny::reactive(sft_grid_row_spec(sft_grid_resolve(rows)))

    value_fields <- shiny::reactive({
      sft_grid_value_fields(form, cols, exclude = c(key, names(group_values())))
    })

    loaded <- shiny::reactiveVal(NULL)
    changed <- shiny::reactiveVal(0L)
    status <- shiny::reactiveVal("")
    structure_tick <- shiny::reactiveVal(0L)

    load <- function() {
      # Like every read of the package: the schema is ensured (probe-gated).
      conn <- sft_prepare_mutation(form, live())
      sft_grid_load(conn, form, group_values(), row_spec(), key_field, value_fields())
    }

    output$grid <- shiny::renderUI({
      structure_tick()
      current <- load()
      loaded(current)
      status("")

      hint_matrix <- if (!is.null(hint)) {
        h <- if (is.function(hint)) hint(group_values()) else hint
        if (!is.null(h)) sft_grid_matrix(h, rownames(current), colnames(current))
      }

      grid_input(
        ns("cells"),
        value = current,
        hint = hint_matrix,
        sums = sums,
        sum_label = sft_ui_label(ui_labels(), "grid_sum") %||% "Sum"
      )
    })

    save <- function(value) {
      if (is.null(value)) {
        return(invisible(FALSE))
      }

      spec <- row_spec()
      fields <- value_fields()

      if (!identical(dim(value), c(nrow(spec), length(fields)))) {
        return(invisible(FALSE))
      }

      status(sft_ui_label(ui_labels(), "grid_saving") %||% "")
      group <- group_values()

      result <- tryCatch(
        sft_with_language(language, upsert_records(
          form,
          sft_grid_records(value, group, spec, key, vapply(fields, function(field) field$id, character(1))),
          key = c(names(group), key),
          conn = live(),
          user = current_user(),
          empty = empty,
          scope = group
        )),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        status("")
        shiny::showNotification(conditionMessage(result), type = "error", duration = 8)
        return(invisible(FALSE))
      }

      loaded(value)

      if (any(result$action %in% c("insert", "update", "delete"))) {
        changed(changed() + 1L)
      }

      status(sft_ui_label(
        ui_labels(), "grid_saved",
        values = list(time = format(Sys.time(), "%H:%M:%S"))
      ) %||% "")
      invisible(TRUE)
    }

    unchanged <- function(value) {
      previous <- loaded()
      !is.null(previous) && identical(dim(value), dim(previous)) &&
        isTRUE(all.equal(unname(value), unname(previous)))
    }

    shiny::observeEvent(input$cells, {
      value <- input$cells

      if (unchanged(value)) {
        return()
      }

      if (isTRUE(autosave)) {
        save(value)
      } else {
        status("")
      }
    })

    output$save_button <- shiny::renderUI({
      shiny::actionButton(ns("save"), sft_ui_label(ui_labels(), "grid_save") %||% "Save", class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$save, {
      save(shiny::isolate(input$cells))
    })

    output$status <- shiny::renderText(status())

    list(
      changed = shiny::reactive(changed()),
      value = shiny::reactive(input$cells),
      reload = function() structure_tick(shiny::isolate(structure_tick()) + 1L)
    )
  })
}
