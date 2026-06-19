# Form action-button UI helpers.

sft_default_button_options <- function() {
  list(
    placement = "top",
    align = "left",
    class = "btn-sm",
    container_class = NULL,
    container_style = NULL,
    button_classes = list(
      open_add = "btn-primary",
      open_edit = "btn-default",
      delete = "btn-danger",
      refresh_table = "btn-default",
      open_deleted_records = "btn-default",
      open_column_settings = "btn-warning",
      open_column_selection = "btn-info"
    )
  )
}

sft_normalize_button_options <- function(button_options = list()) {
  if (is.null(button_options)) {
    button_options <- list()
  }

  if (!is.list(button_options)) {
    stop("button_options must be a named list.", call. = FALSE)
  }

  out <- utils::modifyList(
    sft_default_button_options(),
    button_options,
    keep.null = TRUE
  )

  valid_placements <- c("top", "bottom", "both", "none")
  if (!is.character(out$placement) || length(out$placement) != 1L ||
      !out$placement %in% valid_placements) {
    stop(
      "button_options$placement must be one of: ",
      paste(valid_placements, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  valid_align <- c("left", "center", "right", "between")
  if (!is.character(out$align) || length(out$align) != 1L ||
      !out$align %in% valid_align) {
    stop(
      "button_options$align must be one of: ",
      paste(valid_align, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  for (name in c("class", "container_class", "container_style")) {
    value <- out[[name]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L)) {
      stop("button_options$", name, " must be NULL or a character scalar.", call. = FALSE)
    }
  }

  if (is.null(out$button_classes)) {
    out$button_classes <- list()
  }

  if (is.character(out$button_classes)) {
    out$button_classes <- as.list(out$button_classes)
  }

  if (!is.list(out$button_classes)) {
    stop("button_options$button_classes must be NULL, a named character vector, or a named list.", call. = FALSE)
  }

  out
}

sft_button_justify_content <- function(align) {
  switch(
    align,
    left = "flex-start",
    center = "center",
    right = "flex-end",
    between = "space-between",
    "flex-start"
  )
}

sft_button_css <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
    "
    .sft-button-row {
      display: flex;
      flex-wrap: wrap;
      gap: 0.4rem;
      align-items: center;
      margin: 0.75rem 0;
    }
    .sft-button-row .btn {
      margin: 0;
    }
    .sft-records-table {
      width: 100%;
      overflow-x: auto;
    }
    .sft-records-table .dataTables_wrapper {
      width: 100%;
    }
    .sft-records-table table.dataTable {
      width: 100% !important;
      margin-left: 0 !important;
      margin-right: 0 !important;
    }
    .sft-records-table .dataTables_scrollHead table,
    .sft-records-table .dataTables_scrollBody table {
      margin-left: 0 !important;
      margin-right: 0 !important;
    }
    .sft-records-table table.dataTable.compact tbody td,
    .sft-records-table table.dataTable.compact thead th {
      padding: 0.35rem 0.55rem;
    }
    .sft-records-table table.dataTable thead th {
      white-space: nowrap;
    }
    "
    )),
    shiny::tags$script(shiny::HTML(
      "
      (function() {
        function registerSftResetHandler() {
          if (window.sftResetDataTableHandlerRegistered) return;

          if (!window.Shiny) {
            window.setTimeout(registerSftResetHandler, 100);
            return;
          }

          window.sftResetDataTableHandlerRegistered = true;

          Shiny.addCustomMessageHandler('sftResetDataTable', function(message) {
            var tableId = message.id;
            var node = document.getElementById(tableId);

            if (!node || !window.jQuery || !jQuery.fn.DataTable) return;

            var table = jQuery(node).DataTable();

            table.search('');
            table.columns().search('');
            table.order([]);
            table.page('first');
            if (table.rows && table.rows({selected: true}).deselect) {
              table.rows({selected: true}).deselect();
            }
            table.draw(false);
          });
        }

        registerSftResetHandler();
      })();
      "
    ))
  )
}

sft_button_class <- function(input_id, button_options) {
  classes <- c(
    "sft-action-button",
    button_options$class,
    button_options$button_classes[[input_id]]
  )

  paste(classes[nzchar(classes)], collapse = " ")
}

sft_action_button_if_label <- function(ns, input_id, labels, key, button_options = NULL) {
  label <- sft_ui_label(labels, key)

  if (is.null(label)) {
    return(NULL)
  }

  if (is.null(button_options)) {
    button_options <- sft_default_button_options()
  }

  shiny::actionButton(
    inputId = ns(input_id),
    label = label,
    class = sft_button_class(input_id, button_options)
  )
}

sft_form_button_row <- function(ns,
                                labels,
                                button_options = list(),
                                show_add = TRUE,
                                show_edit = TRUE,
                                show_delete = TRUE,
                                show_refresh_table = TRUE,
                                show_versions = TRUE,
                                show_deleted_records = TRUE,
                                show_column_settings = TRUE,
                                show_column_selection = TRUE) {
  labels <- sft_ui_labels(labels)
  button_options <- sft_normalize_button_options(button_options)

  if (identical(button_options$placement, "none")) {
    return(NULL)
  }

  shiny::div(
    class = paste(
      c("sft-button-row", button_options$container_class),
      collapse = " "
    ),
    style = paste(
      c(
        paste0("justify-content: ", sft_button_justify_content(button_options$align), ";"),
        button_options$container_style
      ),
      collapse = " "
    ),
    if (isTRUE(show_add)) {
      sft_action_button_if_label(ns, "open_add", labels, "open_add", button_options)
    },
    if (isTRUE(show_edit)) {
      sft_action_button_if_label(ns, "open_edit", labels, "open_edit", button_options)
    },
    if (isTRUE(show_delete)) {
      sft_action_button_if_label(ns, "delete", labels, "delete", button_options)
    },
    if (isTRUE(show_refresh_table)) {
      sft_action_button_if_label(ns, "refresh_table", labels, "refresh_table", button_options)
    },
    if (isTRUE(show_deleted_records)) {
      sft_action_button_if_label(ns, "open_deleted_records", labels, "open_deleted_records", button_options)
    },
    if (isTRUE(show_column_settings) || isTRUE(show_column_selection)) {
      sft_action_button_if_label(ns, "open_column_selection", labels, "open_column_selection", button_options)
    }
  )
}

#' External form action buttons
#'
#' Create the same action buttons used by [form_ui()] for placement outside
#' the module UI, for example in a page header or dashboard toolbar. The buttons
#' use the module namespace from `id`, so they trigger the matching
#' [form_server()] instance. Hide the internal buttons with
#' `form_ui(..., button_options = list(placement = "none"))` when using this
#' helper.
#'
#' @param id Module id matching [form_ui()] and [form_server()].
#' @param show_add,show_edit,show_delete,show_refresh_table,show_versions,show_deleted_records,show_column_settings,show_column_selection Logical flags controlling individual buttons. `show_versions` is kept for compatibility but the standalone versions button is no longer rendered by the default button row.
#' @param labels Optional named list overriding UI labels and button texts.
#' @param button_options Optional named list controlling action-button alignment
#'   and classes. The same structure as in [form_ui()] is supported.
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name"))
#' )
#' ui <- fluidPage(
#'   # External buttons in a header; hide the module's own button row.
#'   form_buttons("contacts", show_column_settings = FALSE),
#'   form_ui("contacts", button_options = list(placement = "none"))
#' )
#' server <- function(input, output, session) {
#'   form_server(id = "contacts", form = contacts)
#' }
#' shinyApp(ui, server)
#' }
#' @export
form_buttons <- function(id,
                             show_add = TRUE,
                             show_edit = TRUE,
                             show_delete = TRUE,
                             show_refresh_table = TRUE,
                             show_versions = FALSE,
                             show_deleted_records = TRUE,
                             show_column_settings = TRUE,
                             show_column_selection = TRUE,
                             labels = list(),
                             button_options = list()) {
  ns <- shiny::NS(id)

  shiny::tagList(
    sft_button_css(),
    sft_form_button_row(
      ns = ns,
      labels = labels,
      button_options = button_options,
      show_add = show_add,
      show_edit = show_edit,
      show_delete = show_delete,
      show_refresh_table = show_refresh_table,
      show_versions = show_versions,
      show_deleted_records = show_deleted_records,
      show_column_settings = show_column_settings,
      show_column_selection = show_column_selection
    )
  )
}

