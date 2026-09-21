# Form action-button UI helpers.

sft_default_button_options <- function() {
  list(
    placement = "top",
    align = "left",
    class = "btn-sm",
    container_class = NULL,
    container_style = NULL,
    # Neutral by default: the form module is a building block, so it should not
    # impose an accent colour on the host app. Override per button via
    # button_options$button_classes.
    button_classes = list(
      open_add = "btn-default",
      open_edit = "btn-default",
      delete = "btn-default",
      refresh_table = "btn-default",
      export_csv = "btn-default",
      export_csv2 = "btn-default",
      export_xlsx = "btn-default",
      open_deleted_records = "btn-default",
      open_column_settings = "btn-default",
      open_column_selection = "btn-default"
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
                                show_column_selection = TRUE,
                                show_export = FALSE) {
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
    },
    sft_export_buttons(ns, labels, show_export, button_options)
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
#' As in [form_ui()], only the CRUD core is on by default; the deleted-records
#' and column buttons are opt-in.
#'
#' @param id Module id matching [form_ui()] and [form_server()].
#' @param show_add,show_edit,show_delete,show_refresh_table,show_deleted_records,show_column_settings,show_column_selection Logical flags controlling individual buttons. `show_deleted_records`, `show_column_settings` and `show_column_selection` default to `FALSE`, matching [form_ui()].
#' @param show_export Download buttons for the records table; see [form_ui()].
#' @param labels Optional named list overriding UI labels and button texts.
#' @param button_options Optional named list controlling action-button alignment
#'   and classes. The same structure as in [form_ui()] is supported.
#' @param ... Deprecated: `show_versions` (no effect; versions are shown inside
#'   the edit dialog when permitted).
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
#'   form_buttons("contacts"),
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
                             show_deleted_records = FALSE,
                             show_column_settings = FALSE,
                             show_column_selection = FALSE,
                             labels = list(),
                             button_options = list(),
                             show_export = FALSE,
                             ...) {
  ns <- shiny::NS(id)
  sft_map_deprecated_args(
    dots = list(...),
    mapping = list(show_versions = list(bundle = NULL, key = NULL)),
    fn = "form_buttons"
  )

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
      show_deleted_records = show_deleted_records,
      show_column_settings = show_column_settings,
      show_column_selection = show_column_selection,
      show_export = show_export
    )
  )
}

# Reactively hide the action buttons, the records table and the audit table
# whose permission is FALSE (`permissions$hide_forbidden`), so the visible
# controls match the permissions without callers reaching into namespaced
# ids. The server-side guards in mod_crud.R stay in force regardless of
# visibility, so a hidden control can never trigger its action. Registrar.
sft_register_visibility <- function(input, output, session, state) {
  permissions <- state$permissions

  if (!isTRUE(permissions$hide_forbidden)) {
    return(invisible(list()))
  }

  button_permissions <- list(
    open_add = permissions$can_add,
    open_edit = permissions$can_view_record,
    delete = permissions$can_delete,
    refresh_table = permissions$can_reset_table,
    open_deleted_records = permissions$can_view_deleted_records,
    open_column_selection = function() {
      state$permission("can_select_column_view") ||
        state$permission("can_change_column_settings")
    }
  )

  shiny::observe({
    for (button_id in names(button_permissions)) {
      shinyjs::toggle(
        id = button_id,
        condition = sft_module_permission(button_permissions[[button_id]], default = TRUE)
      )
    }

    # No-op for the formats form_ui() drew no button for.
    for (export_format in sft_export_formats()) {
      shinyjs::toggle(
        id = sft_export_button_id(export_format),
        condition = state$permission("can_export") && state$permission("can_view_table")
      )
    }

    shinyjs::toggle(
      id = "records_container",
      condition = state$permission("can_view_table")
    )

    # No-op when form_ui() did not render the audit container.
    shinyjs::toggle(
      id = "audit_container",
      condition = state$permission("can_view_audit")
    )
  })

  invisible(list())
}
