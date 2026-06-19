# Public form module UI and server entry points.

#' Form module UI
#'
#' @param id Module id.
#' @param title Optional title shown above the module.
#' @param show_user Logical. Whether to show a user input field.
#' @param show_audit Logical. Whether to show the audit log table.
#' @param show_include_deleted Logical. Whether to show the legacy include-deleted
#'   checkbox.
#' @param show_add Logical. Whether to show the add button.
#' @param show_edit Logical. Whether to show the edit button.
#' @param show_deleted_records Logical. Whether to show the deleted-records dialog button.
#' @param show_delete Logical. Whether to show the delete button.
#' @param show_refresh_table Logical. Whether to show a button that clears table filters, ordering, paging and row selection.
#' @param show_versions Deprecated compatibility flag. Standalone version buttons are no longer rendered by the default UI; versions are shown in the case modal when permitted.
#' @param show_column_settings Logical. Whether to show the admin column-settings button.
#' @param show_column_selection Logical. Whether to show the user column-selection button.
#' @param form_layout Where the add/edit forms render. `"modal"` (default) opens
#'   them in a dialog; `"inline"` renders them in a panel above the records table,
#'   with add and edit mutually exclusive. Must match the `form_layout` passed to
#'   [form_server()].
#' @param labels Optional named list overriding UI labels and button texts.
#' @param button_options Optional named list controlling action-button placement
#'   and classes. Supported entries are `placement` (`"top"`, `"bottom"`,
#'   `"both"`, `"none"`), `align` (`"left"`, `"center"`,
#'   `"right"`, `"between"`), `class`, `container_class`,
#'   `container_style` and `button_classes`.
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' ui <- fluidPage(form_ui("contacts", title = "Contacts"))
#' server <- function(input, output, session) {
#'   form_server(id = "contacts", form = contacts)
#' }
#' shinyApp(ui, server)
#' }
#' @export
form_ui <- function(id,
                        title = NULL,
                        show_user = TRUE,
                        show_audit = TRUE,
                        show_include_deleted = FALSE,
                        show_add = TRUE,
                        show_edit = TRUE,
                        show_deleted_records = TRUE,
                        show_delete = TRUE,
                        show_refresh_table = TRUE,
                        show_versions = FALSE,
                        show_column_settings = TRUE,
                        show_column_selection = TRUE,
                        form_layout = c("modal", "inline"),
                        labels = list(),
                        button_options = list()) {
  ns <- shiny::NS(id)
  form_layout <- match.arg(form_layout)
  labels <- sft_ui_labels(labels)
  button_options <- sft_normalize_button_options(button_options)

  button_row <- function() {
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
  }

  top_buttons <- if (button_options$placement %in% c("top", "both")) {
    button_row()
  }

  bottom_buttons <- if (button_options$placement %in% c("bottom", "both")) {
    button_row()
  }

  shiny::tagList(
    shinyjs::useShinyjs(),
    sft_button_css(),

    if (!is.null(title)) {
      shiny::h3(title)
    },

    if (isTRUE(show_user)) {
      shiny::textInput(
        inputId = ns("sft_user"),
        label = sft_ui_label(labels, "user_label"),
        value = Sys.info()[["user"]]
      )
    },

    top_buttons,

    if (isTRUE(show_include_deleted)) {
      shiny::checkboxInput(
        inputId = ns("include_deleted"),
        label = sft_ui_label(labels, "include_deleted"),
        value = FALSE
      )
    },

    if (identical(form_layout, "inline")) {
      shiny::uiOutput(ns("sft_inline_form"))
    },

    shiny::hr(),

    shiny::div(
      id = ns("records_container"),
      class = "sft-records-table",
      DT::DTOutput(ns("records"))
    ),

    bottom_buttons,

    if (isTRUE(show_audit)) {
      shiny::div(
        id = ns("audit_container"),
        shiny::hr(),
        shiny::h4(sft_ui_label(labels, "audit_title")),
        DT::DTOutput(ns("audit"))
      )
    }
  )
}

#' Form module server
#'
#' @param id Module id.
#' @param form Object created with [form()].
#' @param conn Optional existing DBI connection.
#' @param user Optional user identifier or function returning a user identifier.
#' @param include_deleted_default Logical default for including deleted records.
#' @param show_audit Logical. Whether audit output should be rendered.
#' @param table_columns Optional columns shown in the records table.
#' @param table_views Optional named list of predefined table views. Each entry is
#'   a character vector of database column names. A function returning such a list
#'   is also accepted.
#' @param default_column_view Optional default table-view name. Can be a scalar or
#'   a function/reactive returning a scalar.
#' @param show_system_columns Logical. Whether extended system columns are shown.
#' @param can_add Logical or function. Whether the current user may add records.
#' @param can_view_record Logical or function. Whether the current user may open
#'   the selected record in the read-only/edit modal.
#' @param can_edit Logical or function. Whether the current user may save changes
#'   to an opened record.
#' @param can_delete Logical or function. Whether the current user may soft-delete
#'   records.
#' @param can_restore Logical or function. Whether the current user may restore
#'   previous record versions.
#' @param can_view_versions Logical or function. Whether the current user may open
#'   the versions dialog.
#' @param can_view_deleted_records Logical or function. Whether the current user
#'   may open the deleted-records dialog.
#' @param can_change_column_settings Logical or function. Whether the current user
#'   may open and save shared column settings. Intended for admins/managers.
#' @param can_select_column_view Logical or function. Whether the current user
#'   may load an existing column view.
#' @param can_view_audit Logical or function. Whether the current user may see
#'   the audit-log table. Only effective when the audit table is part of the UI
#'   (`form_ui(show_audit = TRUE)`); when `FALSE` the audit table is hidden
#'   and no audit rows are sent to the client.
#' @param can_view_table Logical or function. Whether the current user may see
#'   the records table at all. When `FALSE` the table is hidden and no rows are
#'   sent to the client.
#' @param can_reset_table Logical or function. Whether the current user may use
#'   the reset/refresh button that clears table filters, ordering, paging and
#'   selection.
#' @param hide_forbidden Logical. When `TRUE` (default), action buttons, the
#'   reset button, the records table and the audit table whose `can_*`
#'   permission is `FALSE` are hidden reactively, so the visible controls match
#'   the permissions. The server-side guards stay in force regardless, so a
#'   hidden control can never trigger its action. Set to `FALSE` to keep every
#'   control visible and rely on the warning that a denied action shows.
#' @param editable_fields Optional character vector of field ids the current
#'   user may edit (or a function/reactive returning one). When supplied, every
#'   other input field in the edit dialog is rendered read-only and is ignored on
#'   save, so a user can be allowed to edit only specific inputs. `NULL` (default)
#'   keeps each field's own `editable` setting.
#' @param labels Optional named list overriding UI labels, modal texts and
#'   notification messages. Set individual entries to `NULL` to hide the
#'   corresponding button or modal title.
#' @param modal_sizes Optional named list with modal settings for `add`, `edit`,
#'   `delete`, `versions` and `column_settings`. Each entry can be `"s"`,
#'   `"m"`, `"l"`, a CSS width such as `"90vw"`, or a list with `size`,
#'   `width`, `height` and `max_height`.
#' @param persist_column_settings Logical. Whether per-user column choices are
#'   stored in the form database.
#' @param display_transform Optional function used to derive the records table
#'   shown to the user from the raw database records. The function may use
#'   `function(data)` or `function(data, context)` and must return a data frame.
#'   If raw records contain `sft_id`, the returned data must keep `sft_id` so
#'   row selections can be mapped back to the underlying record.
#' @param display_column_labels Optional named character vector with labels for
#'   additional display-only columns created by `display_transform`.
#' @param input_bindings Optional list of dynamic input bindings created with
#'   [dynamic_choices()], [dynamic_value()] or [dynamic_visibility()]. Bindings
#'   are registered for both add and edit dialogs.
#' @param modal_header Optional UI or function rendered at the top of add/edit
#'   dialogs. Functions may declare any of `values`, `record`, `context`,
#'   `prefix`, `input`, `output`, `session`, `ns` and `form`. This is intended
#'   for display-only, cross-table context such as contact details or linked
#'   record summaries.
#' @param table_options Additional DT options for the records table.
#' @param table_class CSS class passed to [DT::datatable()] for the records table.
#' @param table_filter Per-column search controls for the records table, passed
#'   to [DT::datatable()]. `"none"` (default), `"top"` or `"bottom"`. The control
#'   adapts to each column's type (range slider for numeric/integer columns, a
#'   select for factors, a text box otherwise).
#' @param table_format Optional function for display-only DT formatting. Functions
#'   may declare any of `table`, `data` and `context` and must return a DT table
#'   widget. This replaces legacy `tableedit2` string hooks with regular R code,
#'   for example `function(table) DT::formatStyle(table, "Status", ...)`.
#' @param audit_options Additional DT options for the audit table.
#' @param version_options Additional DT options for the version table.
#' @param deleted_records_options Additional DT options for the deleted-records table.
#' @param datetime_format Format used for displayed timestamps.
#' @param refresh_triggers Optional reactive (or list of reactives) that this
#'   table should re-fetch on. Pass another form's returned `changed` reactive to
#'   make this table react to changes in that table, so dependent tables and
#'   downstream outputs (maps, summaries) stay in sync.
#' @param form_layout Where the add/edit forms render. `"modal"` (default) opens
#'   them in a dialog; `"inline"` renders them in a panel above the records table,
#'   with add and edit mutually exclusive. Must match the `form_layout` passed to
#'   [form_ui()].
#'
#' @return A list of reactive helpers: `records`, `display_records` and
#'   `selected_record` reactives, a `changed` reactive that increments on every
#'   mutation (insert/update/delete/restore), a `refresh()` function, and the
#'   `conn` and `form`.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' ui <- fluidPage(form_ui("contacts", title = "Contacts"))
#' server <- function(input, output, session) {
#'   form_server(id = "contacts", form = contacts)
#' }
#' shinyApp(ui, server)
#' }
#' @export
form_server <- function(id,
                            form,
                            conn = NULL,
                            user = NULL,
                            include_deleted_default = FALSE,
                            show_audit = TRUE,
                            table_columns = NULL,
                            table_views = NULL,
                            default_column_view = "Standard",
                            show_system_columns = FALSE,
                            can_add = TRUE,
                            can_view_record = TRUE,
                            can_edit = TRUE,
                            can_delete = TRUE,
                            can_restore = TRUE,
                            can_view_versions = TRUE,
                            can_view_deleted_records = TRUE,
                            can_change_column_settings = TRUE,
                            can_select_column_view = TRUE,
                            can_view_audit = TRUE,
                            can_view_table = TRUE,
                            can_reset_table = TRUE,
                            hide_forbidden = TRUE,
                            editable_fields = NULL,
                            labels = list(),
                            modal_sizes = list(),
                            persist_column_settings = TRUE,
                            display_transform = NULL,
                            display_column_labels = NULL,
                            input_bindings = NULL,
                            modal_header = NULL,
                            table_options = list(),
                            table_class = "display compact stripe hover",
                            table_filter = "none",
                            table_format = NULL,
                            audit_options = list(),
                            version_options = list(),
                            deleted_records_options = list(),
                            datetime_format = sft_default_datetime_format(),
                            refresh_triggers = NULL,
                            form_layout = c("modal", "inline")) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  form_layout <- match.arg(form_layout)
  labels <- sft_ui_labels(labels)
  modal_sizes <- sft_modal_sizes(modal_sizes)
  sft_check_form_region(modal_header, "modal_header")
  sft_check_table_format(table_format)

  shiny::moduleServer(id, function(input, output, session) {
    owns_connection <- is.null(conn)

    if (owns_connection) {
      conn <- db_connect(form$db)

      session$onSessionEnded(function() {
        db_disconnect(conn)
      })
    }

    initial_user <- if (!is.null(user) && !is.function(user)) {
      user
    } else {
      NA_character_
    }

    sft_ensure_schema(
      conn = conn,
      form = form,
      user = initial_user
    )

    if (is.function(form$server)) {
      form$server(input, output, session)
    }

    refresh_tick <- shiny::reactiveVal(0L)
    restore_record_id <- shiny::reactiveVal(NULL)
    record_columns <- shiny::reactiveVal(table_columns)
    record_columns_loaded_for <- shiny::reactiveVal(NULL)
    active_column_view <- shiny::reactiveVal("Standard")
    current_edit_row <- shiny::reactiveVal(NULL)
    selected_record_id <- shiny::reactiveVal(NULL)
    table_structure_tick <- shiny::reactiveVal(0L)
    # Inline (non-modal) add/edit panel state: NULL | "add" | "edit". Single value,
    # so add and edit are mutually exclusive by construction.
    inline_active <- shiny::reactiveVal(NULL)

    set_record_columns <- function(columns) {
      old <- record_columns()
      columns <- as.character(columns %||% character())
      old <- as.character(old %||% character())

      if (!identical(old, columns)) {
        record_columns(columns)
        table_structure_tick(table_structure_tick() + 1L)
      } else {
        record_columns(columns)
      }

      invisible(columns)
    }

    refresh <- function() {
      refresh_tick(refresh_tick() + 1L)
    }

    # Re-fetch when an external dependency (another form's `changed` reactive)
    # signals a change, so dependent tables stay in sync.
    external_triggers <- if (is.null(refresh_triggers)) {
      list()
    } else if (is.list(refresh_triggers)) {
      refresh_triggers
    } else {
      list(refresh_triggers)
    }

    for (trigger in external_triggers) {
      local({
        trigger_fn <- trigger
        shiny::observeEvent(
          trigger_fn(),
          refresh(),
          ignoreInit = TRUE
        )
      })
    }

    # Reactively hide the action buttons (and the audit table) whose permission
    # is FALSE, so the visible controls match the can_* permissions without
    # callers reaching into namespaced button ids. The server-side guards in the
    # individual observers stay in force regardless of visibility, so a hidden
    # control can never trigger its action.
    if (isTRUE(hide_forbidden)) {
      button_permissions <- list(
        open_add = can_add,
        open_edit = can_view_record,
        delete = can_delete,
        refresh_table = can_reset_table,
        open_deleted_records = can_view_deleted_records,
        open_column_selection = function() {
          sft_module_permission(can_select_column_view, default = TRUE) ||
            sft_module_permission(can_change_column_settings, default = TRUE)
        }
      )

      shiny::observe({
        for (button_id in names(button_permissions)) {
          shinyjs::toggle(
            id = button_id,
            condition = sft_module_permission(button_permissions[[button_id]], default = TRUE)
          )
        }

        shinyjs::toggle(
          id = "records_container",
          condition = sft_module_permission(can_view_table, default = TRUE)
        )

        if (isTRUE(show_audit)) {
          shinyjs::toggle(
            id = "audit_container",
            condition = sft_module_permission(can_view_audit, default = TRUE)
          )
        }
      })
    }

    # Show a warning notification for a UI label. The early return that usually
    # follows stays in the calling observer, since it must return from there.
    notify_warning <- function(label) {
      shiny::showNotification(
        sft_ui_label(labels, label),
        type = "warning"
      )
    }

    # Run a mutating CRUD action, then on success close the modal, notify, and
    # refresh the table; on error keep the modal open and show the error. Shared
    # by the add / edit / delete submit handlers.
    run_mutation <- function(action, success_label) {
      tryCatch(
        {
          action()

          shiny::removeModal()
          inline_active(NULL)
          shiny::showNotification(
            sft_ui_label(labels, success_label),
            type = "message"
          )

          refresh()
        },
        error = function(err) {
          shiny::showNotification(
            conditionMessage(err),
            type = "error",
            duration = 8
          )
        }
      )
    }

    records <- shiny::reactive({
      refresh_tick()

      fetch_records(
        form = form,
        conn = conn,
        include_deleted = isTRUE(
          sft_module_include_deleted(
            input = input,
            include_deleted_default = include_deleted_default
          )
        )
      )
    })

    display_context <- function() {
      sft_form_context(
        form = form,
        conn = conn,
        input = input,
        output = output,
        session = session,
        records = records,
        display_records = display_records,
        selected_record = selected_record,
        refresh = refresh,
        user = sft_module_current_user(input, user)
      )
    }

    display_records <- shiny::reactive({
      sft_apply_display_transform(
        data = records(),
        display_transform = display_transform,
        context = display_context()
      )
    })

    sft_register_input_bindings(
      input_bindings = input_bindings,
      form = form,
      input = input,
      session = session,
      context = display_context
    )

    if (!is.null(modal_header)) {
      output$add_modal_header <- shiny::renderUI({
        sft_render_modal_header(
          modal_header = modal_header,
          form = form,
          ns = session$ns,
          prefix = "add_",
          values = sft_modal_input_values(form = form, input = input, prefix = "add_"),
          record = NULL,
          context = display_context(),
          input = input,
          output = output,
          session = session
        )
      })

      output$edit_modal_header <- shiny::renderUI({
        row <- current_edit_row()

        sft_render_modal_header(
          modal_header = modal_header,
          form = form,
          ns = session$ns,
          prefix = "edit_",
          values = sft_modal_input_values(
            form = form,
            input = input,
            prefix = "edit_",
            record = row
          ),
          record = row,
          context = display_context(),
          input = input,
          output = output,
          session = session
        )
      })
    }


    selected_record <- shiny::reactive({
      sft_selected_record_from_display(
        display_data = display_records(),
        raw_data = records(),
        selected = input$records_rows_selected
      )
    })

    shiny::observeEvent(input$records_rows_selected, {
      row <- selected_record()

      if (!is.null(row) && "sft_id" %in% names(row)) {
        selected_record_id(row$sft_id[1])
      }
    }, ignoreNULL = TRUE)


    current_record_columns <- shiny::reactive({
      data <- display_records()

      user_id <- sft_preference_user_id(
        sft_module_current_user(input, user)
      )

      if (isTRUE(persist_column_settings) && !identical(record_columns_loaded_for(), user_id)) {
        view_name <- sft_get_active_column_view(
          conn = conn,
          form = form,
          user = user_id
        )

        saved_columns <- sft_resolve_saved_column_view(
          conn = conn,
          form = form,
          user = user_id,
          table_views = table_views,
          view_name = view_name
        )

        if (!is.null(saved_columns)) {
          set_record_columns(saved_columns)
          active_column_view(view_name)
        } else {
          fallback_view <- sft_column_view_key(
            sft_module_value(default_column_view, default = "Standard")
          )

          fallback_columns <- sft_resolve_saved_column_view(
            conn = conn,
            form = form,
            user = user_id,
            table_views = table_views,
            view_name = fallback_view
          )

          if (!is.null(fallback_columns)) {
            set_record_columns(fallback_columns)
            active_column_view(fallback_view)
          } else {
            legacy_columns <- sft_get_column_settings(
              conn = conn,
              form = form,
              user = user_id
            )

            if (!is.null(legacy_columns)) {
              set_record_columns(legacy_columns)
              active_column_view("Standard")
            } else {
              set_record_columns(table_columns)
              active_column_view(fallback_view)
            }
          }
        }

        record_columns_loaded_for(user_id)
      }

      sft_resolve_record_columns(
        form = form,
        data = data,
        columns = record_columns(),
        show_system_columns = show_system_columns
      )
    })

    column_choices <- shiny::reactive({
      sft_record_column_choices(
        form = form,
        data = display_records(),
        show_system_columns = show_system_columns,
        display_column_labels = display_column_labels
      )
    })

    shiny::observeEvent(input$include_deleted, {
      refresh()
    })

    shiny::observeEvent(input$open_add, {
      if (!sft_module_permission(can_add, default = TRUE)) {
        notify_warning("add_not_allowed")

        return()
      }

      if (identical(form_layout, "inline")) {
        inline_active("add")
      } else {
        sft_show_add_modal(
          form = sft_resolve_editable(form, sft_module_current_user(input, user)),
          session = session,
          labels = labels,
          modal_sizes = modal_sizes,
          modal_header = modal_header
        )
      }
    })

    shiny::observeEvent(input$submit_add, {
      if (!sft_module_permission(can_add, default = TRUE)) {
        shiny::removeModal()
        notify_warning("add_not_allowed")

        return()
      }

      run_mutation(
        function() {
          current_user <- sft_module_current_user(input, user)

          values <- collect_input_values(
            form = form,
            input = input,
            prefix = "add_"
          )

          # Server-side enforcement: a field locked for this user by a function
          # `editable` is dropped even though its disabled input still submits.
          locked_fields <- sft_user_locked_input_fields(form, current_user)
          if (length(locked_fields)) {
            values <- values[!names(values) %in% locked_fields]
          }

          # Drop the value of any field a dynamic_visibility() binding hides, so a
          # field that is not currently shown never persists a stale value.
          values <- sft_drop_hidden_field_values(input_bindings, form, values)

          insert_record(
            form = form,
            record = values,
            conn = conn,
            user = current_user
          )
        },
        "record_added"
      )
    })

    shiny::observeEvent(input$open_edit, {
      if (!sft_module_permission(can_view_record, default = TRUE)) {
        notify_warning("view_not_allowed")

        return()
      }

      row <- selected_record()

      if (is.null(row)) {
        notify_warning("no_selection")

        return()
      }

      if (sft_row_is_deleted(row)) {
        notify_warning("deleted_cannot_edit")

        return()
      }

      current_edit_row(row)
      restore_record_id(row$sft_id[1])

      if (identical(form_layout, "inline")) {
        inline_active("edit")
      } else {
        sft_show_edit_modal(
          form = sft_resolve_editable(form, sft_module_current_user(input, user)),
          session = session,
          row = row,
          labels = labels,
          modal_sizes = modal_sizes,
          modal_header = modal_header,
          datetime_format = datetime_format,
          can_edit = sft_module_permission(can_edit, default = TRUE),
          can_view_versions = sft_module_permission(can_view_versions, default = TRUE),
          can_restore = sft_module_permission(can_restore, default = TRUE),
          editable_fields = sft_module_value(editable_fields)
        )
      }
    })

    # Inline layout: render the active add/edit form into the panel above the
    # table. Reuses the shared modal bodies and the same submit_* ids, so the
    # submit/validation observers below are unchanged. The form is resolved for
    # the current user so per-user `editable` functions apply inline too.
    output$sft_inline_form <- shiny::renderUI({
      mode <- inline_active()

      if (is.null(mode)) {
        return(NULL)
      }

      current_user <- sft_module_current_user(input, user)

      if (identical(mode, "add")) {
        if (!sft_module_permission(can_add, default = TRUE)) {
          return(NULL)
        }

        return(
          sft_inline_form_panel(
            ns = session$ns,
            title = sft_ui_label(labels, "add_title"),
            body = sft_add_form_body(
              ns = session$ns,
              form = sft_resolve_editable(form, current_user),
              modal_header = modal_header
            ),
            submit_id = "submit_add",
            submit_label = "save",
            labels = labels
          )
        )
      }

      row <- current_edit_row()

      if (is.null(row)) {
        return(NULL)
      }

      can_edit_now <- sft_module_permission(can_edit, default = TRUE)

      sft_inline_form_panel(
        ns = session$ns,
        title = sft_ui_label(labels, "edit_title", values = list(id = row$sft_easy_id[1])),
        body = sft_edit_form_body(
          ns = session$ns,
          form = sft_resolve_editable(form, current_user),
          row = row,
          labels = labels,
          datetime_format = datetime_format,
          can_edit = can_edit_now,
          can_view_versions = sft_module_permission(can_view_versions, default = TRUE),
          can_restore = sft_module_permission(can_restore, default = TRUE),
          editable_fields = sft_module_value(editable_fields),
          modal_header = modal_header
        ),
        submit_id = "submit_edit",
        submit_label = "update",
        labels = labels,
        can_submit = can_edit_now,
        cancel_label = if (isTRUE(can_edit_now)) "cancel" else "close"
      )
    })

    shiny::observeEvent(input$sft_inline_cancel, {
      inline_active(NULL)
    })

    shiny::observeEvent(input$submit_edit, {
      if (!sft_module_permission(can_edit, default = TRUE)) {
        shiny::removeModal()
        notify_warning("edit_not_allowed")

        return()
      }

      row <- current_edit_row()

      if (is.null(row)) {
        notify_warning("no_selection")

        return()
      }

      run_mutation(
        function() {
          current_user <- sft_module_current_user(input, user)

          # Resolve per-user field editability to plain logicals, so editable_only
          # below (and inside update_record) drops fields this user may not edit.
          resolved_form <- sft_resolve_editable(form, current_user)

          values <- collect_input_values(
            form = resolved_form,
            input = input,
            prefix = "edit_",
            editable_only = TRUE
          )

          # Server-side enforcement of per-user field editing: drop any field the
          # user is not allowed to edit, so a disabled input cannot be saved.
          allowed_fields <- sft_module_value(editable_fields)
          if (!is.null(allowed_fields)) {
            values <- values[names(values) %in% allowed_fields]
          }

          # Drop the value of any field a dynamic_visibility() binding hides, so a
          # field that is not currently shown never persists a stale value.
          values <- sft_drop_hidden_field_values(input_bindings, resolved_form, values)

          update_record(
            form = resolved_form,
            record_id = row$sft_id[1],
            values = values,
            conn = conn,
            user = current_user
          )
        },
        "record_updated"
      )
    })

    shiny::observeEvent(input$delete, {
      row <- selected_record()

      if (!sft_module_permission(can_delete, default = TRUE)) {
        notify_warning("delete_not_allowed")

        return()
      }

      if (is.null(row)) {
        notify_warning("no_selection")

        return()
      }

      if (sft_row_is_deleted(row)) {
        notify_warning("already_deleted")

        return()
      }

      sft_show_delete_modal(
        session = session,
        row = row,
        labels = labels,
        modal_sizes = modal_sizes
      )
    })

    shiny::observeEvent(input$confirm_delete, {
      row <- selected_record()

      if (!sft_module_permission(can_delete, default = TRUE)) {
        shiny::removeModal()
        notify_warning("delete_not_allowed")

        return()
      }

      if (is.null(row)) {
        shiny::removeModal()
        notify_warning("no_valid_selection")

        return()
      }

      run_mutation(
        function() {
          soft_delete_record(
            form = form,
            record_id = row$sft_id[1],
            conn = conn,
            user = sft_module_current_user(input, user)
          )
        },
        "record_deleted"
      )
    })

    sft_register_deleted_versions(
      input = input,
      output = output,
      session = session,
      form = form,
      conn = conn,
      user = user,
      labels = labels,
      modal_sizes = modal_sizes,
      datetime_format = datetime_format,
      display_transform = display_transform,
      display_column_labels = display_column_labels,
      show_system_columns = show_system_columns,
      deleted_records_options = deleted_records_options,
      version_options = version_options,
      can_view_deleted_records = can_view_deleted_records,
      can_view_versions = can_view_versions,
      can_restore = can_restore,
      refresh = refresh,
      refresh_tick = refresh_tick,
      restore_record_id = restore_record_id,
      selected_record = selected_record,
      display_context = display_context,
      current_record_columns = current_record_columns
    )

    sft_register_column_settings(
      input = input,
      output = output,
      session = session,
      form = form,
      conn = conn,
      user = user,
      labels = labels,
      modal_sizes = modal_sizes,
      table_views = table_views,
      persist_column_settings = persist_column_settings,
      show_system_columns = show_system_columns,
      can_change_column_settings = can_change_column_settings,
      can_select_column_view = can_select_column_view,
      display_records = display_records,
      column_choices = column_choices,
      current_record_columns = current_record_columns,
      active_column_view = active_column_view,
      set_record_columns = set_record_columns
    )

    shiny::observeEvent(input$refresh_table, {
      if (!sft_module_permission(can_reset_table, default = TRUE)) {
        notify_warning("reset_not_allowed")

        return()
      }

      selected_record_id(NULL)

      session$sendCustomMessage(
        type = "sftResetDataTable",
        message = list(id = session$ns("records"))
      )

      shiny::showNotification(
        sft_ui_label(labels, "table_refreshed"),
        type = "message"
      )
    })

    output$records <- DT::renderDT({
      table_structure_tick()

      if (!sft_module_permission(can_view_table, default = TRUE)) {
        return(NULL)
      }

      records_data <- shiny::isolate(display_records())
      records_columns <- shiny::isolate(current_record_columns())
      records_table <- records_datatable(
        data = records_data,
        form = form,
        columns = records_columns,
        show_system_columns = show_system_columns,
        selection = "single",
        options = table_options,
        datetime_format = datetime_format,
        display_column_labels = display_column_labels,
        class = table_class,
        filter = table_filter
      )

      sft_apply_table_format(
        table = records_table,
        data = records_data,
        table_format = table_format,
        context = display_context()
      )
    })

    records_proxy <- DT::dataTableProxy("records", session = session)

    shiny::observeEvent(
      list(refresh_tick(), current_record_columns()),
      {
        records_data <- display_records()
        records_columns <- current_record_columns()
        replacement_data <- sft_records_table_data(
          data = records_data,
          form = form,
          columns = records_columns,
          show_system_columns = show_system_columns,
          datetime_format = datetime_format,
          display_column_labels = display_column_labels
        )

        try(
          DT::replaceData(
            proxy = records_proxy,
            data = replacement_data,
            resetPaging = FALSE,
            rownames = FALSE
          ),
          silent = TRUE
        )

        if (!is.null(selected_record_id()) && "sft_id" %in% names(records_data)) {
          row_index <- which(records_data$sft_id == selected_record_id())[1]

          if (!is.na(row_index)) {
            try(DT::selectRows(records_proxy, row_index), silent = TRUE)
          }
        }
      },
      ignoreInit = TRUE
    )

    if (isTRUE(show_audit)) {
      output$audit <- DT::renderDT({
        if (!sft_module_permission(can_view_audit, default = TRUE)) {
          return(NULL)
        }

        row <- selected_record()

        audit <- if (is.null(row)) {
          fetch_audit_log(
            form = form,
            conn = conn
          )
        } else {
          fetch_audit_log(
            form = form,
            conn = conn,
            record_id = row$sft_id[1]
          )
        }

        audit_datatable(
          data = audit,
          options = audit_options,
          datetime_format = datetime_format
        )
      })
    }

    list(
      records = records,
      display_records = display_records,
      selected_record = selected_record,
      changed = shiny::reactive(refresh_tick()),
      refresh = refresh,
      conn = conn,
      form = form
    )
  })
}
