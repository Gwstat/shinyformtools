# Deleted-records and version/restore flow for the form module.
#
# Registers the reactives, table renderers, and observers that power the
# "deleted records" and "versions / restore" dialogs. This is NOT a namespaced
# Shiny module: it is called from within form_server() with that module's
# own input/output/session, so all input ids and outputs stay in the parent
# namespace (behaviour-preserving). Shared state created by the parent
# (refresh, refresh_tick, restore_record_id, selected_record, display_context,
# current_record_columns) is threaded in explicitly rather than captured.
sft_register_deleted_versions <- function(input,
                                           output,
                                           session,
                                           form,
                                           conn,
                                           user,
                                           labels,
                                           modal_sizes,
                                           datetime_format,
                                           display_transform,
                                           display_column_labels,
                                           show_system_columns,
                                           deleted_records_options,
                                           version_options,
                                           can_view_deleted_records,
                                           can_view_versions,
                                           can_restore,
                                           refresh,
                                           refresh_tick,
                                           restore_record_id,
                                           selected_record,
                                           display_context,
                                           current_record_columns) {
  deleted_records <- shiny::reactive({
    refresh_tick()

    data <- fetch_records(
      form = form,
      conn = conn,
      include_deleted = TRUE
    )

    if (!is.data.frame(data) || nrow(data) == 0L || !"sft_is_deleted" %in% names(data)) {
      return(data.frame())
    }

    data[data$sft_is_deleted == 1L, , drop = FALSE]
  })

  display_deleted_records <- shiny::reactive({
    sft_apply_display_transform(
      data = deleted_records(),
      display_transform = display_transform,
      context = display_context()
    )
  })

  selected_deleted_record <- shiny::reactive({
    sft_selected_record_from_display(
      display_data = display_deleted_records(),
      raw_data = deleted_records(),
      selected = input$deleted_records_rows_selected
    )
  })

  restore_versions <- shiny::reactive({
    refresh_tick()
    record_id <- restore_record_id()

    if (is.null(record_id)) {
      return(data.frame())
    }

    list_restorable_versions(
      form = form,
      conn = conn,
      record_id = record_id
    )
  })

  output$deleted_records <- DT::renderDT({
    data <- display_deleted_records()

    if (nrow(data) == 0L) {
      return(
        DT::datatable(
          data.frame(
            Note = sft_ui_label(labels, "deleted_records_empty")
          ),
          rownames = FALSE,
          selection = "none"
        )
      )
    }

    records_datatable(
      data = data,
      form = form,
      columns = current_record_columns(),
      show_system_columns = show_system_columns,
      selection = "single",
      options = deleted_records_options,
      datetime_format = datetime_format,
      display_column_labels = display_column_labels
    )
  })

  output$restore_versions <- DT::renderDT({
    versions <- restore_versions()

    if (nrow(versions) == 0L) {
      return(
        DT::datatable(
          data.frame(
            Note = sft_ui_label(labels, "no_versions")
          ),
          rownames = FALSE,
          selection = "none"
        )
      )
    }

    sft_versions_datatable(
      data = versions,
      form = form,
      selection = "single",
      options = version_options,
      datetime_format = datetime_format
    )
  })

  shiny::observeEvent(input$open_deleted_records, {
    if (!sft_module_permission(can_view_deleted_records, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "deleted_records_not_allowed"),
        type = "warning"
      )

      return()
    }

    sft_show_deleted_records_modal(
      session = session,
      labels = labels,
      modal_sizes = modal_sizes,
      can_restore = sft_module_permission(can_restore, default = TRUE)
    )
  })

  # One-click restore of a deleted record: reactivates it from its latest
  # (pre-deletion) version. Restoring a specific older version is done from the
  # versions accordion in the view-case dialog, not here.
  shiny::observeEvent(input$restore_deleted, {
    if (!sft_module_permission(can_restore, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "restore_not_allowed"),
        type = "warning"
      )

      return()
    }

    row <- selected_deleted_record()

    if (is.null(row)) {
      shiny::showNotification(
        sft_ui_label(labels, "no_selection"),
        type = "warning"
      )

      return()
    }

    tryCatch(
      {
        restore_record(
          form = form,
          record_id = row$sft_id[1],
          conn = conn,
          user = sft_module_current_user(input, user),
          reason = "Restored latest version via deleted-records dialog."
        )

        shiny::removeModal()
        shiny::showNotification(
          sft_ui_label(labels, "record_restored"),
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
  })

  shiny::observeEvent(input$open_versions, {
    if (!sft_module_permission(can_view_versions, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "versions_not_allowed"),
        type = "warning"
      )

      return()
    }

    row <- selected_record()

    if (is.null(row)) {
      shiny::showNotification(
        sft_ui_label(labels, "no_selection"),
        type = "warning"
      )

      return()
    }

    restore_record_id(NULL)
    restore_record_id(row$sft_id[1])

    sft_show_versions_modal(
      session = session,
      row = row,
      labels = labels,
      modal_sizes = modal_sizes,
      can_restore = sft_module_permission(can_restore, default = TRUE)
    )
  })

  shiny::observeEvent(input$confirm_restore, {
    record_id <- restore_record_id()

    if (!sft_module_permission(can_restore, default = TRUE)) {
      shiny::removeModal()
      shiny::showNotification(
        sft_ui_label(labels, "restore_not_allowed"),
        type = "warning"
      )

      return()
    }

    if (is.null(record_id)) {
      shiny::removeModal()
      shiny::showNotification(
        sft_ui_label(labels, "no_valid_record_selection"),
        type = "warning"
      )

      return()
    }

    selected_version_row <- input$restore_versions_rows_selected

    if (is.null(selected_version_row) || length(selected_version_row) != 1L) {
      shiny::showNotification(
        sft_ui_label(labels, "choose_version"),
        type = "warning"
      )

      return()
    }

    versions <- restore_versions()

    if (nrow(versions) == 0L || selected_version_row > nrow(versions)) {
      shiny::showNotification(
        sft_ui_label(labels, "version_unavailable"),
        type = "warning"
      )

      return()
    }

    version_no <- versions$version_no[selected_version_row]

    tryCatch(
      {
        restore_record(
          form = form,
          record_id = record_id,
          version_no = version_no,
          conn = conn,
          user = sft_module_current_user(input, user),
          reason = paste0(
            "Restored via module dialog from version ",
            version_no,
            "."
          )
        )

        shiny::removeModal()
        shiny::showNotification(
          sft_ui_label(
            labels = labels,
            key = "version_restored",
            values = list(version = version_no)
          ),
          type = "message"
        )

        restore_record_id(NULL)
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
  })

  invisible(NULL)
}
