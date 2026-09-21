# Deleted-records and version/restore flow for the form module.
#
# Registers the reactives, table renderers, and observers that power the
# "deleted records" and "versions / restore" dialogs. This is NOT a namespaced
# Shiny module: it is called from within form_server() with that module's
# own input/output/session, so all input ids and outputs stay in the parent
# namespace (behaviour-preserving). Shared state created by the parent
# (refresh, refresh_tick, restore_record_id, selected_record, display_context,
# current_record_columns) is threaded in explicitly rather than captured.
sft_register_deleted_versions <- function(input, output, session, state) {
  form <- state$form
  live_conn <- state$conn
  user <- state$user
  # Live binding, not a copy: the labels follow a language that changes.
  makeActiveBinding("labels", function() state$labels, environment())
  language <- state$language
  modal_sizes <- state$modal_sizes
  datetime_format <- state$table$datetime_format
  display_transform <- state$display_transform
  display_column_labels <- state$columns$labels
  show_system_columns <- state$columns$show_system
  deleted_records_options <- state$table$deleted_records_options
  version_options <- state$table$version_options
  can_view_deleted_records <- state$permissions$can_view_deleted_records
  can_view_versions <- state$permissions$can_view_versions
  can_restore <- state$permissions$can_restore
  refresh_tick <- state$refresh_tick
  restore_record_id <- state$restore_record_id
  selected_record <- state$selected_record
  display_context <- state$display_context
  # exposed by sft_register_column_settings(); read lazily
  current_record_columns <- function() state$current_record_columns()
  deleted_records <- shiny::reactive({
    refresh_tick()

    # Permission gate on the data source, not only on the dialog-opening
    # observer: outputs compute once something subscribes, so a client-injected
    # output binding must never receive rows the permission forbids.
    if (!sft_module_permission(can_view_deleted_records, default = TRUE)) {
      return(data.frame())
    }

    data <- fetch_records(
      form = form,
      conn = live_conn(),
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

    # Same gate as deleted_records above: restore_record_id is already set by
    # merely opening the edit dialog (can_view_record), so without this check a
    # client-injected output binding would expose the full version history.
    if (!sft_module_permission(can_view_versions, default = TRUE)) {
      return(data.frame())
    }

    if (is.null(record_id)) {
      return(data.frame())
    }

    list_restorable_versions(
      form = form,
      conn = live_conn(),
      record_id = record_id
    )
  })

  output$deleted_records <- DT::renderDT(sft_with_language(language, {
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
  }))

  output$restore_versions <- DT::renderDT(sft_with_language(language, {
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
  }))

  shiny::observeEvent(input$open_deleted_records, {
    state$guard(function() {
      if (!sft_module_permission(can_view_deleted_records, default = TRUE)) {
        state$notify("deleted_records_not_allowed")

        return()
      }

      sft_show_deleted_records_modal(
        session = session,
        labels = labels,
        modal_sizes = modal_sizes,
        can_restore = sft_module_permission(can_restore, default = TRUE)
      )
    })
  })

  # One-click restore of a deleted record: reactivates it from its latest
  # (pre-deletion) version. Restoring a specific older version is done from the
  # versions accordion in the view-case dialog, not here.
  shiny::observeEvent(input$restore_deleted, {
    # Guarded: reading the selection can hit a failing connection.
    state$guard(function() {
      if (!sft_module_permission(can_restore, default = TRUE)) {
        state$notify("restore_not_allowed")

        return()
      }

      row <- selected_deleted_record()

      if (is.null(row)) {
        state$notify("no_selection")

        return()
      }

      # Same wrapper as add / edit / delete: heals a dropped connection first,
      # shows validation warnings, keeps the dialog open on error.
      state$run_mutation(
        function() {
          restore_record(
            form = form,
            record_id = row$sft_id[1],
            conn = live_conn(),
            user = sft_module_current_user(input, user),
            reason = "Restored latest version via deleted-records dialog."
          )
        },
        "record_restored"
      )
    })
  })

  shiny::observeEvent(input$open_versions, {
    # Guarded: reading the selection can hit a failing connection.
    state$guard(function() {
      if (!sft_module_permission(can_view_versions, default = TRUE)) {
        state$notify("versions_not_allowed")

        return()
      }

      row <- selected_record()

      if (is.null(row)) {
        state$notify("no_selection")

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
  })

  shiny::observeEvent(input$confirm_restore, {
    # Guarded: reading the selection can hit a failing connection.
    state$guard(function() {
      record_id <- restore_record_id()

      if (!sft_module_permission(can_restore, default = TRUE)) {
        shiny::removeModal()
        state$notify("restore_not_allowed")

        return()
      }

      if (is.null(record_id)) {
        shiny::removeModal()
        state$notify("no_valid_record_selection")

        return()
      }

      selected_version_row <- input$restore_versions_rows_selected

      if (is.null(selected_version_row) || length(selected_version_row) != 1L) {
        state$notify("choose_version")

        return()
      }

      versions <- restore_versions()

      if (nrow(versions) == 0L || selected_version_row > nrow(versions)) {
        state$notify("version_unavailable")

        return()
      }

      version_no <- versions$version_no[selected_version_row]

      state$run_mutation(
        function() {
          restore_record(
            form = form,
            record_id = record_id,
            version_no = version_no,
            conn = live_conn(),
            user = sft_module_current_user(input, user),
            reason = paste0(
              "Restored via module dialog from version ",
              version_no,
              "."
            )
          )
        },
        "version_restored",
        success_values = list(version = version_no),
        on_success = function() restore_record_id(NULL)
      )
    })
  })

  invisible(list(
    deleted_records = deleted_records,
    restore_versions = restore_versions
  ))
}
