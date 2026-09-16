# The add / edit / delete flows of the form module, plus the reset button and
# the include-deleted checkbox. Non-namespaced registrar called from
# form_server(); reads everything from the module state (mod_state.R) and
# exposes nothing. The add and edit bodies themselves live in mod_modal.R
# (dialog layout) and mod_inline.R (panel layout); this file owns the
# observers and the server-side enforcement around a submit.

sft_register_crud <- function(input, output, session, state) {
  form <- state$form
  labels <- state$labels
  modal_sizes <- state$modal_sizes
  modal_header <- state$modal_header
  input_bindings <- state$input_bindings
  editable_fields <- state$permissions$editable_fields
  datetime_format <- state$table$datetime_format
  notify_warning <- state$notify
  run_mutation <- state$run_mutation

  shiny::observeEvent(input$include_deleted, {
    state$refresh()
  })

  shiny::observeEvent(input$open_add, {
    if (!state$permission("can_add")) {
      notify_warning("add_not_allowed")

      return()
    }

    if (identical(state$layout(), "inline")) {
      state$inline_active("add")
    } else {
      sft_show_add_modal(
        form = sft_resolve_editable(form, state$current_user()),
        session = session,
        labels = labels,
        modal_sizes = modal_sizes,
        modal_header = modal_header
      )
    }
  })

  shiny::observeEvent(input$submit_add, {
    if (!state$permission("can_add")) {
      shiny::removeModal()
      notify_warning("add_not_allowed")

      return()
    }

    run_mutation(
      function() {
        current_user <- state$current_user()

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

        # Statically non-editable fields render disabled, which only the
        # client enforces. Derived fields (dynamic_value bindings) are
        # recomputed server-side from the other values; every other locked
        # field falls back to its declared default (assigning NULL drops
        # fields without one), so no locked value comes from the client.
        static_defaults <- sft_static_locked_input_defaults(form)
        derived_fields <- intersect(
          names(static_defaults),
          sft_value_binding_fields(input_bindings)
        )
        for (field_id in setdiff(names(static_defaults), derived_fields)) {
          values[[field_id]] <- static_defaults[[field_id]]
        }
        values <- sft_recompute_locked_value_bindings(
          input_bindings = input_bindings,
          form = form,
          values = values,
          field_ids = derived_fields
        )

        # Drop the value of any field a dynamic_visibility() binding hides, so a
        # field that is not currently shown never persists a stale value.
        values <- sft_drop_hidden_field_values(input_bindings, form, values)

        insert_record(
          form = form,
          record = values,
          conn = state$conn(),
          user = current_user
        )
      },
      "record_added"
    )
  })

  shiny::observeEvent(input$open_edit, {
    if (!state$permission("can_view_record")) {
      notify_warning("view_not_allowed")

      return()
    }

    row <- state$selected_record()

    if (is.null(row)) {
      notify_warning("no_selection")

      return()
    }

    if (sft_row_is_deleted(row)) {
      notify_warning("deleted_cannot_edit")

      return()
    }

    state$current_edit_row(row)
    state$edit_conflict(NULL)
    state$edit_conflict_baseline(row)
    state$restore_record_id(row$sft_id[1])

    if (identical(state$layout(), "inline")) {
      state$inline_active("edit")
    } else {
      sft_show_edit_modal(
        form = sft_resolve_editable(form, state$current_user()),
        session = session,
        row = row,
        labels = labels,
        modal_sizes = modal_sizes,
        modal_header = modal_header,
        datetime_format = datetime_format,
        can_edit = state$permission("can_edit"),
        can_view_versions = state$permission("can_view_versions"),
        can_restore = state$permission("can_restore"),
        editable_fields = sft_module_editable_fields(editable_fields)
      )
    }
  })

  shiny::observeEvent(input$submit_edit, {
    if (!state$permission("can_edit")) {
      shiny::removeModal()
      notify_warning("edit_not_allowed")

      return()
    }

    row <- state$current_edit_row()

    if (is.null(row)) {
      notify_warning("no_selection")

      return()
    }

    run_mutation(
      function() {
        current_user <- state$current_user()

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
        allowed_fields <- sft_module_editable_fields(editable_fields)
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
          conn = state$conn(),
          user = current_user,
          expected_record = if (isTRUE(state$conflict_check)) {
            state$edit_conflict_baseline()
          }
        )
      },
      "record_updated"
    )
  })

  shiny::observeEvent(input$delete, {
    if (!state$permission("can_delete")) {
      notify_warning("delete_not_allowed")

      return()
    }

    row <- state$selected_record()

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
    if (!state$permission("can_delete")) {
      shiny::removeModal()
      notify_warning("delete_not_allowed")

      return()
    }

    row <- state$selected_record()

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
          conn = state$conn(),
          user = state$current_user()
        )
      },
      "record_deleted"
    )
  })

  shiny::observeEvent(input$refresh_table, {
    if (!state$permission("can_reset_table")) {
      notify_warning("reset_not_allowed")

      return()
    }

    state$selected_record_id(NULL)

    session$sendCustomMessage(
      type = "sftResetDataTable",
      message = list(id = session$ns("records"))
    )

    shiny::showNotification(
      sft_ui_label(labels, "table_refreshed"),
      type = "message"
    )
  })

  invisible(list())
}
