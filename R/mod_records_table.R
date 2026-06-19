# Records and audit table rendering for the form module. Extracted verbatim
# from form_server(), mirroring sft_register_deleted_versions() and
# sft_register_column_settings(): a non-namespaced registrar called from the
# module server (so it is exercised by form_server's tests). It renders the
# records DataTable, keeps it in sync via a proxy on every mutation (re-selecting
# the active row), and renders the audit table when show_audit is TRUE.
sft_register_records_table <- function(input,
                                       output,
                                       session,
                                       form,
                                       conn,
                                       show_audit,
                                       show_system_columns,
                                       table_options,
                                       table_class,
                                       table_filter,
                                       table_format,
                                       audit_options,
                                       datetime_format,
                                       display_column_labels,
                                       can_view_table,
                                       can_view_audit,
                                       table_structure_tick,
                                       refresh_tick,
                                       display_records,
                                       current_record_columns,
                                       selected_record,
                                       selected_record_id,
                                       display_context) {
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

  invisible(NULL)
}
