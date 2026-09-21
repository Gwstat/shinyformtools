# Records and audit table rendering for the form module. Extracted verbatim
# from form_server(), mirroring sft_register_deleted_versions() and
# sft_register_column_settings(): a non-namespaced registrar called from the
# module server (so it is exercised by form_server's tests). It renders the
# records DataTable, keeps it in sync via a proxy on every mutation (re-selecting
# the active row), and registers the audit table render.
sft_register_records_table <- function(input, output, session, state) {
  form <- state$form
  language <- state$language
  live_conn <- state$conn
  show_system_columns <- state$columns$show_system
  table_options <- state$table$options
  table_class <- state$table$class
  table_filter <- state$table$filter
  table_format <- state$table$format
  audit_options <- state$table$audit_options
  datetime_format <- state$table$datetime_format
  display_column_labels <- state$columns$labels
  can_view_table <- state$permissions$can_view_table
  can_view_audit <- state$permissions$can_view_audit
  table_structure_tick <- state$table_structure_tick
  refresh_tick <- state$refresh_tick
  display_records <- state$display_records
  selected_record <- state$selected_record
  selected_record_id <- state$selected_record_id
  display_context <- state$display_context
  # exposed by sft_register_column_settings(); read lazily
  current_record_columns <- function() state$current_record_columns()

  # Remember the selected sft_id so the table can re-select the row after a
  # refresh. Deselection delivers NULL: clear the id, so a later refresh does
  # not re-select the row the user just deselected.
  shiny::observeEvent(input$records_rows_selected, {
    row <- selected_record()

    if (!is.null(row) && "sft_id" %in% names(row)) {
      selected_record_id(row$sft_id[1])
    } else {
      selected_record_id(NULL)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$records <- DT::renderDT(sft_with_language(language, {
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
  }))

  records_proxy <- DT::dataTableProxy("records", session = session)

  # Keeps the rendered table in sync through the DT proxy. A read that fails
  # (connection refused) is reported by the table output itself, so this
  # observer stays quiet - but it must stay ALIVE: an error escaping it would
  # end the session on the next refresh.
  shiny::observeEvent(
    list(refresh_tick(), tryCatch(current_record_columns(), error = function(err) NULL)),
    tryCatch({
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
    }, error = function(err) NULL),
    ignoreInit = TRUE
  )

  # The audit output is always registered: Shiny computes it only when
  # form_ui(show_audit = TRUE) placed the container, so no server flag is needed.
  output$audit <- DT::renderDT(sft_with_language(language, {
    if (!sft_module_permission(can_view_audit, default = TRUE)) {
      return(NULL)
    }

    row <- selected_record()

    audit <- if (is.null(row)) {
      fetch_audit_log(
        form = form,
        conn = live_conn()
      )
    } else {
      fetch_audit_log(
        form = form,
        conn = live_conn(),
        record_id = row$sft_id[1]
      )
    }

    audit_datatable(
      data = audit,
      options = audit_options,
      datetime_format = datetime_format
    )
  }))

  invisible(list())
}
