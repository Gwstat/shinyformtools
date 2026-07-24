# Edit-conflict view for the form module.
#
# When update_record() rejects a stale edit (another user saved while the edit
# dialog was open), the module swaps the dialog body for a conflict view: what
# changed, who changed it last, and three choices - apply their changes to the
# inputs, keep the own entries, or go back. The regular form body is only
# hidden via CSS (see sft_edit_form_body), never rebuilt, so the user's typed
# values survive the switch in both modal and inline layout.

# CSS that hides the regular edit body and its submit buttons while the
# conflict view is shown. Rendered as part of the conflict view itself, so it
# disappears (and the form reappears) as soon as the conflict is resolved.
sft_conflict_hide_css <- function(ns) {
  paste0(
    "#", ns("sft_edit_form_main"), " { display: none; }\n",
    "#shiny-modal .modal-footer { display: none; }\n",
    ".sft-inline-form .sft-inline-form-actions { display: none; }\n",
    ".sft-edit-conflict { border: 1px solid #e0a800; background: #fff8e6; ",
    "border-radius: 4px; padding: 12px 16px; margin-top: 8px; }\n",
    ".sft-edit-conflict-actions { margin-top: 12px; text-align: right; }\n",
    ".sft-edit-conflict-actions .btn { margin-left: 6px; }"
  )
}

# Format a stored value for the conflict table. Falls back to the raw value on
# formatting errors and shows an en dash for empty values.
sft_conflict_display_value <- function(field, value) {
  formatted <- tryCatch(
    sft_format_field_display_value(field, value),
    error = function(err) value
  )

  formatted <- as.character(formatted %||% "")

  if (length(formatted) == 0L || is.na(formatted[1]) || !nzchar(formatted[1])) {
    return("\u2013")
  }

  paste(formatted, collapse = "; ")
}

# One display row per conflicting column: field label, the value when editing
# started, the value now stored, and whether the user's own (unsaved) input
# also differs from the baseline - i.e. a genuine both-sides conflict.
sft_conflict_rows <- function(form, baseline_record, current_record, columns, input) {
  baseline <- sft_row_to_list(baseline_record)
  current <- sft_row_to_list(current_record)

  rows <- list()

  for (field in sft_active_input_fields(form)) {
    column <- field$db_column

    if (!column %in% columns) {
      next
    }

    input_value <- input[[paste0("edit_", field$id)]]
    mine <- tryCatch(
      sft_values_differ(
        sft_field_db_value(field, input_value),
        baseline[[column]]
      ),
      error = function(err) FALSE
    )

    rows[[length(rows) + 1L]] <- list(
      column = column,
      label = field$label %||% field$id,
      old = sft_conflict_display_value(field, baseline[[column]]),
      now = sft_conflict_display_value(field, current[[column]]),
      mine = isTRUE(mine)
    )
  }

  rows
}

# Who changed which column since the baseline, derived from the audit log
# (changed_fields_json / changed_by per version, ascending, so the last writer
# per column wins). Falls back to the row's sft_updated_by when the audit log
# is not readable - attribution is display-only, never load-bearing.
sft_conflict_changes_meta <- function(conn, form, record_id, since, fallback_user) {
  by_column <- character()
  users <- character()

  audit <- tryCatch(
    fetch_audit_log(form = form, conn = conn, record_id = record_id),
    error = function(err) NULL
  )

  if (!is.null(audit) && nrow(audit) > 0L && !is.null(since) && !is.na(since)) {
    audit <- audit[order(audit$version_no), , drop = FALSE]

    # A version is "newer than the baseline" when the sft_updated_at stored in
    # its snapshot is greater than the baseline's. The snapshot value is the
    # exact stored timestamp, so this cleanly excludes the version that
    # produced the baseline itself (whose audit changed_at is written a few
    # milliseconds later and would pass a changed_at comparison).
    snapshot_updated_at <- function(i) {
      new_data <- tryCatch(
        jsonlite::fromJSON(audit$new_data_json[i]),
        error = function(err) NULL
      )

      value <- as.character(new_data[["sft_updated_at"]] %||% NA_character_)[1]

      if (is.na(value)) audit$changed_at[i] else value
    }

    newer <- vapply(
      seq_len(nrow(audit)),
      function(i) {
        stamp <- snapshot_updated_at(i)
        !is.na(stamp) && stamp > since
      },
      logical(1)
    )
    audit <- audit[newer, , drop = FALSE]

    for (i in seq_len(nrow(audit))) {
      changed_by <- audit$changed_by[i]

      if (is.na(changed_by) || !nzchar(changed_by)) {
        next
      }

      changed_fields <- tryCatch(
        sft_parse_json_vector(audit$changed_fields_json[i]),
        error = function(err) character()
      )

      for (column in changed_fields) {
        by_column[[column]] <- changed_by
      }

      users <- c(users, changed_by)
    }
  }

  if (length(users) == 0L) {
    fallback_user <- as.character(fallback_user %||% "")

    if (length(fallback_user) > 0L && !is.na(fallback_user[1]) && nzchar(fallback_user[1])) {
      users <- fallback_user[1]
    }
  }

  list(by_column = by_column, users = unique(users))
}

# Push a stored value into an edit input. Choices-based inputs update their
# selection; everything else goes through sft_update_value_input. Errors are
# swallowed: an input type without an update function simply keeps its value.
sft_conflict_set_input <- function(session, field, value) {
  input_id <- paste0("edit_", field$id)
  ui_value <- sft_ui_value(field, value)

  update_selected <- function(fun) {
    do.call(
      fun,
      list(
        session = session,
        inputId = input_id,
        selected = ui_value %||% character(0)
      )
    )
  }

  tryCatch(
    switch(
      field$input_type,
      selectInput = update_selected(shiny::updateSelectInput),
      selectizeInput = update_selected(shiny::updateSelectizeInput),
      radioButtons = update_selected(shiny::updateRadioButtons),
      checkboxGroupInput = update_selected(shiny::updateCheckboxGroupInput),
      multiInput = update_selected(shinyWidgets::updateMultiInput),
      checkboxInput = shiny::updateCheckboxInput(
        session = session,
        inputId = input_id,
        value = isTRUE(ui_value)
      ),
      sft_update_value_input(
        session = session,
        input_type = field$input_type,
        input_id = input_id,
        value = ui_value %||% ""
      )
    ),
    error = function(err) NULL
  )

  invisible(NULL)
}

# Register the conflict view output and its three action observers on the form
# module's session. Not namespaced itself; called from form_server() with the
# module's input/output/session, like the other sft_register_* helpers.
sft_register_edit_conflict <- function(input,
                                       output,
                                       session,
                                       form,
                                       labels,
                                       conn,
                                       edit_conflict,
                                       edit_conflict_baseline) {
  output$sft_edit_conflict_ui <- shiny::renderUI({
    conflict <- edit_conflict()

    if (is.null(conflict)) {
      return(NULL)
    }

    baseline <- edit_conflict_baseline()
    ns <- session$ns

    meta <- sft_conflict_changes_meta(
      conn = conn,
      form = form,
      record_id = conflict$current_record$sft_id[1],
      since = baseline$sft_updated_at[1],
      fallback_user = conflict$current_record$sft_updated_by[1]
    )

    rows <- sft_conflict_rows(
      form = form,
      baseline_record = baseline,
      current_record = conflict$current_record,
      columns = conflict$columns,
      input = input
    )

    any_mine <- any(vapply(rows, function(row) row$mine, logical(1)))

    by_column_lookup <- meta$by_column
    changed_by_label <- function(column) {
      value <- unname(by_column_lookup[column])
      if (length(value) == 0L || is.na(value)) "" else value
    }

    table_rows <- lapply(rows, function(row) {
      shiny::tags$tr(
        shiny::tags$td(
          row$label,
          if (row$mine) shiny::tags$strong(" \u26a0")
        ),
        shiny::tags$td(row$old),
        shiny::tags$td(row$now),
        shiny::tags$td(changed_by_label(row$column))
      )
    })

    shiny::tagList(
      shiny::tags$style(shiny::HTML(sft_conflict_hide_css(ns))),
      shiny::div(
        class = "sft-edit-conflict",
        role = "alert",
        shiny::tags$h4(sft_ui_label(labels, "conflict_title")),
        shiny::p(
          sft_ui_label(
            labels,
            "conflict_intro",
            values = list(users = paste(meta$users, collapse = ", "))
          )
        ),
        shiny::tags$table(
          class = "table table-condensed sft-edit-conflict-table",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th(sft_ui_label(labels, "conflict_field")),
              shiny::tags$th(sft_ui_label(labels, "conflict_opened")),
              shiny::tags$th(sft_ui_label(labels, "conflict_now")),
              shiny::tags$th(sft_ui_label(labels, "conflict_changed_by"))
            )
          ),
          shiny::tags$tbody(table_rows)
        ),
        if (any_mine) {
          shiny::p(
            shiny::tags$small(
              paste0("\u26a0 ", sft_ui_label(labels, "conflict_also_mine"))
            )
          )
        },
        shiny::div(
          class = "sft-edit-conflict-actions",
          shiny::actionButton(
            ns("sft_conflict_accept"),
            sft_ui_label(labels, "conflict_accept"),
            class = "btn-default"
          ),
          shiny::actionButton(
            ns("sft_conflict_keep"),
            sft_ui_label(labels, "conflict_keep"),
            class = "btn-default"
          ),
          shiny::actionButton(
            ns("sft_conflict_back"),
            sft_ui_label(labels, "conflict_back"),
            class = "btn-default"
          )
        )
      )
    )
  })

  shiny::observeEvent(input$sft_conflict_accept, {
    conflict <- edit_conflict()
    shiny::req(conflict)

    for (field in sft_active_input_fields(form)) {
      if (field$db_column %in% conflict$columns) {
        sft_conflict_set_input(
          session = session,
          field = field,
          value = conflict$current_record[[field$db_column]][1]
        )
      }
    }

    edit_conflict_baseline(conflict$current_record)
    edit_conflict(NULL)
  })

  shiny::observeEvent(input$sft_conflict_keep, {
    conflict <- edit_conflict()
    shiny::req(conflict)

    edit_conflict_baseline(conflict$current_record)
    edit_conflict(NULL)
  })

  shiny::observeEvent(input$sft_conflict_back, {
    shiny::req(edit_conflict())
    edit_conflict(NULL)
  })

  invisible(NULL)
}
