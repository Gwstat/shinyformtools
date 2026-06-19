# Form modal rendering helpers.

sft_call_modal_header <- function(modal_header,
                                  form,
                                  ns = identity,
                                  prefix = "",
                                  values = list(),
                                  record = NULL,
                                  context = list(),
                                  input = NULL,
                                  output = NULL,
                                  session = NULL) {
  if (is.null(modal_header)) {
    return(NULL)
  }

  if (is.function(modal_header)) {
    args <- list(
      ns = ns,
      prefix = prefix,
      values = values,
      record = record,
      context = context,
      input = input,
      output = output,
      session = session,
      form = form
    )

    formals_names <- names(formals(modal_header))

    if ("..." %in% formals_names) {
      return(do.call(modal_header, args))
    }

    return(do.call(modal_header, args[intersect(names(args), formals_names)]))
  }

  modal_header
}

sft_render_modal_header <- function(modal_header,
                                    form,
                                    ns = identity,
                                    prefix = "",
                                    values = list(),
                                    record = NULL,
                                    context = list(),
                                    input = NULL,
                                    output = NULL,
                                    session = NULL) {
  header_ui <- sft_call_modal_header(
    modal_header = modal_header,
    form = form,
    ns = ns,
    prefix = prefix,
    values = values,
    record = record,
    context = context,
    input = input,
    output = output,
    session = session
  )

  if (is.null(header_ui)) {
    return(NULL)
  }

  if (is.character(header_ui)) {
    if (length(header_ui) != 1L || is.na(header_ui)) {
      stop("modal_header character values must be scalar strings.", call. = FALSE)
    }

    return(shiny::HTML(header_ui))
  }

  header_ui
}

sft_modal_input_values <- function(form, input, prefix = "", record = NULL) {
  values <- if (!is.null(record)) {
    sft_row_to_list(record)
  } else {
    list()
  }

  fields <- sft_active_input_fields(form)

  for (field in fields) {
    input_value <- input[[paste0(prefix, field$id)]]

    if (!is.null(input_value)) {
      values[[field$id]] <- input_value
    } else if (is.null(values[[field$id]])) {
      values[[field$id]] <- sft_field_value_from_record(record, field)
    }
  }

  values
}

# Shared body of the add form, rendered identically in the modal and inline
# layouts. The add_modal_header uiOutput is wired by an existing server output.
sft_add_form_body <- function(ns, form, modal_header = NULL) {
  shiny::tagList(
    if (!is.null(modal_header)) {
      shiny::uiOutput(ns("add_modal_header"))
    },
    render_form_fields(
      form = form,
      ns = ns,
      prefix = "add_"
    )
  )
}

sft_show_add_modal <- function(form,
                               session,
                               labels,
                               modal_sizes,
                               modal_header = NULL) {
  ns <- session$ns

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(labels, "add_title"),
      size = sft_modal_size(modal_sizes, "add"),
      easyClose = TRUE,
      sft_modal_css(modal_sizes, "add"),
      sft_add_form_body(ns = ns, form = form, modal_header = modal_header),
      footer = shiny::tagList(
        sft_modal_button_if_label(labels, "cancel"),
        sft_action_button_if_label(ns, "submit_add", labels, "save")
      )
    )
  )
}

sft_versions_output_ui <- function(ns, output_id = "restore_versions") {
  shiny::div(
    class = "sft-version-table-scroll",
    style = paste(
      "width: 100%;",
      "overflow-x: auto;",
      "overflow-y: visible;",
      "padding-bottom: 0.5rem;"
    ),
    DT::DTOutput(ns(output_id))
  )
}


sft_modal_accordion_css <- function() {
  shiny::tags$style(shiny::HTML(
    "
    .sft-versions-accordion summary {
      cursor: pointer;
      list-style: none;
      user-select: none;
      padding: 0.4rem 0;
    }
    .sft-versions-accordion summary::-webkit-details-marker {
      display: none;
    }
    .sft-versions-accordion summary::before {
      content: '+ ';
      display: inline-block;
      width: 1.2rem;
      font-weight: 700;
    }
    .sft-versions-accordion[open] summary::before {
      content: '\u2212 ';
    }
    .sft-versions-accordion .sft-versions-actions {
      margin-top: 0.75rem;
      text-align: right;
    }
    "
  ))
}

# Shared body of the edit form, rendered identically in the modal and inline
# layouts. The edit_modal_header uiOutput and the restore_versions DTOutput /
# confirm_restore button are wired by existing server outputs/observers, so the
# same ids work in either layout.
sft_edit_form_body <- function(ns,
                               form,
                               row,
                               labels,
                               datetime_format = sft_default_datetime_format(),
                               can_edit = TRUE,
                               can_view_versions = TRUE,
                               can_restore = TRUE,
                               editable_fields = NULL,
                               modal_header = NULL) {
  shiny::tagList(
    sft_modal_accordion_css(),
    sft_record_meta_ui(
      row = row,
      labels = labels,
      datetime_format = datetime_format
    ),
    if (!is.null(modal_header)) {
      shiny::uiOutput(ns("edit_modal_header"))
    },
    render_form_fields(
      form = form,
      ns = ns,
      prefix = "edit_",
      values = row,
      read_only = !isTRUE(can_edit),
      editable_fields = editable_fields
    ),
    if (isTRUE(can_view_versions)) {
      shiny::tagList(
        shiny::hr(),
        shiny::tags$details(
          class = "sft-versions-accordion",
          shiny::tags$summary(shiny::strong(sft_ui_label(labels, "edit_versions_title"))),
          shiny::p(shiny::tags$small(sft_ui_label(labels, "edit_versions_intro"))),
          sft_versions_output_ui(ns, "restore_versions"),
          if (isTRUE(can_restore)) {
            shiny::div(
              class = "sft-versions-actions",
              sft_action_button_if_label(ns, "confirm_restore", labels, "confirm_restore")
            )
          }
        )
      )
    }
  )
}

sft_show_edit_modal <- function(form,
                                session,
                                row,
                                labels,
                                modal_sizes,
                                modal_header = NULL,
                                datetime_format = sft_default_datetime_format(),
                                can_edit = TRUE,
                                can_view_versions = TRUE,
                                can_restore = TRUE,
                                editable_fields = NULL) {
  ns <- session$ns

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(
        labels = labels,
        key = "edit_title",
        values = list(id = row$sft_easy_id[1])
      ),
      size = sft_modal_size(modal_sizes, "edit"),
      easyClose = TRUE,
      sft_modal_css(modal_sizes, "edit"),
      sft_edit_form_body(
        ns = ns,
        form = form,
        row = row,
        labels = labels,
        datetime_format = datetime_format,
        can_edit = can_edit,
        can_view_versions = can_view_versions,
        can_restore = can_restore,
        editable_fields = editable_fields,
        modal_header = modal_header
      ),
      footer = shiny::tagList(
        sft_modal_button_if_label(labels, if (isTRUE(can_edit)) "cancel" else "close"),
        if (isTRUE(can_edit)) {
          sft_action_button_if_label(ns, "submit_edit", labels, "update")
        }
      )
    )
  )
}

sft_show_delete_modal <- function(session, row, labels, modal_sizes) {
  ns <- session$ns

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(labels, "delete_title"),
      size = sft_modal_size(modal_sizes, "delete"),
      sft_modal_css(modal_sizes, "delete"),
      sft_ui_label(
        labels = labels,
        key = "delete_question",
        values = list(id = row$sft_easy_id[1])
      ),
      footer = shiny::tagList(
        sft_modal_button_if_label(labels, "cancel"),
        sft_action_button_if_label(ns, "confirm_delete", labels, "confirm_delete")
      )
    )
  )
}


sft_record_meta_ui <- function(row,
                               labels,
                               datetime_format = sft_default_datetime_format()) {
  if (is.null(row) || !is.data.frame(row) || nrow(row) == 0L) {
    return(NULL)
  }

  time <- if ("sft_updated_at" %in% names(row)) {
    sft_format_datetime_value(row$sft_updated_at[1], datetime_format = datetime_format)
  } else {
    NA_character_
  }

  user <- if ("sft_updated_by" %in% names(row)) {
    row$sft_updated_by[1]
  } else {
    NA_character_
  }

  if ((is.na(time) || !nzchar(time)) && (is.na(user) || !nzchar(user))) {
    return(NULL)
  }

  shiny::div(
    class = "sft-record-meta",
    style = "margin-bottom: 1rem; color: #666; font-size: 0.95em;",
    sft_ui_label(
      labels = labels,
      key = "record_meta",
      values = list(
        time = time %||% "",
        user = user %||% ""
      )
    )
  )
}

sft_show_deleted_records_modal <- function(session, labels, modal_sizes, can_restore = TRUE) {
  ns <- session$ns

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(labels, "deleted_records_title"),
      size = sft_modal_size(modal_sizes, "deleted_records"),
      easyClose = TRUE,
      sft_modal_css(modal_sizes, "deleted_records"),
      shiny::p(sft_ui_label(labels, "deleted_records_intro")),
      DT::DTOutput(ns("deleted_records")),
      footer = shiny::tagList(
        sft_modal_button_if_label(labels, "close"),
        if (isTRUE(can_restore)) {
          sft_action_button_if_label(ns, "restore_deleted", labels, "restore_deleted")
        }
      )
    )
  )
}

sft_show_versions_modal <- function(session, row, labels, modal_sizes, can_restore = TRUE) {
  ns <- session$ns

  footer <- shiny::tagList(
    sft_modal_button_if_label(labels, "close"),
    if (isTRUE(can_restore)) {
      sft_action_button_if_label(ns, "confirm_restore", labels, "confirm_restore")
    }
  )

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(
        labels = labels,
        key = "versions_title",
        values = list(id = row$sft_easy_id[1])
      ),
      size = sft_modal_size(modal_sizes, "versions"),
      easyClose = TRUE,
      sft_modal_css(modal_sizes, "versions"),
      shiny::p(sft_ui_label(labels, "versions_intro")),
      if (sft_row_is_deleted(row)) {
        shiny::p(
          shiny::strong("Note: "),
          sft_ui_label(labels, "deleted_restore_hint")
        )
      },
      sft_versions_output_ui(ns, "restore_versions"),
      footer = footer
    )
  )
}

