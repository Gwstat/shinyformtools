# Inline (non-modal) add/edit form panel.
#
# When form_server()/form_ui() run with form_layout = "inline", the add and edit
# forms render into a panel above the records table instead of a modalDialog. The
# panel reuses the shared form bodies (sft_add_form_body / sft_edit_form_body) and
# the same submit ids (submit_add / submit_edit), so validation, the
# versions/restore flow and the modal-header outputs all work unchanged. Cancel is
# a real actionButton (sft_inline_cancel), since modalButton only dismisses modals.

sft_inline_form_css <- function() {
  shiny::tags$style(shiny::HTML(
    "
    .sft-inline-form {
      margin-bottom: 1rem;
    }
    .sft-inline-form-actions {
      margin-top: 0.75rem;
      text-align: right;
    }
    "
  ))
}

sft_inline_form_panel <- function(ns,
                                  title,
                                  body,
                                  submit_id,
                                  submit_label,
                                  labels,
                                  can_submit = TRUE,
                                  cancel_label = "cancel") {
  shiny::div(
    class = "sft-inline-form well",
    sft_inline_form_css(),
    if (!is.null(title) && nzchar(title)) {
      shiny::h4(title)
    },
    body,
    shiny::div(
      class = "sft-inline-form-actions",
      sft_action_button_if_label(ns, "sft_inline_cancel", labels, cancel_label),
      if (isTRUE(can_submit)) {
        sft_action_button_if_label(ns, submit_id, labels, submit_label)
      }
    )
  )
}

# Inline layout: render the active add/edit form into the panel above the
# table. Reuses the shared modal bodies and the same submit_* ids, so the
# submit/validation observers in mod_crud.R are unchanged. The form is resolved
# for the current user so per-user `editable` functions apply inline too.
sft_register_inline_form <- function(input, output, session, state) {
  form <- state$form
  # Live binding, not a copy: the labels follow a language that changes.
  makeActiveBinding("labels", function() state$labels, environment())
  modal_header <- state$modal_header
  editable_fields <- state$permissions$editable_fields
  datetime_format <- state$table$datetime_format
  language <- state$language

  output$sft_inline_form <- shiny::renderUI(sft_with_language(language, {
    mode <- state$inline_active()

    if (is.null(mode)) {
      return(NULL)
    }

    current_user <- state$current_user()

    if (identical(mode, "add")) {
      if (!state$permission("can_add")) {
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

    row <- state$current_edit_row()

    if (is.null(row)) {
      return(NULL)
    }

    can_edit_now <- state$permission("can_edit")

    sft_inline_form_panel(
      ns = session$ns,
      title = sft_ui_label(labels, "edit_title", values = list(id = sft_row_display_id(row))),
      body = sft_edit_form_body(
        ns = session$ns,
        form = sft_resolve_editable(form, current_user),
        row = row,
        labels = labels,
        datetime_format = datetime_format,
        can_edit = can_edit_now,
        can_view_versions = state$permission("can_view_versions"),
        can_restore = state$permission("can_restore"),
        editable_fields = sft_module_editable_fields(editable_fields),
        modal_header = modal_header
      ),
      submit_id = "submit_edit",
      submit_label = "update",
      labels = labels,
      can_submit = can_edit_now,
      cancel_label = if (isTRUE(can_edit_now)) "cancel" else "close"
    )
  }))

  shiny::observeEvent(input$sft_inline_cancel, {
    state$inline_active(NULL)
    state$edit_conflict(NULL)
    state$invalid_fields(character())
  })

  invisible(list())
}
