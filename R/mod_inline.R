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
