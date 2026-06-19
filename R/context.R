# Shared context object passed to display, modal, table-format and input-binding hooks.

sft_form_context <- function(form,
                             conn = NULL,
                             input = NULL,
                             output = NULL,
                             session = NULL,
                             records = NULL,
                             display_records = NULL,
                             selected_record = NULL,
                             refresh = NULL,
                             refs = list(),
                             user = NULL) {
  list(
    form = form,
    conn = conn,
    input = input,
    output = output,
    session = session,
    records = records,
    display_records = display_records,
    selected_record = selected_record,
    refresh = refresh,
    refs = refs,
    user = user
  )
}

sft_display_transform_context <- function(form,
                                          conn = NULL,
                                          input = NULL,
                                          output = NULL,
                                          session = NULL,
                                          records = NULL,
                                          display_records = NULL,
                                          selected_record = NULL,
                                          refresh = NULL,
                                          refs = list(),
                                          user = NULL) {
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
    refs = refs,
    user = user
  )
}
