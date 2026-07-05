# Reactive field/tab highlighting for add/edit forms.
#
# The highlight is driven entirely by a single, reactively re-rendered <style>
# block (the "sft_highlight_style" uiOutput injected by form_ui()). It carries
# CSS rules that target each field's container element by id and, optionally, the
# nav tab that holds it by position. Because the browser re-evaluates CSS as the
# DOM changes, the glow lands on the right fields whenever the add/edit dialog
# opens - there is no custom-message timing to get wrong, no client-side caching,
# and no modal-shown hooks. The add/edit form bodies are built once at showModal
# time (not inside a renderUI), so an approach that depends on the elements being
# present when a message arrives is fragile; a global, reactive stylesheet is not.
#
# Two channels share the one style block:
#   * highlight - a caller-driven glow (highlight_color) on chosen fields, in
#     both the add and edit forms.
#   * changed   - an automatic glow (changed_color) on edit-form fields whose
#     current value differs from the stored record value.
#
# Colours and the highlighted set are resolved server-side, so the whole feature
# is controlled from R with no UI-side argument.

# Placeholder injected by form_ui(): the reactive style block. Filled in by
# sft_register_highlight() via output$sft_highlight_style.
sft_highlight_css <- function(ns) {
  shiny::uiOutput(ns("sft_highlight_style"))
}

# Best-effort scalar/vector normalisation for change detection. Values arrive
# from Shiny inputs (numeric, logical, Date, character, possibly multi-valued)
# and from the stored record (often character), so a plain identical() would
# report spurious changes. We collapse each side to a canonical string.
sft_norm_value <- function(x) {
  if (is.null(x)) {
    return("")
  }

  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return("")
  }

  sep <- ""

  if (is.logical(x)) {
    return(paste(as.integer(x), collapse = sep))
  }

  if (is.numeric(x)) {
    return(paste(format(x, scientific = FALSE, trim = TRUE), collapse = sep))
  }

  paste(trimws(as.character(x)), collapse = sep)
}

sft_values_differ <- function(current, original) {
  !identical(sft_norm_value(current), sft_norm_value(original))
}

# Map a set of field ids to the namespaced container element ids the CSS targets.
# add/edit forms wrap each field in "sft_field_container_<prefix><id>".
sft_highlight_container_ids <- function(ns, field_ids, prefixes) {
  if (length(field_ids) == 0L) {
    return(character())
  }

  ids <- character()

  for (prefix in prefixes) {
    ids <- c(
      ids,
      vapply(
        field_ids,
        function(field_id) ns(paste0("sft_field_container_", prefix, field_id)),
        character(1),
        USE.NAMES = FALSE
      )
    )
  }

  # Keep the result unnamed so it pastes cleanly into a CSS selector list.
  unname(ids)
}

# Resolve a highlight_fields argument (NULL / character / function / reactive)
# to a character vector of field ids. Unlike sft_module_value(), an empty result
# is preserved (it means "clear all highlights").
sft_resolve_highlight_fields <- function(highlight_fields) {
  value <- if (is.function(highlight_fields)) {
    highlight_fields()
  } else {
    highlight_fields
  }

  as.character(value %||% character())
}

# One CSS box-shadow rule glowing the input control inside a set of field
# containers. We target the control (not the container) so only the input box
# glows - the label and the surrounding row are left alone. .form-control covers
# text/number/date/textarea/plain-select inputs; .selectize-input covers
# selectize dropdowns. position/z-index keep the shadow drawing above
# neighbouring fields instead of being covered by them.
sft_glow_rule <- function(container_ids, color) {
  if (length(container_ids) == 0L) {
    return(character())
  }

  selector <- paste(
    c(
      paste0("#", container_ids, " .form-control"),
      paste0("#", container_ids, " .selectize-input")
    ),
    collapse = ",\n"
  )

  paste0(
    selector, " {\n",
    "  box-shadow: 0 0 0 2px ", color, ", 0 0 8px 3px ", color, ";\n",
    "  border-radius: 4px;\n",
    "  position: relative;\n",
    "  z-index: 2;\n",
    "}"
  )
}

# One CSS rule glowing the nav tabs at the given 1-based positions. nth-child on
# the tab list maps directly to tab order in both Bootstrap 3 (shiny default) and
# Bootstrap 4/5 (bslib) markup.
sft_tab_glow_rule <- function(positions, color) {
  if (length(positions) == 0L) {
    return(character())
  }

  selector <- paste0(
    ".nav-tabs > li:nth-child(", positions, ") > a",
    collapse = ",\n"
  )

  paste0(
    selector, " {\n",
    "  box-shadow: inset 0 -3px 0 ", color, ";\n",
    "  font-weight: 700;\n",
    "}"
  )
}

# 1-based positions of the tabs that contain any of `field_ids`, among the form's
# sorted unique tabs. Empty when the form has at most one tab (no tabset is
# rendered, so there is nothing to glow).
sft_field_tab_positions <- function(form, field_ids) {
  if (length(field_ids) == 0L) {
    return(integer())
  }

  fields <- sft_active_input_fields(form)
  tabs <- sort(unique(vapply(fields, function(field) field$tab, integer(1))))

  if (length(tabs) <= 1L) {
    return(integer())
  }

  owners <- Filter(function(field) field$id %in% field_ids, fields)
  positions <- vapply(owners, function(field) match(field$tab, tabs), integer(1))
  sort(unique(positions))
}

# Build the combined <style> block for both channels. An empty result (no fields
# highlighted) yields an empty stylesheet, which clears any previous glow.
sft_highlight_style_block <- function(ns,
                                      form,
                                      highlight_field_ids = character(),
                                      changed_field_ids = character(),
                                      highlight_tab = TRUE,
                                      highlight_color = "#dc3545",
                                      changed_color = "#2b8cff") {
  rules <- c(
    # Channel 1: caller-driven glow on chosen fields, in add and edit.
    sft_glow_rule(
      sft_highlight_container_ids(ns, highlight_field_ids, c("add_", "edit_")),
      highlight_color
    ),
    # Channel 2: automatic glow on changed edit fields.
    sft_glow_rule(
      sft_highlight_container_ids(ns, changed_field_ids, "edit_"),
      changed_color
    )
  )

  if (isTRUE(highlight_tab)) {
    rules <- c(
      rules,
      sft_tab_glow_rule(
        sft_field_tab_positions(form, highlight_field_ids), highlight_color
      ),
      sft_tab_glow_rule(
        sft_field_tab_positions(form, changed_field_ids), changed_color
      )
    )
  }

  shiny::tags$style(shiny::HTML(paste(rules, collapse = "\n")))
}

# Field ids whose current stored value differs from the record's ORIGINAL value
# at creation - i.e. fields that have been edited at some point since the record
# was first added. The original values come from the earliest audit-log version
# (the insert snapshot); fetch_audit_log() orders by version_no, so row 1 is it.
sft_changed_since_creation_ids <- function(conn, form, row) {
  if (is.null(row) || is.null(conn)) {
    return(character())
  }

  record_id <- if (is.data.frame(row)) row[["sft_id"]][1] else row[["sft_id"]]
  if (is.null(record_id) || length(record_id) == 0L || is.na(record_id)) {
    return(character())
  }

  audit <- tryCatch(
    fetch_audit_log(form, conn = conn, record_id = record_id),
    error = function(e) NULL
  )
  if (is.null(audit) || nrow(audit) == 0L) {
    return(character())
  }

  original <- tryCatch(
    sft_json_to_record(audit$new_data_json[1]),
    error = function(e) NULL
  )
  if (is.null(original)) {
    return(character())
  }

  changed <- character()

  for (field in sft_active_input_fields(form)) {
    current_value <- sft_field_value_from_record(row, field)
    original_value <- sft_field_value_from_record(original, field)

    if (sft_values_differ(current_value, original_value)) {
      changed <- c(changed, field$id)
    }
  }

  changed
}

# Register the reactive highlight stylesheet on the module session. Non-namespaced
# registrar, called from form_server(); covered by testServer tests.
sft_register_highlight <- function(output,
                                   session,
                                   form,
                                   current_edit_row,
                                   conn = NULL,
                                   highlight_fields = NULL,
                                   highlight_tab = TRUE,
                                   highlight_color = "#dc3545",
                                   show_changed = TRUE,
                                   changed_color = "#2b8cff") {
  ns <- session$ns

  output$sft_highlight_style <- shiny::renderUI({
    highlight_ids <- if (is.null(highlight_fields)) {
      character()
    } else {
      sft_resolve_highlight_fields(highlight_fields)
    }

    changed_ids <- if (isTRUE(show_changed)) {
      sft_changed_since_creation_ids(conn, form, current_edit_row())
    } else {
      character()
    }

    sft_highlight_style_block(
      ns = ns,
      form = form,
      highlight_field_ids = highlight_ids,
      changed_field_ids = changed_ids,
      highlight_tab = highlight_tab,
      highlight_color = highlight_color,
      changed_color = changed_color
    )
  })

  invisible(NULL)
}
