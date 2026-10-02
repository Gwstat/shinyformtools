# Visual table-style presets for the form module.
#
# A preset is a single static <style> block injected by form_ui(), following the
# same pattern as sft_button_css() / sft_highlight_css(): pure CSS, no
# JavaScript, no change to how the tables are built. Every rule is scoped to the
# module's own DT outputs by their namespaced ids (records, audit,
# deleted_records, restore_versions), so host-app tables are never touched.
# Id-scoping (rather than a wrapper class) is deliberate: the deleted-records
# and versions tables render inside modals, which Shiny appends to <body>,
# outside any container form_ui() could wrap.
#
# "classic" emits no CSS at all, so existing apps are byte-for-byte unchanged.
# The stripe/hover/selection overrides set both background-color and box-shadow
# because DataTables switched from the former to the latter (inset 9999px
# box-shadow) in 1.13; covering both keeps the presets robust across DT
# versions.

sft_table_styles <- function() {
  c("classic", "clean", "publication", "compact")
}

# Resolve the effective preset: an explicit argument wins, NULL consults the
# global option (so an app can set the look once), and the fallback is
# "classic" = today's unmodified DT appearance.
sft_resolve_table_style <- function(table_style = NULL) {
  if (is.null(table_style)) {
    table_style <- getOption("shinyformtools.table_style", "classic")
  }

  if (!is.character(table_style) || length(table_style) != 1L ||
      !(table_style %in% sft_table_styles())) {
    stop(
      "`table_style` must be one of ",
      paste0('"', sft_table_styles(), '"', collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  table_style
}

# The module's DT output ids that a preset styles. All of them, not just the
# records table, so the audit table and the modal tables match.
sft_table_style_output_ids <- function() {
  c("records", "audit", "deleted_records", "restore_versions")
}

# Build one comma-joined selector list: `suffix` is appended to each
# namespaced output id, e.g. " table.dataTable thead th".
sft_table_style_selector <- function(ns, suffix = "") {
  paste0("#", ns(sft_table_style_output_ids()), suffix, collapse = ",\n")
}

sft_table_style_rules <- function(ns, table_style) {
  sel <- function(suffix) sft_table_style_selector(ns, suffix)

  # DataTables applies striping/hover for the classes "stripe"/"hover" AND for
  # the shorthand class "display" (which implies both) - the audit table uses
  # "display", so every stripe/hover rule must cover both spellings.
  stripe_sel <- paste(
    sel(" table.dataTable.stripe > tbody > tr.odd > *"),
    sel(" table.dataTable.display > tbody > tr.odd > *"),
    sep = ",\n"
  )
  hover_sel <- paste(
    sel(" table.dataTable.hover > tbody > tr:hover > *"),
    sel(" table.dataTable.display > tbody > tr:hover > *"),
    sep = ",\n"
  )

  if (identical(table_style, "clean")) {
    return(paste0(
      "/* sft table style: clean */\n",
      sel(" .dataTables_wrapper"), " {\n",
      "  background: #ffffff;\n",
      "  border: 1px solid #e3e7eb;\n",
      "  border-radius: 10px;\n",
      "  padding: 0.75rem 1rem;\n",
      "  box-shadow: 0 1px 3px rgba(16, 24, 40, 0.06);\n",
      "}\n",
      sel(" table.dataTable"), " {\n",
      "  border-collapse: collapse;\n",
      "}\n",
      sel(" table.dataTable thead th"), " {\n",
      "  text-transform: uppercase;\n",
      "  font-size: 0.72em;\n",
      "  letter-spacing: 0.06em;\n",
      "  color: #6b7683;\n",
      "  font-weight: 600;\n",
      "  border-top: none;\n",
      "  border-bottom: 2px solid #e3e7eb;\n",
      "  padding: 0.6rem 0.75rem;\n",
      "}\n",
      sel(" table.dataTable tbody td"), " {\n",
      "  padding: 0.6rem 0.75rem;\n",
      "  border-top: 1px solid #eef1f4;\n",
      "  border-left: none;\n",
      "  border-right: none;\n",
      "}\n",
      # No zebra stripes in the clean look, even with the default
      # "stripe" table class.
      stripe_sel, " {\n",
      "  box-shadow: none;\n",
      "  background-color: transparent;\n",
      "}\n",
      hover_sel, " {\n",
      "  box-shadow: inset 0 0 0 9999px #f4f6f8;\n",
      "  background-color: #f4f6f8;\n",
      "}\n",
      sel(" table.dataTable > tbody > tr.selected > *"), " {\n",
      "  box-shadow: inset 0 0 0 9999px rgba(33, 133, 208, 0.12);\n",
      "  background-color: rgba(33, 133, 208, 0.12);\n",
      "  color: inherit;\n",
      "}\n",
      sel(" .dataTables_filter input"), ",\n",
      sel(" .dt-search input"), ",\n",
      sel(" .dataTables_length select"), ",\n",
      sel(" .dt-length select"), " {\n",
      "  border: 1px solid #d5dbe0;\n",
      "  border-radius: 6px;\n",
      "  padding: 0.25rem 0.5rem;\n",
      "}\n",
      sel(" .dataTables_paginate .paginate_button"), ",\n",
      sel(" .dt-paging button"), " {\n",
      "  border-radius: 6px;\n",
      "}\n"
    ))
  }

  if (identical(table_style, "publication")) {
    # A booktabs-style journal table: serif type, a heavy rule above and below
    # the table, a light rule under the header, no vertical rules, no stripes.
    return(paste0(
      "/* sft table style: publication */\n",
      sel(" table.dataTable"), " {\n",
      "  font-family: Georgia, Cambria, \"Times New Roman\", Times, serif;\n",
      "  color: #1a1a1a;\n",
      "  border-collapse: collapse;\n",
      "  border-top: 2px solid #222222;\n",
      "  border-bottom: 2px solid #222222;\n",
      "}\n",
      sel(" table.dataTable thead th"), " {\n",
      "  font-weight: 700;\n",
      "  color: #111111;\n",
      "  background: transparent;\n",
      "  border-top: none;\n",
      "  border-bottom: 1px solid #222222;\n",
      "  padding: 0.5rem 0.75rem;\n",
      "}\n",
      sel(" table.dataTable tbody td"), " {\n",
      "  padding: 0.45rem 0.75rem;\n",
      "  border: none;\n",
      "}\n",
      stripe_sel, " {\n",
      "  box-shadow: none;\n",
      "  background-color: transparent;\n",
      "}\n",
      hover_sel, " {\n",
      "  box-shadow: inset 0 0 0 9999px #f7f6f2;\n",
      "  background-color: #f7f6f2;\n",
      "}\n",
      sel(" table.dataTable > tbody > tr.selected > *"), " {\n",
      "  box-shadow: inset 0 0 0 9999px rgba(70, 90, 120, 0.12);\n",
      "  background-color: rgba(70, 90, 120, 0.12);\n",
      "  color: inherit;\n",
      "}\n"
    ))
  }

  if (identical(table_style, "compact")) {
    return(paste0(
      "/* sft table style: compact */\n",
      sel(" table.dataTable"), " {\n",
      "  font-size: 0.9em;\n",
      "}\n",
      sel(" table.dataTable thead th"), " {\n",
      "  background: #37474f;\n",
      "  color: #ffffff;\n",
      "  font-weight: 600;\n",
      "  padding: 0.3rem 0.5rem;\n",
      "  border-bottom: none;\n",
      "}\n",
      sel(" table.dataTable tbody td"), " {\n",
      "  padding: 0.25rem 0.5rem;\n",
      "}\n",
      stripe_sel, " {\n",
      "  box-shadow: inset 0 0 0 9999px #f4f6f8;\n",
      "  background-color: #f4f6f8;\n",
      "}\n",
      hover_sel, " {\n",
      "  box-shadow: inset 0 0 0 9999px #e8edf1;\n",
      "  background-color: #e8edf1;\n",
      "}\n",
      sel(" table.dataTable > tbody > tr.selected > *"), " {\n",
      "  box-shadow: inset 0 0 0 9999px rgba(55, 71, 79, 0.14);\n",
      "  background-color: rgba(55, 71, 79, 0.14);\n",
      "  color: inherit;\n",
      "}\n"
    ))
  }

  NULL
}

# The tag form_ui() injects. NULL for "classic" so the default UI is unchanged.
sft_table_style_css <- function(ns, table_style = NULL) {
  table_style <- sft_resolve_table_style(table_style)
  rules <- sft_table_style_rules(ns, table_style)

  if (is.null(rules)) {
    return(NULL)
  }

  shiny::tags$style(shiny::HTML(rules))
}
