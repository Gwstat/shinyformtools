# Resolution of form_server()'s option bundles (permissions, table, columns,
# highlight) plus the deprecated flat arguments that still map onto them.
# Kept out of mod_form.R so the module file stays about wiring.

# Defaults of each bundle. The names double as the allow-list: a key that is
# not here is an error in sft_resolve_bundle(), so a typo such as `can_ad`
# cannot silently grant or deny anything.
sft_permission_defaults <- function() {
  list(
    can_add = TRUE,
    can_view_record = TRUE,
    can_edit = TRUE,
    can_delete = TRUE,
    can_restore = TRUE,
    can_view_versions = TRUE,
    can_view_deleted_records = TRUE,
    can_change_column_settings = TRUE,
    can_select_column_view = TRUE,
    can_view_audit = TRUE,
    can_view_table = TRUE,
    can_reset_table = TRUE,
    can_export = TRUE,
    hide_forbidden = TRUE,
    editable_fields = NULL,
    user = NULL
  )
}

sft_table_defaults <- function() {
  list(
    options = list(),
    class = "display compact stripe hover",
    filter = "none",
    format = NULL,
    audit_options = list(),
    version_options = list(),
    deleted_records_options = list(),
    datetime_format = sft_default_datetime_format(),
    checkbox_labels = FALSE
  )
}

sft_columns_defaults <- function() {
  list(
    visible = NULL,
    views = NULL,
    default_view = "Standard",
    persist = TRUE,
    show_system = FALSE,
    labels = NULL
  )
}

sft_highlight_defaults <- function() {
  list(
    fields = NULL,
    tab = TRUE,
    color = "#dc3545",
    show_changed = TRUE,
    changed_color = "#2b8cff",
    invalid = TRUE
  )
}

# Where each deprecated flat argument of form_server() now lives.
sft_form_server_legacy_mapping <- function() {
  to <- function(bundle, key) list(bundle = bundle, key = key)

  c(
    lapply(
      stats::setNames(nm = setdiff(names(sft_permission_defaults()), "user")),
      function(key) to("permissions", key)
    ),
    list(
      table_options = to("table", "options"),
      table_class = to("table", "class"),
      table_filter = to("table", "filter"),
      table_format = to("table", "format"),
      audit_options = to("table", "audit_options"),
      version_options = to("table", "version_options"),
      deleted_records_options = to("table", "deleted_records_options"),
      datetime_format = to("table", "datetime_format"),
      table_columns = to("columns", "visible"),
      table_views = to("columns", "views"),
      default_column_view = to("columns", "default_view"),
      persist_column_settings = to("columns", "persist"),
      show_system_columns = to("columns", "show_system"),
      display_column_labels = to("columns", "labels"),
      highlight_fields = to("highlight", "fields"),
      highlight_tab = to("highlight", "tab"),
      highlight_color = to("highlight", "color"),
      show_changed = to("highlight", "show_changed"),
      changed_color = to("highlight", "changed_color"),
      # The audit output is always registered; Shiny only computes it when
      # form_ui() rendered the container, so the server flag had no job left.
      show_audit = list(bundle = NULL, key = NULL)
    )
  )
}

# Resolve the four bundles of form_server() against their defaults, folding in
# whatever arrived through deprecated arguments (`dots`). Explicit bundle
# entries win over deprecated ones. Also validates `form_layout`.
sft_form_server_settings <- function(permissions = list(),
                                     table = list(),
                                     columns = list(),
                                     highlight = list(),
                                     form_layout = NULL,
                                     dots = list()) {
  legacy <- sft_map_deprecated_args(
    dots = dots,
    mapping = sft_form_server_legacy_mapping(),
    fn = "form_server"
  )

  if (!is.null(form_layout)) {
    form_layout <- match.arg(form_layout, c("modal", "inline"))
  }

  list(
    permissions = sft_resolve_bundle(
      permissions, legacy$bundles$permissions,
      sft_permission_defaults(), "permissions"
    ),
    table = sft_resolve_bundle(
      table, legacy$bundles$table,
      sft_table_defaults(), "table"
    ),
    columns = sft_resolve_bundle(
      columns, legacy$bundles$columns,
      sft_columns_defaults(), "columns"
    ),
    highlight = sft_resolve_bundle(
      highlight, legacy$bundles$highlight,
      sft_highlight_defaults(), "highlight"
    ),
    form_layout = form_layout
  )
}
