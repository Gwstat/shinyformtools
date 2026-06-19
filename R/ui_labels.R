sft_default_ui_labels <- function() {
  list(
    user_label = "User",
    open_add = "Add entry",
    add_not_allowed = "Adding is not permitted for this user.",
    open_edit = "View case",
    view_not_allowed = "Viewing cases is not permitted for this user.",
    edit_not_allowed = "Editing is not permitted for this user.",
    delete = "Soft-delete selection",
    open_versions = NULL,
    refresh_table = "Reset table",
    open_deleted_records = "Deleted records",
    open_column_settings = "Column settings",
    open_column_selection = "Columns",
    include_deleted = "Show deleted records",
    audit_title = "Audit log",
    add_title = "Add entry",
    edit_title = "View case: {id}",
    delete_title = "Confirm deletion",
    delete_question = "Really soft-delete record {id}?",
    versions_title = "Show versions: {id}",
    versions_intro = "Saved versions of the selected record are shown here.",
    edit_versions_title = "Versions",
    edit_versions_intro = "Saved versions of the case. A selected version can be restored if restoring is permitted.",
    deleted_records_title = "Deleted records",
    deleted_records_intro = "Soft-deleted records are shown here. To restore one, select a record and open its versions.",
    deleted_records_empty = "No deleted records.",
    open_deleted_versions = "Show versions of the deleted record",
    column_settings_title = "Column settings",
    column_selection_title = "Columns",
    column_settings_label = "Columns to display",
    column_settings_view = "View",
    column_selection_view = "Saved view",
    column_settings_view_name = "Save view as",
    column_settings_view_name_placeholder = "New view",
    column_settings_load_view = "Load view",
    column_selection_help = "Pick columns freely and reorder them by drag and drop. When a saved view is loaded, its order is applied exactly.",
    column_selection_free_help = "Free selection: add columns on the left, remove them on the right, or reorder by drag and drop. Saved views are applied via 'Load view'.",
    column_settings_help = "Columns can be added, removed and reordered by drag and drop. Columns that no longer exist are ignored automatically.",
    column_settings_available = "Not shown",
    column_settings_selected = "Shown / order",
    column_settings_empty = "No columns",
    column_settings_add_column = "Show in table",
    column_settings_remove_column = "Remove from table",
    deleted_restore_hint = "This record is currently deleted. Restoring will reactivate it.",
    cancel = "Cancel",
    close = "Close",
    save = "Save",
    update = "Save changes",
    confirm_delete = "Soft-delete",
    confirm_restore = "Restore selected version",
    restore_deleted = "Restore latest version",
    record_restored = "Record restored.",
    apply_columns = NULL,
    apply_column_selection = NULL,
    save_column_view = "Save",
    reset_columns = NULL,
    no_selection = "Please select exactly one record.",
    no_valid_selection = "No valid selection remaining.",
    no_valid_record_selection = "No valid record selection.",
    delete_not_allowed = "Deleting is not permitted for this user.",
    restore_not_allowed = "Restoring is not permitted for this user.",
    versions_not_allowed = "Viewing versions is not permitted for this user.",
    deleted_records_not_allowed = "Viewing deleted records is not permitted for this user.",
    column_settings_not_allowed = "Column settings are not permitted for this user.",
    column_selection_not_allowed = "Column selection is not permitted for this user.",
    reset_not_allowed = "Resetting the table is not permitted for this user.",
    table_not_allowed = "Viewing the records table is not permitted for this user.",
    deleted_cannot_edit = "Deleted records cannot be edited. Please restore via Show versions.",
    already_deleted = "Record is already deleted.",
    record_added = "Record added.",
    record_updated = "Record updated.",
    record_deleted = "Record soft-deleted.",
    record_meta = "Last edit: {time} \u00b7 User: {user}",
    no_versions = "No restorable versions available.",
    choose_version = "Please select exactly one version.",
    version_unavailable = "The selected version is no longer available.",
    version_restored = "Version {version} was restored.",
    columns_applied = "Column selection applied.",
    columns_saved = "Column view saved.",
    columns_loaded = "Column view loaded.",
    columns_reset = "Column selection reset to default.",
    standard_column_view_not_overwritable = "The default view cannot be overwritten. Please choose a new view name.",
    table_refreshed = "Table filter and state were reset."
  )
}

sft_ui_labels <- function(labels = list()) {
  if (is.null(labels)) {
    labels <- list()
  }

  if (!is.list(labels)) {
    stop("labels must be a named list.", call. = FALSE)
  }

  option_labels <- getOption("shinyformtools.labels", list())

  if (!is.list(option_labels)) {
    option_labels <- list()
  }

  # English default <- global option (use_german) <- explicit per-form labels.
  utils::modifyList(
    utils::modifyList(
      sft_default_ui_labels(),
      option_labels,
      keep.null = TRUE
    ),
    labels,
    keep.null = TRUE
  )
}

sft_ui_label <- function(labels, key, values = list()) {
  value <- labels[[key]]

  if (is.null(value)) {
    return(NULL)
  }

  sft_interpolate_text(
    text = value,
    values = values
  )
}

sft_has_ui_label <- function(labels, key) {
  !is.null(sft_ui_label(labels, key))
}
