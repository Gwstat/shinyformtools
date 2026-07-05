# Column-set resolution for record and audit tables: which columns are shown by
# default, which are allowed, the labelled choices for the column picker, and
# resolving a requested column set against the allowed set.

sft_default_record_columns <- function(form,
                                       data,
                                       show_system_columns = FALSE) {
  field_columns <- vapply(
    sft_active_input_fields(form),
    function(field) field$db_column,
    character(1)
  )

  if (isTRUE(show_system_columns)) {
    columns <- c(
      "sft_id",
      "sft_easy_id",
      "sft_uuid",
      field_columns,
      "sft_form_version",
      "sft_created_at",
      "sft_created_by",
      "sft_updated_at",
      "sft_updated_by",
      "sft_is_deleted",
      "sft_deleted_at",
      "sft_deleted_by"
    )
  } else {
    columns <- c(
      "sft_easy_id",
      field_columns,
      "sft_updated_at",
      "sft_updated_by",
      "sft_is_deleted"
    )
  }

  intersect(columns, names(data))
}


sft_allowed_record_columns <- function(form,
                                       data,
                                       show_system_columns = FALSE) {
  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  default_columns <- sft_default_record_columns(
    form = form,
    data = data,
    show_system_columns = show_system_columns
  )

  if (ncol(data) == 0L) {
    return(default_columns)
  }

  # Shape columns hold serialized geometry, not a meaningful table cell, so they
  # are never offered as record-table columns (they are drawn on a map instead).
  shape_columns <- sft_shape_columns(form)

  if (isTRUE(show_system_columns)) {
    return(unique(c(default_columns, setdiff(names(data), shape_columns))))
  }

  extra_columns <- setdiff(names(data), default_columns)
  extra_columns <- extra_columns[!startsWith(extra_columns, sft_reserved_prefix)]
  extra_columns <- setdiff(extra_columns, shape_columns)

  unique(c(default_columns, extra_columns))
}

# Database column names of a form's shape fields (geometry columns).
sft_shape_columns <- function(form) {
  shape_fields <- Filter(sft_is_shape_field, form$fields)
  vapply(shape_fields, function(field) field$db_column, character(1))
}

sft_record_column_choices <- function(form,
                                      data,
                                      show_system_columns = FALSE,
                                      display_column_labels = NULL) {
  columns <- sft_allowed_record_columns(
    form = form,
    data = data,
    show_system_columns = show_system_columns
  )

  labels <- sft_display_column_labels(
    form = form,
    display_column_labels = display_column_labels
  )

  stats::setNames(columns, sft_relabel(columns, labels))
}

sft_resolve_record_columns <- function(form,
                                       data,
                                       columns = NULL,
                                       show_system_columns = FALSE) {
  default_columns <- sft_default_record_columns(
    form = form,
    data = data,
    show_system_columns = show_system_columns
  )

  if (is.null(columns) || length(columns) == 0L) {
    return(default_columns)
  }

  allowed_columns <- sft_allowed_record_columns(
    form = form,
    data = data,
    show_system_columns = show_system_columns
  )

  resolved <- intersect(as.character(columns), allowed_columns)

  if (length(resolved) == 0L) {
    return(default_columns)
  }

  resolved
}

sft_default_audit_columns <- function(data) {
  intersect(
    c(
      "log_id",
      "record_id",
      "record_uuid",
      "action",
      "version_no",
      "changed_at",
      "changed_by",
      "changed_fields_json",
      "reason"
    ),
    names(data)
  )
}
