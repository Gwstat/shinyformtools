# Internal module state and row-selection helpers.

sft_module_current_user <- function(input, user = NULL) {
  if (is.function(user)) {
    return(user())
  }

  if (!is.null(user)) {
    return(user)
  }

  shiny::isolate(input$sft_user) %||% NA_character_
}

sft_module_permission <- function(permission, default = TRUE) {
  if (is.function(permission)) {
    return(isTRUE(permission()))
  }

  if (is.null(permission)) {
    return(isTRUE(default))
  }

  isTRUE(permission)
}

sft_module_value <- function(value, default = NULL) {
  if (is.function(value)) {
    out <- value()
  } else {
    out <- value
  }

  if (is.null(out) || length(out) == 0L || is.na(out[[1L]])) {
    return(default)
  }

  out
}

sft_table_view_columns <- function(table_views, view_name) {
  if (is.function(table_views)) {
    table_views <- table_views()
  }

  if (is.null(table_views)) {
    return(NULL)
  }

  if (!is.list(table_views)) {
    stop("table_views must be NULL, a named list, or a function returning a named list.", call. = FALSE)
  }

  view_name <- sft_column_view_key(view_name)
  table_views[[view_name]] %||% NULL
}

sft_table_view_names <- function(table_views) {
  if (is.function(table_views)) {
    table_views <- table_views()
  }

  if (is.null(table_views)) {
    return(character())
  }

  if (!is.list(table_views)) {
    stop("table_views must be NULL, a named list, or a function returning a named list.", call. = FALSE)
  }

  names(table_views) %||% character()
}

sft_module_include_deleted <- function(input, include_deleted_default = FALSE) {
  input$include_deleted %||% include_deleted_default
}

sft_selected_record_from_dt <- function(data, selected) {
  if (is.null(selected) || length(selected) != 1L) {
    return(NULL)
  }

  if (!is.data.frame(data) || nrow(data) == 0L || selected > nrow(data)) {
    return(NULL)
  }

  data[selected, , drop = FALSE]
}

sft_selected_record_from_display <- function(display_data, raw_data, selected) {
  row <- sft_selected_record_from_dt(display_data, selected)

  if (is.null(row)) {
    return(NULL)
  }

  if (is.data.frame(raw_data) &&
      "sft_id" %in% names(row) &&
      "sft_id" %in% names(raw_data)) {
    matched <- raw_data[raw_data$sft_id == row$sft_id[1], , drop = FALSE]

    if (nrow(matched) == 1L) {
      return(matched)
    }
  }

  row
}

sft_row_is_deleted <- function(row) {
  if (is.null(row) || !"sft_is_deleted" %in% names(row)) {
    return(FALSE)
  }

  isTRUE(row$sft_is_deleted[1] == 1L)
}


