sft_preference_user_id <- function(user) {
  if (is.null(user) || length(user) == 0L || is.na(user[[1L]]) || !nzchar(as.character(user[[1L]]))) {
    return("__default__")
  }

  as.character(user[[1L]])
}

sft_init_preferences_table <- function(conn) {
  types <- sft_system_table_types(conn)

  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE IF NOT EXISTS sft_user_preferences (",
      "preference_id ", types$id, ", ",
      "form_id ", types$short_text, " NOT NULL, ",
      "user_id ", types$short_text, " NOT NULL, ",
      "preference_key ", types$short_text, " NOT NULL, ",
      "preference_json ", types$long_text, ", ",
      "updated_at ", types$short_text, ", ",
      "UNIQUE (form_id, user_id, preference_key)",
      ")"
    )
  )

  invisible(conn)
}

sft_get_user_preference <- function(conn,
                                    form,
                                    user,
                                    key) {
  user_id <- sft_preference_user_id(user)

  rows <- DBI::dbGetQuery(
    conn,
    "
    SELECT preference_json
    FROM sft_user_preferences
    WHERE form_id = ? AND user_id = ? AND preference_key = ?
    ",
    params = list(
      form$form_id,
      user_id,
      key
    )
  )

  if (nrow(rows) == 0L || is.na(rows$preference_json[1L])) {
    return(NULL)
  }

  jsonlite::fromJSON(rows$preference_json[1L], simplifyVector = TRUE)
}

sft_set_user_preference <- function(conn,
                                    form,
                                    user,
                                    key,
                                    value) {
  user_id <- sft_preference_user_id(user)
  now <- sft_now()
  value_json <- as.character(sft_as_json(value))

  sft_db_with_transaction(conn, {
    DBI::dbExecute(
      conn,
      "
      DELETE FROM sft_user_preferences
      WHERE form_id = ? AND user_id = ? AND preference_key = ?
      ",
      params = list(
        form$form_id,
        user_id,
        key
      )
    )

    columns <- c(
      "form_id",
      "user_id",
      "preference_key",
      "preference_json",
      "updated_at"
    )
    values <- list(
      form$form_id,
      user_id,
      key,
      value_json,
      now
    )

    prepared <- sft_prepend_explicit_id(
      conn, "sft_user_preferences", "preference_id", columns, values
    )
    columns <- prepared$columns
    values <- prepared$values

    DBI::dbExecute(
      conn,
      paste0(
        "INSERT INTO sft_user_preferences (",
        sft_sql_quoted_columns(conn, columns),
        ") VALUES (",
        paste(rep("?", length(values)), collapse = ", "),
        ")"
      ),
      params = values
    )

    invisible(TRUE)
  })
}

sft_get_column_settings <- function(conn, form, user) {
  value <- sft_get_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_columns"
  )

  if (is.null(value)) {
    return(NULL)
  }

  if (is.list(value) && !is.null(value$columns)) {
    value <- value$columns
  }

  as.character(value)
}

sft_set_column_settings <- function(conn, form, user, columns) {
  sft_set_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_columns",
    value = list(columns = as.character(columns))
  )
}


sft_column_view_key <- function(view_name) {
  view_name <- as.character(view_name %||% "Standard")

  if (length(view_name) == 0L || is.na(view_name[[1L]]) || !nzchar(trimws(view_name[[1L]]))) {
    return("Standard")
  }

  trimws(view_name[[1L]])
}

sft_get_column_views <- function(conn, form, user) {
  value <- sft_get_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_column_views"
  )

  if (is.null(value)) {
    legacy <- sft_get_column_settings(
      conn = conn,
      form = form,
      user = user
    )

    if (is.null(legacy)) {
      return(list())
    }

    return(list(Standard = as.character(legacy)))
  }

  if (!is.list(value)) {
    return(list())
  }

  if (!is.null(value$views) && is.list(value$views)) {
    value <- value$views
  }

  out <- list()

  for (name in names(value)) {
    view_name <- sft_column_view_key(name)
    out[[view_name]] <- as.character(value[[name]])
  }

  out
}

sft_set_column_view <- function(conn,
                                form,
                                user,
                                view_name = "Standard",
                                columns) {
  view_name <- sft_column_view_key(view_name)

  views <- sft_get_column_views(
    conn = conn,
    form = form,
    user = user
  )

  views[[view_name]] <- as.character(columns)

  sft_set_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_column_views",
    value = list(views = views)
  )

  sft_set_active_column_view(
    conn = conn,
    form = form,
    user = user,
    view_name = view_name
  )

  invisible(TRUE)
}

sft_get_active_column_view <- function(conn, form, user) {
  value <- sft_get_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_column_active_view"
  )

  if (is.null(value)) {
    return("Standard")
  }

  if (is.list(value) && !is.null(value$view_name)) {
    return(sft_column_view_key(value$view_name))
  }

  sft_column_view_key(value)
}

sft_set_active_column_view <- function(conn,
                                       form,
                                       user,
                                       view_name = "Standard") {
  sft_set_user_preference(
    conn = conn,
    form = form,
    user = user,
    key = "record_column_active_view",
    value = list(view_name = sft_column_view_key(view_name))
  )

  invisible(TRUE)
}

sft_get_column_view <- function(conn,
                                form,
                                user,
                                view_name = "Standard") {
  views <- sft_get_column_views(
    conn = conn,
    form = form,
    user = user
  )

  view_name <- sft_column_view_key(view_name)

  views[[view_name]] %||% NULL
}


sft_shared_column_view_user <- function() {
  "__shared_column_views__"
}

sft_get_shared_column_views <- function(conn, form) {
  sft_get_column_views(
    conn = conn,
    form = form,
    user = sft_shared_column_view_user()
  )
}

sft_set_shared_column_view <- function(conn,
                                       form,
                                       view_name = "Standard",
                                       columns) {
  sft_set_column_view(
    conn = conn,
    form = form,
    user = sft_shared_column_view_user(),
    view_name = view_name,
    columns = columns
  )

  invisible(TRUE)
}

sft_get_shared_column_view <- function(conn,
                                       form,
                                       view_name = "Standard") {
  sft_get_column_view(
    conn = conn,
    form = form,
    user = sft_shared_column_view_user(),
    view_name = view_name
  )
}

sft_available_shared_column_view_names <- function(conn, form) {
  names(sft_get_shared_column_views(conn = conn, form = form))
}

sft_resolve_saved_column_view <- function(conn,
                                          form,
                                          user,
                                          table_views = NULL,
                                          view_name = "Standard") {
  view_name <- sft_column_view_key(view_name)

  user_columns <- sft_get_column_view(
    conn = conn,
    form = form,
    user = user,
    view_name = view_name
  )

  if (!is.null(user_columns)) {
    return(user_columns)
  }

  shared_columns <- sft_get_shared_column_view(
    conn = conn,
    form = form,
    view_name = view_name
  )

  if (!is.null(shared_columns)) {
    return(shared_columns)
  }

  sft_table_view_columns(
    table_views = table_views,
    view_name = view_name
  )
}

sft_available_column_view_names <- function(conn, form, user) {
  views <- sft_get_column_views(
    conn = conn,
    form = form,
    user = user
  )

  names(views)
}
