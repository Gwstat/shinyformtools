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

# Resolve editable_fields. Unlike sft_module_value(), an empty vector is
# meaningful here: NULL means "no restriction", character(0) means "no field
# may be edited". A permission boundary must fail closed, so an empty result
# from a function/reactive locks everything instead of unlocking everything.
sft_module_editable_fields <- function(value) {
  if (is.function(value)) {
    value <- value()
  }

  if (is.null(value)) {
    return(NULL)
  }

  value <- as.character(value)
  value[!is.na(value)]
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

# ---------------------------------------------------------------------------
# Module state: the one object every registrar receives.
#
# form_server() builds it once per session. It holds the resolved settings,
# the connection (behind an accessor), every reactiveVal the flows share, the
# core reactives (records, display_records, selected_record), the display
# context and the shared helpers (refresh, notify, run_mutation). Registrars
# are `function(input, output, session, state)`: they read what they need from
# `state` at the top and return the reactives they expose, which form_server()
# adds back to the state with sft_expose(). Anything another registrar exposes
# must be read LAZILY (`function() state$x()`), never unpacked, because it may
# not exist yet when this registrar runs.
sft_module_state <- function(input,
                             output,
                             session,
                             form,
                             conn,
                             user,
                             settings,
                             labels,
                             language = NULL,
                             modal_sizes,
                             display_transform,
                             modal_header,
                             input_bindings,
                             refresh_triggers,
                             conflict_check,
                             include_deleted_default,
                             form_layout) {
  state <- new.env(parent = emptyenv())

  state$form <- form
  state$user <- user
  state$labels <- labels
  # The form's language() or NULL. `labels` above are already resolved with
  # it; everything computed LATER (table headers, validation messages, the
  # DataTables chrome, fallback tab names) reads the active language, so every
  # observer body and render runs inside sft_with_language(). run_mutation()
  # and guard() below do that for observers.
  state$language <- language
  state$modal_sizes <- modal_sizes
  state$permissions <- settings$permissions
  state$table <- settings$table
  state$columns <- settings$columns
  state$highlight <- settings$highlight
  state$display_transform <- display_transform
  state$modal_header <- modal_header
  state$input_bindings <- input_bindings
  state$conflict_check <- conflict_check
  state$include_deleted_default <- include_deleted_default

  # Connection --------------------------------------------------------------
  # The raw handle lives in `state$handle`; nothing outside this block reads
  # it directly. `state$conn()` is the accessor everyone uses: for a
  # module-owned connection it probes and, if the server dropped it (MariaDB
  # wait_timeout), reopens it and swaps the handle - so the session-ended
  # hook below closes the CURRENT handle, and no observer can hold a stale
  # one. A caller-supplied connection is never probed, closed or replaced.
  #
  # A module-owned connection that cannot be opened does NOT stop the module:
  # a server at max_connections refuses new sessions for a while, and an app
  # that dies with the driver's message helps nobody. The handle stays NULL,
  # the user is told, and the next state$conn() simply tries again.
  state$owns_connection <- is.null(conn)
  state$handle <- conn
  state$connect_error <- NULL

  if (state$owns_connection) {
    state$handle <- tryCatch(
      db_connect(form$db),
      error = function(err) {
        state$connect_error <- conditionMessage(err)
        NULL
      }
    )

    session$onSessionEnded(function() {
      if (!is.null(state$handle)) {
        db_disconnect(state$handle)
      }
    })
  }

  state$conn <- function() {
    if (!state$owns_connection) {
      return(state$handle)
    }

    state$handle <- if (is.null(state$handle)) {
      db_connect(form$db)
    } else {
      sft_live_connection(state$handle, form$db)
    }

    state$handle
  }

  # Accessors ---------------------------------------------------------------
  # The layout form_ui() rendered, reported through a hidden input, unless
  # the caller overrode it. Isolated, so observers do not depend on it.
  state$layout <- function() {
    if (!is.null(form_layout)) {
      return(form_layout)
    }
    value <- shiny::isolate(input$sft_form_layout)
    if (identical(value, "inline")) "inline" else "modal"
  }

  state$current_user <- function() {
    sft_module_current_user(input, user)
  }

  state$permission <- function(name) {
    sft_module_permission(state$permissions[[name]], default = TRUE)
  }

  state$notify <- function(label, type = "warning") {
    shiny::showNotification(sft_ui_label(labels, label), type = type)
  }

  # Run `fun` and turn an error into a notification instead of letting it
  # escape an observer, which would end the Shiny session. For observers that
  # touch the database outside run_mutation(): the connection accessor can
  # fail (server at max_connections), and that must cost the user one action,
  # not the session. Returns TRUE when `fun` completed.
  state$guard <- function(fun) {
    tryCatch(
      {
        sft_with_language(language, fun())
        invisible(TRUE)
      },
      error = function(err) {
        shiny::showNotification(conditionMessage(err), type = "error", duration = 8)
        invisible(FALSE)
      }
    )
  }

  if (!is.null(state$connect_error)) {
    shiny::showNotification(
      sft_ui_label(labels, "db_unavailable", values = list(reason = state$connect_error)),
      type = "error",
      duration = NULL
    )
  }

  # Shared reactive values ---------------------------------------------------
  state$refresh_tick <- shiny::reactiveVal(0L)
  state$restore_record_id <- shiny::reactiveVal(NULL)
  state$record_columns <- shiny::reactiveVal(settings$columns$visible)
  state$record_columns_loaded_for <- shiny::reactiveVal(NULL)
  state$active_column_view <- shiny::reactiveVal("Standard")
  state$current_edit_row <- shiny::reactiveVal(NULL)
  # Edit-conflict state: NULL or list(current_record, columns), set when
  # update_record() rejects a stale edit. The baseline is the record as it
  # looked when editing started (or after a conflict was resolved) - kept
  # separate from current_edit_row so refreshing it never rebuilds the
  # inline form body (which would discard the user's typed values).
  state$edit_conflict <- shiny::reactiveVal(NULL)
  state$edit_conflict_baseline <- shiny::reactiveVal(NULL)
  state$selected_record_id <- shiny::reactiveVal(NULL)
  # Field ids the last rejected save complained about; the highlight registrar
  # glows them. Cleared by the next successful save and whenever a form opens.
  state$invalid_fields <- shiny::reactiveVal(character())
  state$table_structure_tick <- shiny::reactiveVal(0L)
  # Inline (non-modal) add/edit panel state: NULL | "add" | "edit". Single
  # value, so add and edit are mutually exclusive by construction.
  state$inline_active <- shiny::reactiveVal(NULL)

  state$set_record_columns <- function(columns) {
    old <- state$record_columns()
    columns <- as.character(columns %||% character())
    old <- as.character(old %||% character())

    if (!identical(old, columns)) {
      state$record_columns(columns)
      state$table_structure_tick(state$table_structure_tick() + 1L)
    } else {
      state$record_columns(columns)
    }

    invisible(columns)
  }

  state$refresh <- function() {
    state$refresh_tick(state$refresh_tick() + 1L)
  }

  # Re-fetch when an external dependency (another form's `changed` reactive)
  # signals a change, so dependent tables stay in sync.
  external_triggers <- if (is.null(refresh_triggers)) {
    list()
  } else if (is.list(refresh_triggers)) {
    refresh_triggers
  } else {
    list(refresh_triggers)
  }

  for (trigger in external_triggers) {
    local({
      trigger_fn <- trigger
      shiny::observeEvent(
        trigger_fn(),
        state$refresh(),
        ignoreInit = TRUE
      )
    })
  }

  # Run a mutating CRUD action, then on success close the modal, notify, and
  # refresh the table; on error keep the modal open and show the error. Shared
  # by the add / edit / delete submit handlers and both restore flows.
  # `success_values` interpolate into the success label; `on_success` runs
  # after the notification and before the refresh.
  state$run_mutation <- function(action,
                                 success_label,
                                 success_values = list(),
                                 on_success = NULL) {
    tryCatch(
      {
        # Heal a connection the server dropped while the session sat idle
        # (MariaDB wait_timeout) before writing. INSIDE the tryCatch: the
        # reconnect itself can fail (server at max_connections), and then the
        # user must get the message and keep the dialog - an error escaping
        # this observer would end the session and lose the input.
        state$conn()

        # Validation warnings (`warning_if()` rules, `on_edit_missing_required
        # = "warn"`) are raised with base warning(): surface each one as a
        # notification and carry on, since a warning must not block the save.
        # Without this handler they only reach the R console, never the user.
        withCallingHandlers(
          sft_with_language(language, action()),
          warning = function(w) {
            shiny::showNotification(
              conditionMessage(w),
              type = "warning",
              duration = 8
            )
            invokeRestart("muffleWarning")
          }
        )

        shiny::removeModal()
        state$inline_active(NULL)
        state$invalid_fields(character())
        shiny::showNotification(
          sft_ui_label(labels, success_label, values = success_values),
          type = "message"
        )

        if (is.function(on_success)) {
          on_success()
        }

        state$refresh()
      },
      sft_edit_conflict = function(cond) {
        # A stale edit was rejected by update_record(). Keep the dialog open
        # with the user's values; the conflict view (mod_conflict.R) takes
        # over the dialog body until the user decides how to continue.
        state$edit_conflict(
          list(
            current_record = cond$current_record,
            columns = cond$columns
          )
        )
      },
      sft_validation_error = function(cond) {
        # The record was rejected: keep the dialog open, say why, and mark the
        # fields the failed checks name.
        state$invalid_fields(cond$fields %||% character())
        shiny::showNotification(
          conditionMessage(cond),
          type = "error",
          duration = 8
        )
      },
      error = function(err) {
        shiny::showNotification(
          conditionMessage(err),
          type = "error",
          duration = 8
        )
      }
    )
  }

  # Core reactives ----------------------------------------------------------
  state$records <- shiny::reactive({
    state$refresh_tick()

    # Same reconnect guard as run_mutation(): a row selection or refresh after
    # a long idle period must not fail on a server-dropped connection.
    fetch_records(
      form = form,
      conn = state$conn(),
      include_deleted = isTRUE(
        sft_module_include_deleted(
          input = input,
          include_deleted_default = include_deleted_default
        )
      )
    )
  })

  state$display_context <- function() {
    sft_form_context(
      form = form,
      # The context also reaches input-binding observers; a connection that
      # cannot be opened right now yields NULL here instead of an error.
      conn = tryCatch(state$conn(), error = function(err) NULL),
      input = input,
      output = output,
      session = session,
      records = state$records,
      display_records = state$display_records,
      selected_record = state$selected_record,
      refresh = state$refresh,
      user = state$current_user()
    )
  }

  state$display_records <- shiny::reactive({
    sft_apply_display_transform(
      data = state$records(),
      display_transform = display_transform,
      context = state$display_context()
    )
  })

  state$selected_record <- shiny::reactive({
    sft_selected_record_from_display(
      display_data = state$display_records(),
      raw_data = state$records(),
      selected = input$records_rows_selected
    )
  })

  state
}

# Add the reactives a registrar exposes to the module state.
sft_expose <- function(state, exposed) {
  for (name in names(exposed)) {
    assign(name, exposed[[name]], envir = state)
  }

  invisible(state)
}
