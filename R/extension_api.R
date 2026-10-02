# The extension API: what a package built on shinyformtools (a new input
# type with its own module, a module over many records) may rely on. Each
# function is a thin, documented wrapper over an internal helper, so the
# internals can move while these names stay. The grid and the basket of
# shinygridtools are built on it.

#' Fields and values of a form
#'
#' Helpers for code that works with a form's fields: an extension package's
#' module, a validation hook, a custom input. They do what the package itself
#' does, so an extension behaves like a built-in part.
#'
#' @param form Object created with [form()].
#' @param id Field id.
#' @param key Field ids; `key_fields()` returns the matching field objects in
#'   that order and stops when one is unknown.
#' @param record Named list or one-row data frame, by field id or by
#'   database column.
#' @param field A field object (an element of `form$fields`).
#' @param value A value of that field as the input delivers it.
#' @param user The current user, for fields whose `editable` is a function.
#'
#' @return `form_fields()`: the active input fields (list). `find_field()`:
#'   one field or `NULL`. `record_value()`: the record's value for the field
#'   (by id, then by column) or `NULL`. `field_db_value()`: the value as it
#'   is stored. `resolve_editable()`: the form with every function-valued
#'   `editable` resolved for `user`.
#' @name form_helpers
NULL

#' @rdname form_helpers
#' @export
form_fields <- function(form) {
  sft_active_input_fields(form)
}

#' @rdname form_helpers
#' @export
find_field <- function(form, id) {
  sft_find_input_field(form, id)
}

#' @rdname form_helpers
#' @export
key_fields <- function(form, key) {
  sft_key_fields(form, key)
}

#' @rdname form_helpers
#' @export
record_value <- function(record, field) {
  sft_record_get_value(record, field)
}

#' @rdname form_helpers
#' @export
field_db_value <- function(field, value) {
  sft_field_db_value(field, value)
}

#' @rdname form_helpers
#' @export
resolve_editable <- function(form, user) {
  sft_resolve_editable(form, user)
}

#' Database helpers for extensions
#'
#' The parts of the package's database layer an extension needs: a connection
#' that heals itself, callbacks after a transaction, records found by a key,
#' and a cheap fingerprint of a group of records.
#'
#' @param session The Shiny session.
#' @param db A database descriptor, for example `form$db`.
#' @param conn A DBI connection. `module_connection()`: a connection the app
#'   passed in (then it is returned as it is and never replaced), or `NULL`
#'   for a connection the module manages.
#' @param fun A function without arguments.
#' @param form Object created with [form()].
#' @param values Named list of field values that identify the records (field
#'   ids), as [upsert_records()] takes its key.
#' @param user Optional user name written to the schema registration.
#' @param record_id An `sft_id`.
#' @param table_name A table name.
#'
#' @return `module_connection()`: a function `function(probe = TRUE)` that
#'   returns the current connection, reconnecting when the server dropped it
#'   (`probe = FALSE` skips the probe query and returns the handle as it is,
#'   for a frequent poll whose own query fails loudly); shared with the
#'   session's forms unless `options(shinyformtools.share_connections =
#'   FALSE)`. Opening may fail (server full): the function then raises the
#'   error, and the next call tries again. `after_commit()` runs `fun` once
#'   the transaction open on `conn` has committed, at once when none is open;
#'   `after_transaction()` also after a rollback. `in_transaction()`: logical.
#'   `prepare_write()`: the connection, with the form's schema brought up to
#'   date; call it before writing outside the package's own functions.
#'   `find_records_by_key()`: a data frame of the live records whose key
#'   columns hold `values` (empty values match NULL and ''). `fetch_record()`:
#'   one record by id, deleted or not, or `NULL`. `records_stamp()`: a string
#'   that changes whenever a record of the group is inserted, updated,
#'   deleted or restored; poll it to notice other sessions' writes.
#'   `table_exists()`, `db_backend()` (`"sqlite"`, `"duckdb"` or
#'   `"mariadb"`).
#' @name db_helpers
NULL

#' @rdname db_helpers
#' @export
module_connection <- function(session, db, conn = NULL) {
  if (!is.null(conn)) {
    return(function(probe = TRUE) conn)
  }

  if (sft_share_connections()) {
    shared <- sft_session_connection(session, db)
    return(function(probe = TRUE) {
      if (!probe && !is.null(shared$handle)) {
        return(shared$handle)
      }
      sft_session_connection_get(shared, db)
    })
  }

  handle <- tryCatch(db_connect(db), error = function(e) NULL)
  session$onSessionEnded(function() {
    if (!is.null(handle)) try(db_disconnect(handle), silent = TRUE)
  })

  function(probe = TRUE) {
    if (!probe && !is.null(handle)) {
      return(handle)
    }
    handle <<- if (is.null(handle)) db_connect(db) else sft_live_connection(handle, db)
    handle
  }
}

#' @rdname db_helpers
#' @export
after_commit <- function(conn, fun) {
  sft_after_commit(conn, fun)
}

#' @rdname db_helpers
#' @export
after_transaction <- function(conn, fun) {
  sft_after_transaction(conn, fun)
}

#' @rdname db_helpers
#' @export
in_transaction <- function(conn) {
  sft_in_transaction(conn)
}

#' @rdname db_helpers
#' @export
prepare_write <- function(form, conn, user = NULL) {
  sft_prepare_mutation(form, conn, user = user, envir = parent.frame())
}

#' @rdname db_helpers
#' @export
find_records_by_key <- function(form, values, conn) {
  encoded <- sft_encode_key_values(form, values)
  sft_find_live_records(conn, form, encoded)
}

#' @rdname db_helpers
#' @export
fetch_record <- function(form, record_id, conn) {
  sft_get_record(conn, form, record_id = record_id)
}

#' @rdname db_helpers
#' @export
records_stamp <- function(form, values, conn) {
  encoded <- sft_encode_key_values(form, values)
  where <- sft_key_where(conn, form, encoded)

  # Every record's id, update time and delete flag, not MAX(sft_updated_at):
  # the timestamps carry the local UTC offset, so after the autumn clock
  # change (or between processes in different zones) a newer update can sort
  # lower as text, and MAX() would not move. A group is a few dozen rows.
  rows <- sft_query_with_params(
    conn,
    paste0(
      "SELECT sft_id, sft_updated_at, sft_is_deleted FROM ",
      sft_quote_identifier(conn, form$table_name), " WHERE ", where$sql,
      " ORDER BY sft_id"
    ),
    where$params
  )

  paste(rows$sft_id, rows$sft_updated_at, rows$sft_is_deleted, sep = "/", collapse = "|")
}

#' @rdname db_helpers
#' @export
table_exists <- function(conn, table_name) {
  sft_table_exists(conn, table_name)
}

#' @rdname db_helpers
#' @export
db_backend <- function(conn) {
  sft_db_backend(conn)
}

# Key values by field id, encoded as stored and named by database column.
sft_encode_key_values <- function(form, values) {
  if (length(values) == 0L) {
    return(list())
  }
  fields <- sft_key_fields(form, names(values))
  encoded <- lapply(seq_along(fields), function(i) sft_field_db_value(fields[[i]], values[[i]]))
  names(encoded) <- vapply(fields, function(field) field$db_column, character(1))
  encoded
}

#' Validation issues and messages for extensions
#'
#' A `validate` hook of [register_input()] returns a list of issues built
#' with `validation_issue()`; the package reports them like its own
#' (notification, red glow on the fields, `sft_validation_error` in scripts).
#' `form_message()` renders a message key with placeholders, in the active
#' language, honouring `form(messages = )`; register keys of your own with
#' [register_texts()].
#'
#' @param fields Field ids the issue concerns.
#' @param message The message text.
#' @param severity `"error"` (refuses the save) or `"warning"`.
#' @param source A short tag naming the check, shown by [validation_issues()].
#' @param form Object created with [form()].
#' @param key Message key.
#' @param values Named list of placeholder values (`{name}` in the text).
#'
#' @return `validation_issue()`: an issue (list). `form_message()`: a string.
#' @name validation_helpers
NULL

#' @rdname validation_helpers
#' @export
validation_issue <- function(fields, message, severity = "error", source = "extension") {
  if (!severity %in% c("error", "warning")) {
    stop("severity must be \"error\" or \"warning\".", call. = FALSE)
  }
  sft_issue(fields = fields, severity = severity, message = message, source = source)
}

#' @rdname validation_helpers
#' @export
form_message <- function(form, key, values = list()) {
  sft_message(form, key, values)
}

#' Language and labels for extensions
#'
#' `with_language()` evaluates `expr` with `language` (a [language()] object
#' or a function returning one) active, as [form_server()] does around its
#' renders. `ui_labels()` returns the active labels, `ui_label()` one of
#' them with placeholders filled. `register_texts()` adds label and message
#' keys of an extension to a language, so they follow [german()] and
#' [english()] like the package's own; call it in your package's `.onLoad()`.
#'
#' @param language A [language()] object, a function returning one, or `NULL`.
#' @param expr The expression to evaluate.
#' @param labels Optional per-form label overrides (`form_ui(labels = )`).
#' @param key A label key.
#' @param values Named list of placeholder values.
#' @param lang `"en"` or `"de"`.
#' @param messages Named list of message texts.
#'
#' @return `with_language()`: the value of `expr`. `ui_labels()`: a named
#'   list. `ui_label()`: a string, or `NULL` for an unknown key.
#'   `register_texts()`: `NULL`, invisibly.
#' @name language_helpers
NULL

#' @rdname language_helpers
#' @export
with_language <- function(language, expr) {
  sft_with_language(language, expr)
}

#' @rdname language_helpers
#' @export
ui_labels <- function(labels = list()) {
  sft_ui_labels(labels)
}

#' @rdname language_helpers
#' @export
ui_label <- function(key, values = list(), labels = NULL) {
  sft_ui_label(labels %||% sft_ui_labels(), key, values = values)
}

.sft_registered_texts <- new.env(parent = emptyenv())

#' @rdname language_helpers
#' @export
register_texts <- function(lang, labels = list(), messages = list()) {
  if (!lang %in% c("en", "de")) {
    stop("lang must be \"en\" or \"de\".", call. = FALSE)
  }
  for (part in list(labels, messages)) {
    if (!is.list(part) || (length(part) > 0L && (is.null(names(part)) || !all(nzchar(names(part)))))) {
      stop("labels and messages must be named lists.", call. = FALSE)
    }
  }

  current <- sft_registered_texts(lang)
  current$labels <- utils::modifyList(current$labels, labels)
  current$messages <- utils::modifyList(current$messages, messages)
  assign(lang, current, envir = .sft_registered_texts)
  invisible(NULL)
}

sft_registered_texts <- function(lang) {
  if (exists(lang, envir = .sft_registered_texts, inherits = FALSE)) {
    get(lang, envir = .sft_registered_texts, inherits = FALSE)
  } else {
    list(labels = list(), messages = list())
  }
}

#' Parts of form_ui() an extension provides
#'
#' Some switches of [form_ui()] draw a part that an extension package
#' provides: `show_board = TRUE` draws the part named `"board"` (the basket
#' board of \pkg{shinygridtools}). The extension registers the part when it
#' loads; without one, the switch stops with a message naming the package to
#' attach.
#'
#' @param name The part's name. Known to [form_ui()]: `"board"`.
#' @param fun `function(id, labels)` returning the UI: `id` is the form
#'   module's id, `labels` the labels [form_ui()] resolved in its language
#'   (including those the extension registered with [register_texts()]).
#'
#' @return `NULL`, invisibly.
#' @export
register_ui_part <- function(name, fun) {
  if (!sft_is_scalar_character(name)) {
    stop("name must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.function(fun)) {
    stop("fun must be a function(id, labels).", call. = FALSE)
  }
  assign(name, fun, envir = .sft_ui_parts)
  invisible(NULL)
}

.sft_ui_parts <- new.env(parent = emptyenv())

# The registered part, or an error that says which switch needs it.
sft_ui_part <- function(name) {
  if (!exists(name, envir = .sft_ui_parts, inherits = FALSE)) {
    switch_name <- c(board = "form_ui(show_board = TRUE)")[[name]] %||% paste0("The UI part '", name, "'")
    stop(sft_moved_message(switch_name), call. = FALSE)
  }
  get(name, envir = .sft_ui_parts, inherits = FALSE)
}

#' Module helpers for extensions
#'
#' Functions for a `server` hook of [register_input()] and for modules that
#' sit beside [form_server()]. `state` is the module state the hook receives
#' (see [module_state]).
#'
#' @param state The module state.
#' @param permissions The `permissions` bundle of [form_server()] or
#'   `shinygridtools::grid_server()` (`state$permissions` inside a hook).
#' @param name A permission name (`"can_edit"`, ...).
#' @param record_id The `sft_id` of the record being changed.
#' @param values Named list of field values to write.
#'
#' @return `module_permission()`: logical, `FALSE` when the permission
#'   function fails. `module_editable_fields()`: the field ids this user may
#'   edit, or `NULL` for no restriction. `apply_edit_rules()`: a list with
#'   `values` and `form`, ready for [update_record()], after the dialog's
#'   rules: locked derived fields recomputed from the new values, fields a
#'   `dynamic_visibility()` binding hides on that record left out (so an
#'   extension never writes what the dialog would not).
#' @name module_helpers
NULL

#' @rdname module_helpers
#' @export
module_permission <- function(permissions, name) {
  isTRUE(sft_module_permission(permissions[[name]], default = TRUE))
}

#' @rdname module_helpers
#' @export
module_editable_fields <- function(permissions) {
  sft_module_editable_fields(permissions$editable_fields)
}

#' @rdname module_helpers
#' @export
apply_edit_rules <- function(state, record_id, values) {
  form <- state$form
  bindings <- state$input_bindings
  context <- state$display_context()
  resolved <- sft_resolve_editable(form, state$current_user())

  if (length(bindings) == 0L) {
    return(list(values = values, form = resolved))
  }

  stored <- sft_get_record(state$conn(), form, record_id = record_id)
  derived <- sft_recompute_derived_on_edit(
    input_bindings = bindings, form = form, values = values,
    stored_record = stored, context = context
  )
  kept <- sft_drop_hidden_field_values(
    bindings, resolved, derived$values, basis = derived$overlay, context = context
  )
  resolved$fields <- lapply(resolved$fields, function(field) {
    if (field$id %in% derived$derived) {
      field$editable <- TRUE
    }
    field
  })

  list(values = kept, form = resolved)
}

#' The module state a `server` hook receives
#'
#' A `server` hook registered with [register_input()] runs inside
#' [form_server()] as `function(input, output, session, state, fields)`,
#' once per form that has a field of the type, with `fields` those fields.
#' `state` is an environment; these members are the contract an extension
#' may use, the rest is internal and may change:
#'
#' \describe{
#'   \item{`form`}{The form object.}
#'   \item{`conn()`}{The module's connection, healed when the server dropped
#'     it; raises an error when none can be opened.}
#'   \item{`current_user()`}{The user name as [form_server()] resolves it.}
#'   \item{`permission(name)`}{A permission of the `permissions` bundle,
#'     `FALSE` when its function fails.}
#'   \item{`permissions`}{The bundle itself (for `editable_fields`).}
#'   \item{`records()`}{Reactive: the records the table shows (without the
#'     display transform).}
#'   \item{`refresh()`}{Reload the records and the table.}
#'   \item{`saved_tick`}{reactiveVal counting the module's own writes; bump
#'     it after a write of your own so `saved` moves.}
#'   \item{`guard(fun)`}{Runs `fun` inside the language scope and turns an
#'     error into a notification instead of ending the session; returns
#'     `TRUE` when `fun` completed.}
#'   \item{`display_context()`}{The [hook_context] for bindings.}
#'   \item{`current_edit_row()`}{Reactive: the record the edit dialog shows,
#'     or `NULL`.}
#'   \item{`input_bindings`, `labels`, `language`}{As passed to
#'     [form_server()]; `labels` is an active binding following the
#'     language.}
#' }
#'
#' Inputs of the module are namespaced: `input$open_add`, `input$open_edit`
#' fire when a dialog opens; the dialog's inputs are `input[[paste0("add_",
#' id)]]` / `input[[paste0("edit_", id)]]`.
#' @name module_state
#' @aliases module_state
NULL
