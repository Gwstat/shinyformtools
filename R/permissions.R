# Adapter from a shinymanager auth object to form_server() permissions.

# Coerce a credential value (often the character "TRUE"/"FALSE" that
# shinymanager stores for extra columns) to a single logical.
sft_truthy <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(isTRUE(default))
  }

  if (is.logical(x)) {
    return(isTRUE(x[[1L]]))
  }

  value <- tolower(trimws(as.character(x[[1L]])))
  value %in% c("true", "t", "1", "yes", "y")
}

# The permission arguments of form_server() that this adapter fills, in the
# order they appear there. Each maps to a same-named credential column unless
# overridden via `mapping`.
sft_permission_fields <- function() {
  c(
    "can_add",
    "can_view_record",
    "can_edit",
    "can_delete",
    "can_restore",
    "can_view_versions",
    "can_view_deleted_records",
    "can_change_column_settings",
    "can_select_column_view",
    "can_view_audit"
  )
}

#' Map a shinymanager auth object to form_server() permissions
#'
#' Turn the authentication result returned by `shinymanager::secure_server()`
#' into a list of permission functions ready to spread into one or more
#' [form_server()] calls with `do.call()`. This keeps the per-user wiring in
#' one place instead of hand-writing a closure for every `can_*` argument of
#' every form, which matters when several tables are driven by the same login.
#'
#' Each permission reads a credential column from `auth` reactively, so the
#' returned functions stay in sync as `auth` updates. A column is treated as
#' granted when its value is truthy (`TRUE`, `"TRUE"`, `"1"`, `"yes"`, ...);
#' a missing column falls back to `default`.
#'
#' The result is a named list whose names match the arguments of
#' [form_server()] (`user`, `can_add`, `can_view_record`, `can_edit`,
#' `can_delete`, `can_restore`, `can_view_versions`, `can_view_deleted_records`,
#' `can_change_column_settings`, `can_select_column_view`, `can_view_audit`), so
#' it can be spread directly:
#' `do.call(form_server, c(list(id = "x", form = form), perms))`.
#'
#' To drive several tables from one login, call the adapter once and spread the
#' same `perms` into each form. For table-specific rights, pass a `mapping` that
#' points a permission at a table-specific credential column, for example
#' `mapping = c(can_edit = "can_edit_locations")`.
#'
#' @param auth The object returned by `shinymanager::secure_server()` (a
#'   `reactiveValues`). Any object that supports `[[` lookup of the credential
#'   columns is accepted, which makes the adapter testable without shinymanager.
#' @param mapping Optional named character vector or list remapping permission
#'   names (and `user`) to credential column names. Names are permission fields;
#'   values are the columns to read. Unmapped permissions read the same-named
#'   column.
#' @param default Logical fallback used when a credential column is absent or
#'   `NA`. Defaults to `FALSE` (deny by default).
#' @param user_field Name of the credential column holding the user identifier.
#' @param user_default User identifier returned when the user column is empty.
#'
#' @return A named list of functions suitable for spreading into
#'   [form_server()].
#'
#' @seealso [form_server()]
#' @examples
#' # The adapter accepts any object supporting `[[` lookup, so a plain named
#' # list stands in for a shinymanager auth object outside a Shiny session.
#' auth <- list(user = "alice", can_add = "TRUE", can_edit = "FALSE")
#' perms <- shinymanager_permissions(auth)
#' perms$user()
#' perms$can_add()
#' perms$can_edit()
#' # Point a permission at a table-specific credential column:
#' auth2 <- list(user = "bob", can_edit_locations = "yes")
#' perms2 <- shinymanager_permissions(auth2, mapping = c(can_edit = "can_edit_locations"))
#' perms2$can_edit()
#' @export
shinymanager_permissions <- function(auth,
                                         mapping = NULL,
                                         default = FALSE,
                                         user_field = "user",
                                         user_default = NA_character_) {
  if (is.null(auth)) {
    stop("auth must be the result of shinymanager::secure_server().", call. = FALSE)
  }

  mapping <- if (is.null(mapping)) {
    list()
  } else {
    as.list(mapping)
  }

  if (length(mapping) > 0L && is.null(names(mapping))) {
    stop("mapping must be a named character vector or list.", call. = FALSE)
  }

  column_for <- function(field) {
    mapped <- mapping[[field]]
    if (is.null(mapped)) field else as.character(mapped)
  }

  permission_fn <- function(field) {
    column <- column_for(field)
    force(column)
    function() {
      sft_truthy(auth[[column]], default = default)
    }
  }

  user_column <- column_for(user_field)
  perms <- list(
    user = function() {
      value <- auth[[user_column]]
      if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) ||
          !nzchar(as.character(value[[1L]]))) {
        return(user_default)
      }
      as.character(value[[1L]])
    }
  )

  for (field in sft_permission_fields()) {
    perms[[field]] <- permission_fn(field)
  }

  perms
}

# Default human labels for the permission checkboxes in a permissions form.
sft_permission_labels <- function() {
  c(
    can_add = "Add",
    can_view_record = "Open",
    can_edit = "Edit",
    can_delete = "Delete",
    can_restore = "Restore",
    can_view_versions = "View versions",
    can_view_deleted_records = "View deleted",
    can_change_column_settings = "Manage columns",
    can_select_column_view = "Switch column view",
    can_view_audit = "View audit"
  )
}

#' A ready-made rights table as a shinyformtools form
#'
#' Build a declarative form whose records are *permission rules*: one row grants
#' a set of permissions to one user across one or more forms. This replaces the
#' "one credential column per permission per table" approach - adding a new
#' table just adds an entry to the `forms` multi-select, never a new column.
#'
#' Each rule has a `user`, a `forms` field (a multi-select of form ids, stored
#' natively as a JSON array), and one checkbox per permission. Resolve the rules
#' for a given user and form with [rights_permissions()] and spread the result
#' into [form_server()]. Because the rights table is an ordinary form, it is
#' audited, soft-deletable, and reactive, and you can render it anywhere with
#' [form_ui()] / [form_server()] (for example an admin-only tab).
#'
#' @param form_ids Character vector of form ids a rule may apply to (the choices
#'   of the `forms` multi-select).
#' @param db,db_path Database spec or path, as in [form()]. Supply one.
#' @param users Character vector of user names for the `user` field choices,
#'   for example from [shinymanager_users()]. Choices can also be refreshed at
#'   runtime with a `dynamic_choices()` input binding.
#' @param permissions Character vector of permission field names. Defaults to
#'   the full set understood by [form_server()].
#' @param form_id,table_name,form_name Identifiers for the rights form/table.
#' @param permission_labels Optional named vector overriding the checkbox labels
#'   (names are permission fields).
#'
#' @return An `sft_form` object.
#' @seealso [rights_permissions()], [shinymanager_users()]
#' @examples
#' # Build a rights table covering two data forms, backed by a temporary DB.
#' rights <- permissions_form(
#'   form_ids = c("people", "locations"),
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   users = c("alice", "bob")
#' )
#' rights$form_id
#' vapply(rights$fields, function(f) f$id, character(1))
#' @export
permissions_form <- function(form_ids,
                             db = NULL,
                             db_path = NULL,
                             users = character(),
                             permissions = NULL,
                             form_id = "permissions",
                             table_name = "permissions",
                             form_name = "Permissions",
                             permission_labels = NULL) {
  permissions <- permissions %||% sft_permission_fields()
  labels <- utils::modifyList(
    as.list(sft_permission_labels()),
    as.list(permission_labels %||% list())
  )

  fields <- list(
    form_field(
      id = "user", label = "User", input_type = "selectizeInput",
      args = list(choices = as.character(users), options = list(placeholder = "Pick a user")),
      mandatory = TRUE, col = 1, pos = 1
    ),
    form_field(
      id = "forms", label = "Forms", input_type = "selectizeInput",
      args = list(
        choices = as.character(form_ids), multiple = TRUE,
        options = list(placeholder = "Pick one or more forms")
      ),
      mandatory = TRUE, col = 1, pos = 2
    )
  )

  for (i in seq_along(permissions)) {
    perm <- permissions[[i]]
    fields <- c(fields, list(
      form_field(
        id = perm, label = labels[[perm]] %||% perm,
        input_type = "checkboxInput", args = list(value = FALSE),
        col = 2, pos = i
      )
    ))
  }

  form_args <- list(
    form_id = form_id, form_name = form_name, table_name = table_name,
    fields = fields
  )
  # Pass only the database argument that was supplied; form() keeps its own
  # db_path default (and validates it), with db overriding the connection.
  if (!is.null(db)) form_args$db <- db
  if (!is.null(db_path)) form_args$db_path <- db_path

  do.call(form, form_args)
}

#' Resolve permission rules for a user and form
#'
#' Turn the records of a [permissions_form()] into the `can_*` functions that
#' [form_server()] expects, for a specific user and form id. A permission is
#' granted when *any* non-deleted rule for that user, whose `forms` list
#' contains `form_id`, has the permission checked. The result is a named list
#' ready to spread with `do.call()`, exactly like [shinymanager_permissions()].
#'
#' `rules` may be a reactive (or function) so the returned permissions stay live
#' as rules are edited - which, together with `form_server(hide_forbidden =
#' TRUE)`, updates the visible buttons immediately.
#'
#' @param rules A data frame of rights records, or a reactive/function returning
#'   one (for example the `records` element returned by [form_server()]).
#' @param user The current user identifier, or a function/reactive returning it.
#' @param form_id The form id to resolve permissions for.
#' @param permissions Permission field names to resolve. Defaults to the full
#'   set understood by [form_server()].
#' @param default Logical granted when no rule matches. Defaults to `FALSE`.
#' @param superuser Logical or function/reactive; when `TRUE` every permission
#'   is granted (use for admins, e.g. `function() isTRUE(auth$admin)`).
#' @param forms_field,user_field Column names of the multi-form and user fields.
#'
#' @return A named list of functions suitable for spreading into [form_server()].
#' @seealso [permissions_form()], [shinymanager_permissions()]
#' @examples
#' # A rules table: each row grants permissions to a user across one or more
#' # forms (the `forms` column is a JSON array, as stored by permissions_form()).
#' rules <- data.frame(
#'   user = c("alice", "bob"),
#'   forms = c('["people"]', '["locations"]'),
#'   can_add = c(TRUE, FALSE),
#'   can_edit = c(TRUE, FALSE),
#'   stringsAsFactors = FALSE
#' )
#' perms <- rights_permissions(rules, user = "alice", form_id = "people")
#' perms$can_add()   # TRUE  (alice has a people rule with can_add)
#' perms$can_edit()  # TRUE
#'
#' # bob has no rule for "people", so a missing permission falls back to default.
#' bob <- rights_permissions(rules, user = "bob", form_id = "people")
#' bob$can_add()     # FALSE
#'
#' # superuser grants everything regardless of the rules.
#' admin <- rights_permissions(rules, user = "carol", form_id = "people",
#'                             superuser = TRUE)
#' admin$can_delete()
#' @export
rights_permissions <- function(rules,
                               user,
                               form_id,
                               permissions = NULL,
                               default = FALSE,
                               superuser = FALSE,
                               forms_field = "forms",
                               user_field = "user") {
  permissions <- permissions %||% sft_permission_fields()
  force(form_id)

  current_user <- function() if (is.function(user)) user() else user
  is_super <- function() if (is.function(superuser)) isTRUE(superuser()) else isTRUE(superuser)
  resolve_rules <- function() {
    df <- if (is.function(rules)) rules() else rules
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
      return(NULL)
    }
    df
  }

  matching_rules <- function() {
    df <- resolve_rules()
    if (is.null(df)) return(NULL)

    u <- current_user()
    keep <- !is.na(df[[user_field]]) & as.character(df[[user_field]]) == u
    if ("sft_is_deleted" %in% names(df)) {
      keep <- keep & sft_parse_logical(df[["sft_is_deleted"]]) %in% c(FALSE, NA)
      keep[is.na(keep)] <- FALSE
    }
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0L) return(NULL)

    in_form <- vapply(
      df[[forms_field]],
      function(v) form_id %in% as.character(sft_parse_json_vector(v)),
      logical(1)
    )
    df <- df[in_form, , drop = FALSE]
    if (nrow(df) == 0L) NULL else df
  }

  permission_fn <- function(perm) {
    force(perm)
    function() {
      if (is_super()) return(TRUE)
      df <- matching_rules()
      if (is.null(df) || !(perm %in% names(df))) {
        return(isTRUE(default))
      }
      any(vapply(df[[perm]], function(x) sft_truthy(x, default = FALSE), logical(1)))
    }
  }

  perms <- list(user = function() current_user())
  for (perm in permissions) {
    perms[[perm]] <- permission_fn(perm)
  }
  perms
}

# Coerce a stored flag column to logical (handles 0/1, "0"/"1", TRUE/FALSE).
sft_parse_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

#' User names from a shinymanager credential database
#'
#' Read the user column from a shinymanager credential database (or a plain
#' credentials data frame) for use as the `users` choices of a
#' [permissions_form()].
#'
#' @param db Path to the encrypted credential SQLite database, or a data frame
#'   of credentials (the latter is handy for tests).
#' @param passphrase Passphrase for the encrypted database (ignored for a data
#'   frame).
#' @param user_field Name of the user column.
#'
#' @return A sorted character vector of unique user names.
#' @seealso [permissions_form()], [shinymanager_permissions()]
#' @examples
#' # From a plain credentials data frame (handy for tests / non-shinymanager use):
#' creds <- data.frame(user = c("bob", "alice", "alice"), stringsAsFactors = FALSE)
#' shinymanager_users(creds)
#'
#' # From an encrypted shinymanager credential database on disk:
#' \dontrun{
#' shinymanager_users("credentials.sqlite", passphrase = "secret")
#' }
#' @export
shinymanager_users <- function(db, passphrase = NULL, user_field = "user") {
  if (is.data.frame(db)) {
    return(sort(unique(as.character(db[[user_field]]))))
  }

  if (!requireNamespace("shinymanager", quietly = TRUE)) {
    stop("Package 'shinymanager' is required to read a credential database.", call. = FALSE)
  }
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required to read a credential database.", call. = FALSE)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  creds <- shinymanager::read_db_decrypt(conn, name = "credentials", passphrase = passphrase)
  sort(unique(as.character(creds[[user_field]])))
}
