# Help topics that describe a convention shared by many functions rather than
# one function.

#' How functions use a database connection
#'
#' Every function that reads or writes records takes an optional `conn`
#' argument. The rules are the same everywhere.
#'
#' @section Without `conn`:
#' The function opens a connection from `form$db` with [db_connect()], uses it
#' for this one call and closes it again before it returns, also when the call
#' fails. This is the convenient form for scripts. Each call pays for one
#' connect, and on a remote server that is usually the most expensive part of
#' the call.
#'
#' @section With `conn`:
#' The function uses your connection and leaves it open. Opening and closing it
#' is your job: `conn <- db_connect(db)` at the start and [db_disconnect()] at
#' the end, for example in `on.exit()`. Pass `conn` when a script makes several
#' calls in a row, and always for an in-memory database, where every new
#' connection would see an empty database.
#'
#' A function never closes or replaces a connection it was given. Each mutating
#' call runs in its own transaction, so do not call one inside a transaction you
#' opened yourself on the same connection.
#'
#' @section In a Shiny app:
#' [form_server()] follows the same rule with a longer lifetime. Without `conn`
#' it uses one connection per session and database, shared by every
#' [form_server()] (and `shinygridtools::grid_server()`) of the session on
#' that database. It is
#' closed when the session ends and reopened when the server has dropped it,
#' for example after MariaDB's `wait_timeout`. The current connection is
#' available as `connection()` in the module's return value. A connection you
#' pass in is never reopened for you.
#'
#' A page with three forms therefore holds one connection per user.
#' `options(shinyformtools.share_connections = FALSE)` gives every form its
#' own again, as before 0.5.0.
#'
#' @section Schema checks:
#' With either form, the first thing a call does is a cheap check that the
#' tables match the form definition. Tables are created or extended only when
#' they do not. See [init_db()] and `form(schema_policy = "manual")` to take
#' that step into your own hands.
#'
#' @section Updating an app:
#' Each running app adapts the tables to ITS form definition. When a new
#' version of an app changes fields while an old version still runs on the
#' same database, the two keep changing the tables back and forth: the old
#' one retires a new column and drops a new unique index, the new one adds
#' them again, and in between the database does not enforce uniqueness. So
#' make sure no old version is left: Shiny Server keeps the R processes of
#' open sessions running after the app's files were replaced, so restart the
#' service (for example `sudo systemctl restart shiny-server`), and stop the
#' app on every other machine that uses the same database.
#'
#' Going back to an older version of the package works as long as the form is
#' unchanged. One exception: 0.4.1 turned the columns of range sliders on
#' DuckDB and MariaDB into text, which older versions read as a type change once the
#' form differs there; such a database stays with 0.4.1 or newer.
#'
#' @seealso [db_connect()], [db_disconnect()], [form_server()]
#' @name connections
#' @aliases connections
NULL

#' The `context` object passed to hooks
#'
#' Several arguments of [form_server()] take a function that the module calls
#' while it runs: `display_transform`, `modal_header`, `table$format` and the
#' handlers of `input_bindings`. When such a function declares an argument named
#' `context`, it receives the list described here. It gives the hook access to
#' the form, the database and the module's current state, so that it can look
#' up related records or react to the selected row.
#'
#' @section Elements:
#' \describe{
#'   \item{`form`}{The form object the module was started with.}
#'   \item{`conn`}{The module's database connection, or `NULL` when the
#'     database cannot be reached at the moment. Use it for your own queries,
#'     for example `fetch_records(other_form, conn = context$conn)`. Do not
#'     close it.}
#'   \item{`user`}{The current user name as a string, as resolved from the
#'     `user` argument.}
#'   \item{`records`}{A reactive: the records as fetched from the database.
#'     Call it as `context$records()`.}
#'   \item{`display_records`}{A reactive: the records after
#'     `display_transform`, which is what the table shows. Do not call it
#'     inside `display_transform` itself.}
#'   \item{`selected_record`}{A reactive: the one-row data frame selected in
#'     the table, or `NULL`.}
#'   \item{`refresh`}{A function without arguments that makes the module fetch
#'     its records again.}
#'   \item{`input`, `output`, `session`}{The module's own Shiny objects. Input
#'     ids are already namespaced, so `context$input$open_add` is the module's
#'     add button.}
#' }
#'
#' @section Reactivity:
#' The reactives are handed over uncalled. A hook that calls one inside a
#' reactive context, which is where the module runs its hooks, takes a
#' dependency on it and runs again when the value changes.
#'
#' @examples
#' # A display_transform that replaces a stored id by a name from another form.
#' show_department <- function(data, context) {
#'   departments <- fetch_records(department_form, conn = context$conn)
#'   data$department <- departments$name[match(data$department, departments$sft_id)]
#'   data
#' }
#' @seealso [form_server()]
#' @name hook_context
#' @aliases hook_context
NULL
