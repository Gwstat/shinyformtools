# Public form module UI and server entry points.

#' Form module UI
#'
#' The defaults are deliberately lean: the records table and the add / edit /
#' delete buttons, and nothing else. Everything beyond that CRUD core -- the
#' audit log, the deleted-records dialog, the column buttons and the user field
#' -- is opt-in, so a plain `form_ui(id)` yields a plain table rather than a
#' wall of controls. Switch on what the app needs with the `show_*` arguments.
#'
#' @param id Module id.
#' @param title Optional title shown above the module.
#' @param show_user Logical. Whether to show a user input field. Off by default:
#'   in most apps the acting user comes from authentication, not a text box.
#' @param show_audit Logical. Whether to show the audit log table. Off by
#'   default. Every mutation is still recorded either way; this only controls
#'   whether the log is displayed. Requires the `can_view_audit` permission.
#' @param show_add Logical. Whether to show the add button.
#' @param show_edit Logical. Whether to show the edit button.
#' @param show_deleted_records Logical. Whether to show the deleted-records
#'   dialog button, from which soft-deleted records can be restored. Off by
#'   default. Records are still only ever soft-deleted; this only controls
#'   whether the restore dialog is reachable.
#' @param show_delete Logical. Whether to show the delete button.
#' @param show_refresh_table Logical. Whether to show a button that clears table filters, ordering, paging and row selection.
#' @param show_column_settings Logical. Whether to show the admin
#'   column-settings button. Off by default.
#' @param show_column_selection Logical. Whether to show the user
#'   column-selection button. Off by default.
#' @param form_layout Where the add/edit forms render. `"modal"` (default) opens
#'   them in a dialog; `"inline"` renders them in a panel above the records table,
#'   with add and edit mutually exclusive. [form_server()] picks the layout up
#'   from the UI, so it only has to be set here.
#' @param table_style Visual preset for the module's tables (records, audit,
#'   deleted records, versions). One of `"classic"` (the unmodified 'DT' look),
#'   `"clean"` (card-style with row separators instead of stripes),
#'   `"publication"` (a booktabs-style journal table: serif type, horizontal
#'   rules only, no stripes) or `"compact"` (dense, dark header). The default `NULL`
#'   consults `getOption("shinyformtools.table_style")` and falls back to
#'   `"classic"`, so an app can set the look once and override it per form.
#'   Purely cosmetic CSS, scoped to this module's tables; host-app tables are
#'   unaffected.
#' @param labels Optional named list overriding UI labels and button texts.
#' @param button_options Optional named list controlling action-button placement
#'   and classes. Supported entries are `placement` (`"top"`, `"bottom"`,
#'   `"both"`, `"none"`), `align` (`"left"`, `"center"`,
#'   `"right"`, `"between"`), `class`, `container_class`,
#'   `container_style` and `button_classes`.
#' @param ... Deprecated arguments, accepted with a warning for one release:
#'   `show_include_deleted` (the legacy include-deleted checkbox; use
#'   `show_deleted_records` instead) and `show_versions` (no effect; versions
#'   are shown inside the edit dialog when permitted).
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' ui <- fluidPage(form_ui("contacts", title = "Contacts"))
#' server <- function(input, output, session) {
#'   form_server(id = "contacts", form = contacts)
#' }
#' shinyApp(ui, server)
#' }
#' @export
form_ui <- function(id,
                        title = NULL,
                        show_user = FALSE,
                        show_audit = FALSE,
                        show_add = TRUE,
                        show_edit = TRUE,
                        show_deleted_records = FALSE,
                        show_delete = TRUE,
                        show_refresh_table = TRUE,
                        show_column_settings = FALSE,
                        show_column_selection = FALSE,
                        form_layout = c("modal", "inline"),
                        table_style = NULL,
                        labels = list(),
                        button_options = list(),
                        ...) {
  ns <- shiny::NS(id)
  form_layout <- match.arg(form_layout)

  legacy <- sft_map_deprecated_args(
    dots = list(...),
    mapping = list(
      show_include_deleted = list(bundle = NULL, key = "show_include_deleted"),
      show_versions = list(bundle = NULL, key = NULL)
    ),
    fn = "form_ui"
  )
  show_include_deleted <- isTRUE(legacy$args$show_include_deleted)
  table_style <- sft_resolve_table_style(table_style)
  labels <- sft_ui_labels(labels)
  button_options <- sft_normalize_button_options(button_options)

  button_row <- function() {
    sft_form_button_row(
      ns = ns,
      labels = labels,
      button_options = button_options,
      show_add = show_add,
      show_edit = show_edit,
      show_delete = show_delete,
      show_refresh_table = show_refresh_table,
      show_deleted_records = show_deleted_records,
      show_column_settings = show_column_settings,
      show_column_selection = show_column_selection
    )
  }

  top_buttons <- if (button_options$placement %in% c("top", "both")) {
    button_row()
  }

  bottom_buttons <- if (button_options$placement %in% c("bottom", "both")) {
    button_row()
  }

  shiny::tagList(
    shinyjs::useShinyjs(),
    sft_button_css(),
    sft_highlight_css(ns),
    sft_table_style_css(ns, table_style),

    if (!is.null(title)) {
      shiny::h3(title)
    },

    if (isTRUE(show_user)) {
      shiny::textInput(
        inputId = ns("sft_user"),
        label = sft_ui_label(labels, "user_label"),
        value = Sys.info()[["user"]]
      )
    },

    top_buttons,

    if (isTRUE(show_include_deleted)) {
      shiny::checkboxInput(
        inputId = ns("include_deleted"),
        label = sft_ui_label(labels, "include_deleted"),
        value = FALSE
      )
    },

    if (identical(form_layout, "inline")) {
      shiny::uiOutput(ns("sft_inline_form"))
    },

    # Tells form_server() which layout the UI rendered, so the two never have
    # to be kept in sync by hand. A hidden text input is still bound and sent.
    shiny::div(
      style = "display: none;",
      shiny::textInput(ns("sft_form_layout"), label = NULL, value = form_layout)
    ),

    shiny::hr(),

    shiny::div(
      id = ns("records_container"),
      class = "sft-records-table",
      DT::DTOutput(ns("records"))
    ),

    bottom_buttons,

    if (isTRUE(show_audit)) {
      shiny::div(
        id = ns("audit_container"),
        shiny::hr(),
        shiny::h4(sft_ui_label(labels, "audit_title")),
        DT::DTOutput(ns("audit"))
      )
    }
  )
}

#' Form module server
#'
#' Options come in four named lists, each with defaults, so a call only names
#' what it changes: `permissions` (who may do what), `table` (how the DT tables
#' look), `columns` (which columns show, saved views) and `highlight` (glow
#' effects in the forms). The permission adapters [shinymanager_permissions()]
#' and [rights_permissions()] return a `permissions` list directly.
#'
#' @param id Module id.
#' @param form Object created with [form()].
#' @param conn Optional existing DBI connection. When `NULL` the module opens
#'   its own connection from `form$db`, closes it when the session ends, and
#'   reopens it if the server drops it (see `connection()` in the return
#'   value). A connection you pass in is yours: it is never closed or replaced.
#' @param user Optional user identifier or function returning a user
#'   identifier. Falls back to `permissions$user` when the adapters supply one.
#' @param permissions Named list of permissions. Each entry is a logical, or a
#'   function / reactive returning one, and defaults to `TRUE`:
#'   \describe{
#'     \item{`can_add`}{Add records.}
#'     \item{`can_view_record`}{Open the selected record in the read-only/edit
#'       dialog.}
#'     \item{`can_edit`}{Save changes to an opened record.}
#'     \item{`can_delete`}{Soft-delete records.}
#'     \item{`can_restore`}{Restore deleted records and previous versions.}
#'     \item{`can_view_versions`}{Open the versions accordion.}
#'     \item{`can_view_deleted_records`}{Open the deleted-records dialog.}
#'     \item{`can_change_column_settings`}{Open and save shared column
#'       settings (admins).}
#'     \item{`can_select_column_view`}{Load an existing column view.}
#'     \item{`can_view_audit`}{See the audit-log table (only effective with
#'       `form_ui(show_audit = TRUE)`); when `FALSE` no audit rows reach the
#'       client.}
#'     \item{`can_view_table`}{See the records table at all; when `FALSE` the
#'       table is hidden and no rows are sent.}
#'     \item{`can_reset_table`}{Use the reset/refresh button.}
#'     \item{`hide_forbidden`}{Logical (default `TRUE`). Hide every control
#'       whose permission is `FALSE` reactively. The server-side guards stay in
#'       force regardless, so a hidden control can never trigger its action.}
#'     \item{`editable_fields`}{Optional character vector of field ids the
#'       current user may edit (or a function/reactive returning one). Every
#'       other input is rendered read-only in the edit dialog and ignored on
#'       save. `NULL` (default) keeps each field's own `editable` setting; an
#'       empty vector locks every input.}
#'     \item{`user`}{Optional; used as `user` when that argument is `NULL`.}
#'   }
#' @param table Named list of table display settings:
#'   \describe{
#'     \item{`options`}{Additional DT options for the records table.}
#'     \item{`class`}{CSS class passed to [DT::datatable()] (default
#'       `"display compact stripe hover"`).}
#'     \item{`filter`}{Per-column search controls: `"none"` (default), `"top"`
#'       or `"bottom"`.}
#'     \item{`format`}{Optional function for display-only DT formatting. May
#'       declare any of `table`, `data` and `context` and must return a DT
#'       widget, e.g. `function(table) DT::formatStyle(table, "Status", ...)`.}
#'     \item{`audit_options`, `version_options`, `deleted_records_options`}{
#'       Additional DT options for the audit, versions and deleted-records
#'       tables.}
#'     \item{`datetime_format`}{Format for displayed timestamps. They are
#'       rendered in the time zone of the machine running the app; set
#'       `options(shinyformtools.datetime_timezone = "UTC")` (or any Olson
#'       name) to pin it. Values stored without an offset, such as a user's
#'       own date field, are never shifted.}
#'   }
#' @param columns Named list controlling which columns the records table shows:
#'   \describe{
#'     \item{`visible`}{Optional character vector of columns shown by default.}
#'     \item{`views`}{Optional named list of predefined views, each a character
#'       vector of database column names (or a function returning such a list).}
#'     \item{`default_view`}{Default view name (default `"Standard"`); a
#'       scalar or a function/reactive returning one.}
#'     \item{`persist`}{Logical (default `TRUE`). Store per-user column choices
#'       in the form database.}
#'     \item{`show_system`}{Logical (default `FALSE`). Offer the extended
#'       system columns.}
#'     \item{`labels`}{Optional named character vector with labels for
#'       display-only columns created by `display_transform`.}
#'   }
#' @param highlight Named list controlling the glow effects in the add/edit
#'   forms:
#'   \describe{
#'     \item{`fields`}{Character vector of field ids to glow, or a
#'       function/reactive returning one; an empty vector clears it.}
#'     \item{`tab`}{Logical (default `TRUE`). Also glow the tab containing a
#'       highlighted or changed field.}
#'     \item{`color`}{Glow colour for `fields` (default `"#dc3545"`).}
#'     \item{`show_changed`}{Logical (default `TRUE`). Glow edit-form fields
#'       whose value differs from the value at creation.}
#'     \item{`changed_color`}{Glow colour for `show_changed` (default
#'       `"#2b8cff"`).}
#'     \item{`invalid`}{Logical (default `TRUE`). After a save was rejected,
#'       glow the fields the failed checks name (missing mandatory fields,
#'       taken unique values, the `fields` of a failed rule) in `color`, until
#'       the next successful save or until a form is opened again.}
#'   }
#' @param labels Optional named list overriding UI labels, modal texts and
#'   notification messages. Set individual entries to `NULL` to hide the
#'   corresponding button or modal title.
#' @param modal_sizes Optional named list with modal settings for `add`, `edit`,
#'   `delete`, `versions` and `column_settings`. Each entry can be `"s"`,
#'   `"m"`, `"l"`, a CSS width such as `"90vw"`, or a list with `size`,
#'   `width`, `height` and `max_height`.
#' @param display_transform Optional function used to derive the records table
#'   shown to the user from the raw database records. The function may use
#'   `function(data)` or `function(data, context)` and must return a data frame.
#'   If raw records contain `sft_id`, the returned data must keep `sft_id` so
#'   row selections can be mapped back to the underlying record.
#' @param modal_header Optional UI or function rendered at the top of add/edit
#'   dialogs. Functions may declare any of `values`, `record`, `context`,
#'   `prefix`, `input`, `output`, `session`, `ns` and `form`. This is intended
#'   for display-only, cross-table context such as contact details or linked
#'   record summaries.
#' @param input_bindings Optional list of dynamic input bindings created with
#'   [dynamic_choices()], [dynamic_value()] or [dynamic_visibility()]. Bindings
#'   are registered for both add and edit dialogs.
#' @param refresh_triggers Optional reactive (or list of reactives) that this
#'   table should re-fetch on. Pass another form's returned `changed` reactive to
#'   make this table react to changes in that table, so dependent tables and
#'   downstream outputs (maps, summaries) stay in sync.
#' @param conflict_check Logical. When `TRUE` (default), saving an edit is
#'   rejected if another user changed the record while the edit dialog was
#'   open: instead of silently overwriting, the dialog switches to a conflict
#'   view showing what changed and who changed it, with the choice to apply
#'   the other changes to the inputs, keep the own entries, or go back. The
#'   check is value-based and runs inside the update transaction (see the
#'   `expected_record` argument of [update_record()]). Set to `FALSE` to
#'   restore the previous last-write-wins behaviour.
#' @param include_deleted_default Logical default for including deleted records.
#' @param form_layout Normally `NULL`: the server reads the layout
#'   (`"modal"` or `"inline"`) that [form_ui()] rendered. Pass a value only to
#'   override it, e.g. in tests without a UI.
#' @param ... Deprecated arguments, accepted with a warning for one release and
#'   mapped onto the lists above: every `can_*` flag, `hide_forbidden` and
#'   `editable_fields` (-> `permissions`); `table_options`, `table_class`,
#'   `table_filter`, `table_format`, `audit_options`, `version_options`,
#'   `deleted_records_options`, `datetime_format` (-> `table`);
#'   `table_columns`, `table_views`, `default_column_view`,
#'   `persist_column_settings`, `show_system_columns`, `display_column_labels`
#'   (-> `columns`); `highlight_fields`, `highlight_tab`, `highlight_color`,
#'   `show_changed`, `changed_color` (-> `highlight`); `show_audit` (no effect:
#'   the audit table renders whenever `form_ui(show_audit = TRUE)` placed it).
#'
#' @return A list of reactive helpers: `records`, `display_records` and
#'   `selected_record` reactives, a `changed` reactive that increments on every
#'   mutation (insert/update/delete/restore), a `refresh()` function, the
#'   `form`, the `conn` the module started with, and `connection()`, a function
#'   returning the module's current connection. Prefer `connection()` when the
#'   module owns its connection: it probes the handle and reopens it if the
#'   server dropped it, whereas `conn` is the handle as it was at start-up.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
#' )
#' ui <- fluidPage(form_ui("contacts", title = "Contacts"))
#' server <- function(input, output, session) {
#'   form_server(id = "contacts", form = contacts)
#' }
#' shinyApp(ui, server)
#' }
#' @export
form_server <- function(id,
                            form,
                            conn = NULL,
                            user = NULL,
                            permissions = list(),
                            table = list(),
                            columns = list(),
                            highlight = list(),
                            labels = list(),
                            modal_sizes = list(),
                            display_transform = NULL,
                            modal_header = NULL,
                            input_bindings = NULL,
                            refresh_triggers = NULL,
                            conflict_check = TRUE,
                            include_deleted_default = FALSE,
                            form_layout = NULL,
                            ...) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  settings <- sft_form_server_settings(
    permissions = permissions,
    table = table,
    columns = columns,
    highlight = highlight,
    form_layout = form_layout,
    dots = list(...)
  )
  form_layout <- settings$form_layout

  # The adapters (rights_permissions(), shinymanager_permissions()) carry the
  # user alongside the permissions; honour it when no user was passed.
  if (is.null(user)) {
    user <- settings$permissions$user
  }

  labels <- sft_ui_labels(labels)
  modal_sizes <- sft_modal_sizes(modal_sizes)
  sft_check_form_region(modal_header, "modal_header")
  sft_check_table_format(settings$table$format)

  shiny::moduleServer(id, function(input, output, session) {
    state <- sft_module_state(
      input = input,
      output = output,
      session = session,
      form = form,
      conn = conn,
      user = user,
      settings = settings,
      labels = labels,
      modal_sizes = modal_sizes,
      display_transform = display_transform,
      modal_header = modal_header,
      input_bindings = input_bindings,
      refresh_triggers = refresh_triggers,
      conflict_check = conflict_check,
      include_deleted_default = include_deleted_default,
      form_layout = form_layout
    )

    initial_user <- if (!is.null(user) && !is.function(user)) {
      user
    } else {
      NA_character_
    }

    sft_ensure_schema(
      conn = state$handle,
      form = form,
      user = initial_user
    )

    if (is.function(form$server)) {
      form$server(input, output, session)
    }

    # Registrars, each `function(input, output, session, state)`. Column
    # settings goes first: it exposes current_record_columns / column_choices,
    # which the tables and the deleted-records dialog read.
    sft_expose(state, sft_register_column_settings(input, output, session, state))
    sft_register_input_bindings(input, output, session, state)
    sft_register_visibility(input, output, session, state)
    sft_register_modal_header(input, output, session, state)
    sft_register_highlight(input, output, session, state)
    sft_register_edit_conflict(input, output, session, state)
    sft_register_inline_form(input, output, session, state)
    sft_register_crud(input, output, session, state)
    sft_expose(state, sft_register_deleted_versions(input, output, session, state))
    sft_register_records_table(input, output, session, state)

    list(
      records = state$records,
      display_records = state$display_records,
      selected_record = state$selected_record,
      changed = shiny::reactive(state$refresh_tick()),
      refresh = state$refresh,
      conn = state$handle,
      connection = state$conn,
      form = form
    )
  })
}
