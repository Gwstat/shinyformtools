# Language packs and the global label/message override layer.
#
# Resolution order for every user-facing string is:
#   English default  <-  global option  <-  explicit per-call argument
# so a global switch (use_german()) changes the defaults while a per-form
# `labels` / `messages` argument still wins for situational renaming.

# ---------------------------------------------------------------------------
# Table-label layer (records-table headers, audit/version headers, the
# Yes/No deleted flag, audit action names and changelog snippets). These were
# historically hardcoded; they now read from getOption("shinyformtools.table_labels").
# ---------------------------------------------------------------------------

sft_default_table_labels <- function() {
  list(
    # System columns (keyed by the actual column name).
    sft_id = "Internal ID",
    sft_uuid = "UUID",
    sft_easy_id = "ID",
    sft_form_id = "Form",
    sft_form_version = "Version",
    sft_schema_hash = "Schema",
    sft_created_at = "Created at",
    sft_created_by = "Created by",
    sft_updated_at = "Last edit",
    sft_updated_by = "Edited by",
    sft_deleted_at = "Deleted at",
    sft_deleted_by = "Deleted by",
    sft_is_deleted = "Deleted",
    # Audit action names.
    action_insert = "Created",
    action_update = "Edited",
    action_delete = "Deleted",
    action_restore = "Restored",
    # Boolean flag rendering for the deleted column.
    flag_yes = "Yes",
    flag_no = "No",
    # Audit-log table headers.
    audit_log_id = "Log ID",
    audit_record_id = "Record ID",
    audit_record_uuid = "UUID",
    audit_action = "Action",
    audit_version_no = "Version",
    audit_changed_at = "Timestamp",
    audit_changed_by = "User",
    audit_changed_fields = "Changed fields",
    audit_reason = "Reason",
    # changelog_box() snippets.
    changelog_version = "Version",
    changelog_fields = "Fields"
  )
}

sft_table_labels <- function() {
  option_labels <- getOption("shinyformtools.table_labels", list())

  if (!is.list(option_labels)) {
    option_labels <- list()
  }

  utils::modifyList(
    sft_default_table_labels(),
    option_labels,
    keep.null = TRUE
  )
}

# ---------------------------------------------------------------------------
# German language pack.
# ---------------------------------------------------------------------------

#' German UI labels
#'
#' Returns the German translation of the user-facing UI labels (buttons, dialog
#' titles, notifications, column-settings and restore text). Use it as the
#' `labels` argument of [form_server()], spread into a custom list, or set it
#' globally with [use_german()]. The list can be edited freely for situational
#' wording (for example renaming `open_edit` to `"Editieren"`).
#'
#' @return A named list of German UI labels.
#' @examples
#' labels <- german_labels()
#' labels$save
#' labels$open_add
#' str(labels[c("save", "cancel", "delete")])
#' @export
german_labels <- function() {
  list(
    user_label = "Benutzer",
    open_add = "Eintrag hinzuf\u00fcgen",
    add_not_allowed = "Hinzuf\u00fcgen ist f\u00fcr diesen Benutzer nicht erlaubt.",
    open_edit = "Bearbeiten",
    view_not_allowed = "Ansehen ist f\u00fcr diesen Benutzer nicht erlaubt.",
    edit_not_allowed = "Bearbeiten ist f\u00fcr diesen Benutzer nicht erlaubt.",
    delete = "Auswahl l\u00f6schen",
    refresh_table = "Tabelle zur\u00fccksetzen",
    open_deleted_records = "Gel\u00f6schte Datens\u00e4tze",
    open_column_settings = "Spalteneinstellungen",
    open_column_selection = "Spalten",
    include_deleted = "Gel\u00f6schte Datens\u00e4tze anzeigen",
    audit_title = "\u00c4nderungsprotokoll",
    add_title = "Eintrag hinzuf\u00fcgen",
    edit_title = "Datensatz bearbeiten: {id}",
    delete_title = "L\u00f6schen best\u00e4tigen",
    delete_question = "Datensatz {id} wirklich l\u00f6schen?",
    versions_title = "Versionen anzeigen: {id}",
    versions_intro = "Hier werden die gespeicherten Versionen des ausgew\u00e4hlten Datensatzes angezeigt.",
    edit_versions_title = "Versionen",
    edit_versions_intro = "Gespeicherte Versionen des Datensatzes. Eine ausgew\u00e4hlte Version kann wiederhergestellt werden, sofern dies erlaubt ist.",
    deleted_records_title = "Gel\u00f6schte Datens\u00e4tze",
    deleted_records_intro = "Hier werden gel\u00f6schte Datens\u00e4tze angezeigt. Zum Wiederherstellen einen Datensatz ausw\u00e4hlen und seine Versionen \u00f6ffnen.",
    deleted_records_empty = "Keine gel\u00f6schten Datens\u00e4tze.",
    open_deleted_versions = "Versionen des gel\u00f6schten Datensatzes anzeigen",
    column_settings_title = "Spalteneinstellungen",
    column_selection_title = "Spalten",
    column_settings_label = "Anzuzeigende Spalten",
    column_settings_view = "Ansicht",
    column_selection_view = "Gespeicherte Ansicht",
    column_settings_view_name = "Ansicht speichern als",
    column_settings_view_name_placeholder = "Neue Ansicht",
    column_settings_load_view = "Ansicht laden",
    column_selection_help = "Spalten frei ausw\u00e4hlen und per Drag-and-drop umordnen. Beim Laden einer gespeicherten Ansicht wird deren Reihenfolge exakt \u00fcbernommen.",
    column_selection_free_help = "Freie Auswahl: links Spalten hinzuf\u00fcgen, rechts entfernen oder per Drag-and-drop umordnen. Gespeicherte Ansichten werden \u00fcber \u201eAnsicht laden\u201c angewendet.",
    column_settings_help = "Spalten k\u00f6nnen per Drag-and-drop hinzugef\u00fcgt, entfernt und umgeordnet werden. Nicht mehr vorhandene Spalten werden automatisch ignoriert.",
    column_settings_available = "Nicht angezeigt",
    column_settings_selected = "Angezeigt / Reihenfolge",
    column_settings_empty = "Keine Spalten",
    column_settings_add_column = "In Tabelle anzeigen",
    column_settings_remove_column = "Aus Tabelle entfernen",
    deleted_restore_hint = "Dieser Datensatz ist derzeit gel\u00f6scht. Beim Wiederherstellen wird er reaktiviert.",
    cancel = "Abbrechen",
    close = "Schlie\u00dfen",
    save = "Speichern",
    update = "\u00c4nderungen speichern",
    confirm_delete = "L\u00f6schen",
    confirm_restore = "Ausgew\u00e4hlte Version wiederherstellen",
    restore_deleted = "Neueste Version wiederherstellen",
    record_restored = "Datensatz wiederhergestellt.",
    save_column_view = "Speichern",
    no_selection = "Bitte genau einen Datensatz ausw\u00e4hlen.",
    no_valid_selection = "Keine g\u00fcltige Auswahl mehr vorhanden.",
    no_valid_record_selection = "Keine g\u00fcltige Datensatzauswahl.",
    delete_not_allowed = "L\u00f6schen ist f\u00fcr diesen Benutzer nicht erlaubt.",
    restore_not_allowed = "Wiederherstellen ist f\u00fcr diesen Benutzer nicht erlaubt.",
    versions_not_allowed = "Das Anzeigen von Versionen ist f\u00fcr diesen Benutzer nicht erlaubt.",
    deleted_records_not_allowed = "Das Anzeigen gel\u00f6schter Datens\u00e4tze ist f\u00fcr diesen Benutzer nicht erlaubt.",
    column_settings_not_allowed = "Spalteneinstellungen sind f\u00fcr diesen Benutzer nicht erlaubt.",
    column_selection_not_allowed = "Die Spaltenauswahl ist f\u00fcr diesen Benutzer nicht erlaubt.",
    reset_not_allowed = "Das Zur\u00fccksetzen der Tabelle ist f\u00fcr diesen Benutzer nicht erlaubt.",
    table_not_allowed = "Das Anzeigen der Datensatztabelle ist f\u00fcr diesen Benutzer nicht erlaubt.",
    deleted_cannot_edit = "Gel\u00f6schte Datens\u00e4tze k\u00f6nnen nicht bearbeitet werden. Bitte \u00fcber \u201eVersionen anzeigen\u201c wiederherstellen.",
    already_deleted = "Datensatz ist bereits gel\u00f6scht.",
    record_added = "Datensatz hinzugef\u00fcgt.",
    record_updated = "Datensatz aktualisiert.",
    record_deleted = "Datensatz gel\u00f6scht.",
    record_meta = "Letzte Bearbeitung: {time} \u00b7 Benutzer: {user}",
    no_versions = "Keine wiederherstellbaren Versionen verf\u00fcgbar.",
    choose_version = "Bitte genau eine Version ausw\u00e4hlen.",
    version_unavailable = "Die ausgew\u00e4hlte Version ist nicht mehr verf\u00fcgbar.",
    version_restored = "Version {version} wurde wiederhergestellt.",
    columns_applied = "Spaltenauswahl angewendet.",
    columns_saved = "Spaltenansicht gespeichert.",
    columns_loaded = "Spaltenansicht geladen.",
    columns_reset = "Spaltenauswahl auf Standard zur\u00fcckgesetzt.",
    standard_column_view_not_overwritable = "Die Standardansicht kann nicht \u00fcberschrieben werden. Bitte einen neuen Ansichtsnamen w\u00e4hlen.",
    table_refreshed = "Tabellenfilter und -zustand wurden zur\u00fcckgesetzt."
  )
}

#' German validation messages
#'
#' Returns the German translation of the validation messages. Use it as the
#' `messages` argument of [form()] / [form_server()], or set it globally with
#' [use_german()].
#'
#' @return A named list of German validation messages.
#' @examples
#' msgs <- german_messages()
#' msgs$mandatory_missing
#' str(msgs)
#' @export
german_messages <- function() {
  list(
    mandatory_missing = "Pflichtfelder fehlen: {fields}.",
    mandatory_empty = "Pflichtfelder sind leer: {fields}.",
    unique = "Der Wert f\u00fcr '{label}' ist bereits vergeben.",
    conditional_required = "Bedingte Pflichtfelder fehlen: {fields}.",
    validation_rule_failed = "Validierungsregel '{rule}' fehlgeschlagen.",
    no_active_fields_for_update = "Es wurden keine aktiven Formularfelder zum Aktualisieren \u00fcbergeben."
  )
}

#' German table, audit and changelog labels
#'
#' Returns the German translation of the records-table system-column headers,
#' the Yes/No deleted flag, the audit action names, the audit/version table
#' headers and the changelog snippets. Set it globally with [use_german()] or
#' via `options(shinyformtools.table_labels = german_table_labels())`.
#'
#' @return A named list of German table labels.
#' @examples
#' labels <- german_table_labels()
#' labels$sft_created_at
#' labels$action_insert
#' str(labels[c("sft_created_at", "flag_yes", "flag_no")])
#' @export
german_table_labels <- function() {
  list(
    sft_id = "Interne ID",
    sft_uuid = "UUID",
    sft_easy_id = "ID",
    sft_form_id = "Formular",
    sft_form_version = "Version",
    sft_schema_hash = "Schema",
    sft_created_at = "Erstellt am",
    sft_created_by = "Erstellt von",
    sft_updated_at = "Letzte Bearbeitung",
    sft_updated_by = "Bearbeitet von",
    sft_deleted_at = "Gel\u00f6scht am",
    sft_deleted_by = "Gel\u00f6scht von",
    sft_is_deleted = "Gel\u00f6scht",
    action_insert = "Erstellt",
    action_update = "Bearbeitet",
    action_delete = "Gel\u00f6scht",
    action_restore = "Wiederhergestellt",
    flag_yes = "Ja",
    flag_no = "Nein",
    audit_log_id = "Protokoll-ID",
    audit_record_id = "Datensatz-ID",
    audit_record_uuid = "UUID",
    audit_action = "Aktion",
    audit_version_no = "Version",
    audit_changed_at = "Zeitstempel",
    audit_changed_by = "Benutzer",
    audit_changed_fields = "Ge\u00e4nderte Felder",
    audit_reason = "Grund",
    changelog_version = "Version",
    changelog_fields = "Felder"
  )
}

#' German DataTables chrome (search, pagination, info)
#'
#' Returns the German translation of the DataTables interface strings - the
#' search box, the "Show N entries" length menu, the "Showing 1 to N" info line
#' and the Previous/Next pagination. These come from DataTables itself (not the R
#' label layer), so they are supplied as the `language` DT option. [use_german()]
#' installs it globally; it can also be passed per table via
#' `table_options = list(language = german_dt_language())`.
#'
#' @return A named list suitable for the DataTables `language` option.
#' @examples
#' lang <- german_dt_language()
#' lang$search
#' lang$paginate$previous
#' # Use per table:
#' # records_datatable(..., table_options = list(language = german_dt_language()))
#' @keywords internal
german_dt_language <- function() {
  list(
    search = "Suchen:",
    searchPlaceholder = "",
    lengthMenu = "_MENU_ Eintr\u00e4ge anzeigen",
    info = "_START_ bis _END_ von _TOTAL_ Eintr\u00e4gen",
    infoEmpty = "0 bis 0 von 0 Eintr\u00e4gen",
    infoFiltered = "(gefiltert von _MAX_ Eintr\u00e4gen)",
    zeroRecords = "Keine passenden Eintr\u00e4ge gefunden",
    emptyTable = "Keine Daten in der Tabelle vorhanden",
    loadingRecords = "Wird geladen ...",
    processing = "Bitte warten ...",
    paginate = list(
      first = "Erste",
      previous = "Zur\u00fcck",
      `next` = "N\u00e4chste",
      last = "Letzte"
    ),
    aria = list(
      sortAscending = ": aktivieren, um Spalte aufsteigend zu sortieren",
      sortDescending = ": aktivieren, um Spalte absteigend zu sortieren"
    )
  )
}

#' Switch all default user-facing text to German
#'
#' Sets the global options consulted by every default string, so a whole app
#' renders in German without passing `labels` / `messages` to each form. English
#' remains the package default; this opt-in switch overrides it. Explicit
#' per-form `labels` / `messages` arguments still take precedence, so individual
#' strings can be renamed situationally.
#'
#' @return Invisibly, the previous values of the affected options.
#' @seealso [use_english()] to clear the switch, and [german_labels()],
#'   [german_messages()], [german_table_labels()] for the underlying lists.
#' @examples
#' # Switch the whole app to German defaults, then restore the previous state.
#' old <- use_german()
#' getOption("shinyformtools.labels")$save
#' options(old)
#' @export
use_german <- function() {
  previous <- options(
    shinyformtools.labels = german_labels(),
    shinyformtools.messages = german_messages(),
    shinyformtools.table_labels = german_table_labels(),
    shinyformtools.dt_language = german_dt_language()
  )

  invisible(previous)
}

#' Reset user-facing text to the English defaults
#'
#' Clears the global language options set by [use_german()] so the package falls
#' back to its English defaults.
#'
#' @return Invisibly, the previous values of the cleared options.
#' @examples
#' # Clear any German language switch and fall back to the English defaults.
#' old <- use_german()
#' use_english()
#' is.null(getOption("shinyformtools.labels"))
#' options(old)
#' @export
use_english <- function() {
  previous <- options(
    shinyformtools.labels = NULL,
    shinyformtools.messages = NULL,
    shinyformtools.table_labels = NULL,
    shinyformtools.dt_language = NULL
  )

  invisible(previous)
}
