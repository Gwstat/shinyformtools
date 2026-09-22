# shinyformtools 0.3.1

* New, off by default: `form_server(table = list(checkbox_labels = TRUE))`
  shows checkbox fields in the records table as the language's yes / no
  labels ("Yes" / "No", "Ja" / "Nein") instead of 0 / 1. The export keeps
  writing `TRUE` / `FALSE`.

* Housekeeping, no change in behaviour: the weekly integration workflow now
  runs the live MariaDB tests (it started a server but set the wrong variable
  names); every tracked file is stored with LF line endings; five internal
  helpers that nothing called were removed.
* Fixed: numeric fields reached the records table as text, so DT sorted them
  as text ("100" before "35") and drew a text box instead of a range slider
  for the column filter. They stay numeric now.
* Fixed: a records table with a column filter (`table = list(filter = )`)
  loads DT's older selectize, which then broke every select input in the add
  and edit dialog under Shiny 1.13 (the dialog stayed unbound and could not
  save). `form_ui()` and `form_buttons()` now put Shiny's own selectize on the
  page first.
* Fixed: a dynamic choice list that becomes empty (`dynamic_choices()` with a
  server-side selectize) raised a script error in the browser.
* Slide headings: `form(slide_labels = )` and `form_field(slide_label = )` were
  stored but never shown. A multi-slide form now renders the label as a heading
  above its slide. Slides without a label get no heading, so existing slide
  forms render as before. New example `app_presentation_german`.

# shinyformtools 0.3.0

* New: export to CSV and Excel. `export_records(form, "people.xlsx")` writes a
  form's records from a script; `form_ui(show_export = TRUE)` adds download
  buttons to the module (off by default). The file has field labels as
  headers, multi-value fields as readable text instead of JSON arrays, numbers
  as numbers, checkboxes as `TRUE` / `FALSE` and timestamps in local time. CSV
  files carry a UTF-8 byte order mark so that Excel reads umlauts correctly;
  `"csv2"` is the semicolon / decimal-comma dialect for European Excel. The
  module's download holds what the table holds: the visible columns and, when
  the user has searched the table, the matching rows. New permission
  `can_export` (default `TRUE`; it also requires `can_view_table`).

# shinyformtools 0.2.2

* Fixed: the audit log listed EVERY submitted field of an update as changed.
  The edit form submits all fields on every save, so each update claimed to
  have changed all of them, the changelog showed them all, and the conflict
  view ("last changed by") credited the last saver with columns somebody else
  had changed. Updates and restores now log only the fields whose stored value
  really differs. Entries written before this fix keep their long lists.
* `changed_fields_json` is always a JSON array. A single changed field used to
  be written as a bare JSON string. Both shapes are still read.
* New help pages: `?connections` (who opens and closes a connection, with and
  without `conn`, in scripts and in the module) and `?hook_context` (what the
  `context` argument of `display_transform`, `modal_header`, `table$format` and
  input-binding handlers holds).

# shinyformtools 0.2.1

* Fixed: a checkbox field written from a script with `1`, `1L`, `"1"` or
  `"TRUE"` was stored as 0 ("no"). Only `TRUE` counted. Storing and reading now
  share one notion of "true". Forms filled in through the app were never
  affected, because a checkbox input delivers `TRUE` / `FALSE`.
* `fetch_records(include_deleted = "only")` returns just the soft-deleted
  records, filtered by the database. The deleted-records dialog uses it instead
  of fetching the whole table.
* Internal: three long database functions were split into named steps. No
  change in behaviour, no schema change, nothing re-migrates.

# shinyformtools 0.2.0

## Argument clean-up

* `form_server()` now takes four option bundles instead of ~30 flat arguments:
  `permissions = list(can_add = , ..., hide_forbidden = , editable_fields = )`,
  `table = list(options = , class = , filter = , format = , audit_options = ,
  version_options = , deleted_records_options = , datetime_format = )`,
  `columns = list(visible = , views = , default_view = , persist = ,
  show_system = , labels = )` and `highlight = list(fields = , tab = , color = ,
  show_changed = , changed_color = )`. The adapters `shinymanager_permissions()`
  and `rights_permissions()` return a `permissions` list directly, so
  `form_server(id, form, permissions = rights_permissions(...))` replaces the
  `do.call()` idiom. Every old flat argument still works and warns once per
  session; a misspelt bundle key is an error.
* `form_server(show_audit = )` is gone: the audit output is always registered
  and Shiny computes it only when `form_ui(show_audit = TRUE)` placed it.
* `form_server(form_layout = )` is optional: the server reads the layout that
  `form_ui()` rendered from a hidden input, so the two no longer have to match
  by hand.
* `inspect_schema()`, `plan_migration()` and `apply_migration()` take
  `(form, conn)` like every other function. The old `(conn, form)` order is
  recognised and warns.
* `validate_record(current_id = )` is `record_id = `; `shinymanager_users(db = )`
  is `credentials = `; `IBANInput()` / `updateIBANInput()` are `ibanInput()` /
  `updateIbanInput()`. Old names still work and warn.
* `form_ui(show_versions = )` and `form_ui(show_include_deleted = )` are
  deprecated (use `show_deleted_records`).
* `form_server()` returns `connection()`, a function yielding the module's
  current connection, next to the start-up `conn`.

## Languages

* New `language()` object: UI labels, validation messages, table labels and the
  'DataTables' chrome of a form in one argument, `form_ui(language = )` /
  `form_server(language = )`. It is scoped to that form, so one app can serve
  forms or users in different languages, which the process-wide `use_german()`
  cannot. `german()` and `english()` are ready-made; `language_keys()` lists
  every key with its English default. Example: `app_language`.
* `form_server(language = )` also accepts a function or reactive returning a
  language, so a user can switch language while the app runs.
* Text that was hard-coded English now follows the language: the fallback names
  of unnamed tabs and wizard slides, the changelog box (title, empty text,
  "+N more") and two uniqueness-rule messages.
* Three label keys that nothing read any more are gone from the defaults:
  `apply_columns`, `apply_column_selection`, `reset_columns`.

## Validation

* A rejected save now marks its fields: the add/edit form glows the fields the
  failed checks name (missing mandatory fields, taken unique values, the
  `fields` of a failed rule) until the next successful save.
  `form_server(highlight = list(invalid = FALSE))` turns it off.
* New `validation_issues()`: the non-throwing counterpart of
  `validate_record()`, returning one row per issue with `severity`, `source`,
  `message` and `fields`.
* The error `validate_record()` (and therefore `insert_record()` /
  `update_record()`) raises is of class `sft_validation_error` and carries
  `issues` and `fields`. Its message is unchanged.

## Input types

* `register_input()` gained `db_type`: the default column type of fields using
  that input, so a numeric widget no longer needs `db_type = "REAL"` on every
  `form_field()`. A `db_type` on the field still wins.
* A two-handle `sliderTextInput` is restored to its range in the edit dialog
  and shown as "from - to" in the tables (it came back as a raw JSON string),
  and `sliderTextInput` now supports `dynamic_choices()`.
* `dynamic_value()` works on choice inputs (`selectInput`, `selectizeInput`,
  `radioButtons`, `checkboxGroupInput`, `multiInput`, `sliderTextInput`); it
  used to stop with "not supported yet". Returning an empty value clears the
  selection.

## Performance

* New option `shinyformtools.schema_probe_ttl` (seconds, default 0 = off):
  remember a passed schema check per database and form definition. Meant for a
  remote database, where the check costs about 12 round trips per call; with
  `30`, a read drops from 15 round trips to 3 and an update from 19 to 7. A
  schema change made by another process is then noticed up to that many seconds
  late.

## Bug fixes

* A checkbox field stored `0` for every value that was not literally `TRUE`:
  a script inserting `1`, `"1"` or `"TRUE"` - an import from a table, say -
  silently saved "no". Storing and reading back now share one notion of true
  (`TRUE`, a non-zero number, `"1"`, `"true"`, `"yes"`). The checkbox in the app
  always delivered `TRUE` / `FALSE` and was never affected.
* `fetch_records(include_deleted = "only")` returns just the soft-deleted
  records, filtered by the database; the deleted-records dialog uses it instead
  of fetching the whole table.

* A write that loses its race five times in a row now fails with a readable
  message ("someone else was saving the same data at the same moment ... please
  save again") and the class `sft_write_conflict`, instead of the database's
  raw text. The original message is kept in parentheses.
* The schema check that precedes every database call no longer lists the
  tables twice; on MariaDB that is three round trips less per operation.

* A write that loses a race now waits a short, random moment before it
  retries. Retries used to fire at once, so writers that had collided collided
  again; measured with 8 processes updating one record, failed updates fell
  from 45% to 22%. No write was ever lost either way - a failed update raises
  an error.

* A database that refuses connections (MariaDB at `max_connections`) no longer
  takes user sessions down. Measured with a live server: a save in a session
  whose connection had been dropped lost the input and left the save button
  dead, a refresh killed the session through the table-sync observer, and a new
  session died at start-up with the driver's message. Now the save reports the
  error and keeps the dialog, the next save works once capacity is back, and a
  session that cannot connect stays up, says so, and connects on the next
  action. The README gained a "Connections" section on sharing one connection
  between forms.

* After a server-dropped connection was healed, the audit table, the
  deleted-records dialog, restore, saved column views and the conflict
  attribution kept using the dead handle. Every part of the module now reads
  the connection through one accessor.
* Validation warnings (`warning_if()`, `on_edit_missing_required = "warn"`)
  are shown to the user as warning notifications; the save still completes.
  They used to reach only the R console.
* A stored time value nothing could parse was replaced by the current time in
  the edit conflict view; the input now keeps its state.
* `form(schema_policy = "manual")` had no effect. It now stops CRUD calls on a
  missing or outdated schema with a hint to run `init_db()`.
* On update and restore, validation ran on the stored representation of a
  record: a unique multi-value field (checkbox group, multi-select) never hit
  the friendly "already taken" message but the raw index error, and a
  validation rule saw a JSON string where it sees a vector on insert. Stored
  rows are now decoded back to input values before validation.

# shinyformtools 0.1.0

First public release.

* Declarative core: describe a form once with `form()` / `form_field()`; the
  database schema, CRUD, rendering, and the Shiny module are derived from that
  description.
* Database-backed CRUD with soft delete only, a full audit log, and record
  restore from any past version.
* Additive schema migrations reconciled to the form definition, with
  database-level uniqueness via composite unique indexes.
* Configurable, user-saveable table views; dynamic and cross-referencing inputs
  (`dynamic_choices()`, `dynamic_value()`, `reference_choices()`) and display-only
  derived columns via `display_transform`.
* Server-side validation (`validation_rule()`, `required_if()`, `forbid_if()`,
  `warning_if()`) that cannot be bypassed from the client.
* Permissions: per-action `can_*` controls on `form_server()`, a rights-table
  model (`permissions_form()` / `rights_permissions()`), and a `shinymanager`
  adapter.
* Shape fields: attach a fixed, non-editable geometry to each record with
  `shape_field()` / `attach_shapes()`, stored backend-neutrally as text.
* Three backends behind one interface: 'SQLite', 'MariaDB', and 'DuckDB'.
* Bundled, runnable example apps (`list_examples()` / `run_example()`).
