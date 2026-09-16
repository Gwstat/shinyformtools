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

## Bug fixes

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
