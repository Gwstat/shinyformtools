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
