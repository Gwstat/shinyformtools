# shinyformtools example apps

Each `app_*.R` file is a self-contained Shiny app. List and run them with:

```r
shinyformtools::list_examples()
shinyformtools::run_example("app_shape_map")
```

Each app lays out a **"How it is built"** walkthrough beside the running form:
the meaningful parts of the source are shown as numbered, annotated steps. Every
example is fully self-contained (each reads its own `#> STEP` markers and draws
the demo page itself) so a single file can be copied and run on its own.

## Examples

Start with **app_crud_basic**; the rest each focus on one capability.

- **app_crud_basic** — the smallest complete app: one `form()` drives the
  schema, the add/edit/delete dialogs, the records table, soft-delete with
  restore, and the audit log. Shows the common input types (text, select,
  numeric, date, checkbox, multi-line) and two server-side validations: a unique
  email (`form_field(unique = TRUE)`) and a conditional required field
  (`required_if()` makes Notes mandatory only when Gender is "Other"). The place
  to start.

- **app_backends** — one `form()` definition driving two database backends side
  by side: **SQLite** and **DuckDB**. The two tabs are identical (same fields,
  validation, CRUD, soft-delete and audit log); the only difference in the code
  is `db_sqlite()` versus `db_duckdb()`. The DuckDB tab appears only when the
  optional `duckdb` package is installed.

- **app_mariadb** — the same declarative form backed by a **MariaDB / MySQL**
  server. Switching backend is again one line (`db_mariadb(...)`), but a server
  is needed. The connection defaults to user `sft` / password `sft` on
  `127.0.0.1:3306` (database `shinyformtools`) so it works out of the box against
  the server below; override any of it with `SFT_MARIADB_USER` /
  `SFT_MARIADB_PASSWORD` / `SFT_MARIADB_DB` / `SFT_MARIADB_HOST` /
  `SFT_MARIADB_PORT`. When it cannot connect the app shows an in-app setup
  tutorial (with a copy-paste Docker command) instead of crashing, and the source
  is presented as a step-by-step walkthrough. Quickest server:
  `docker run --name shinyformtools-mariadb -p 3306:3306 -e
  MARIADB_DATABASE=shinyformtools -e MARIADB_USER=sft -e MARIADB_PASSWORD=sft -e
  MARIADB_ROOT_PASSWORD=root -d mariadb:11` (the same settings are in
  `inst/docker/docker-compose.mariadb.yml`). Requires the optional `RMariaDB`
  package.

- **app_input_types** — a tour of every supported input type (text, password,
  text area, numeric, slider, date, date range, time, IBAN, select, selectize
  with free entry, radio buttons, checkbox, checkbox group, multi-input)
  alongside the two non-input field kinds: `html_field()` (static markup in the
  form) and `output_field()` (a reactive output). The `output_field` "live
  preview" is filled from the server through `modal_header()` — the hook that
  exposes the form module's `output` — and updates as you type. Multi-value
  choices are stored natively as JSON arrays.

- **app_field_control** — constraining individual fields. **Blocking inputs**:
  `editable = function(user)` makes a field editable only for some users (greyed
  out and dropped on save for the rest, enforced server-side), and
  `editable = FALSE` locks a field entirely (here filled from another field by a
  `dynamic_value()` binding). **Invisible inputs**: `show = FALSE` keeps a field
  out of the form while it stays a stored column shown in the table.
  **Conditional inputs**: `dynamic_visibility()` shows a field only when another
  input has a given value (here "Contract end" appears only for contractors); the
  predicate runs again on save, so a hidden field's value is cleared rather than
  stored. An "Act as" switch demonstrates the per-user behaviour live.

- **app_calculated_columns** — table columns added at render time with
  `display_transform()`, in two flavours. **Within a row**: a People table
  derives a full name from first/last name and an age from a birthday.
  **From another table**: a Locations table stores only a reference to a person
  (`reference_choices()`) and shows that person's name and city by joining the
  People records at render time, updating live when a person changes
  (`refresh_triggers`). The derived columns exist in neither schema and recompute
  whenever the data changes.

- **app_markdown** — `form_field(markdown = TRUE)` renders a field's stored text
  as Markdown in the records table. Safe for user input: values are HTML-escaped
  and URL-sanitized, so a pasted `<script>` or `javascript:` link cannot execute.
  Requires the optional `commonmark` package.

- **app_inline_forms** — `form_layout = "inline"` on `form_ui()` and
  `form_server()` renders the add/edit form in a panel above the table instead of
  a modal dialog, with add and edit mutually exclusive.

- **app_questionnaire** — a survey / questionnaire. Each question carries a
  `slide` index (`form_field(slide = N)`); the slides are shown as a
  **shinyglide** Back/Next wizard, and fields sharing a slide (laid out by `col` /
  `pos`) put two inputs on one slide. There is no records table and no Add
  button: the fields are rendered directly with `render_form_fields()` (one slide
  per shinyglide screen), so the form is just on the page. The **Submit** button
  lives in the last screen, so it only appears on the final slide;
  `collect_input_values()` reads the answers, `insert_record()` stores the
  response, and the form then **closes** to a thank-you ("Submit another
  response" reloads a fresh survey). Requires the optional `shinyglide` package.

- **app_bug_report** — a "Report a bug" button living in an application's
  **header** rather than a records toolbar. `form_buttons("bugs", ...)` renders
  the form module's Add button (relabelled) using the same module id, so it opens
  the very same dialog that `form_server()` drives; the module's own button row
  is hidden with `form_ui(button_options = list(placement = "none"))`. Submitted
  reports appear in the table below.

- **app_cascading_inputs** — chained inputs driven by binding declarations.
  Street drives the house-number choices, the house number drives the suffix
  choices, and the ZIP is derived from the chosen address
  (`dynamic_choices()` / `dynamic_value()`) — no hand-written observers. The
  modal header echoes the address as it is filled in and shows a short change
  history (`changelog_box`). Requires the optional `dplyr` package. (For columns
  joined from another table, see **app_calculated_columns**.)

- **app_shinymanager** — a small support desk (Tickets + Agents) with per-user
  CRUD permissions across both tables, driven by an **editable rights table**
  rather than per-table credential columns.
  **shinymanager** only logs people in; all rights live in one
  `permissions_form()` table where each row grants a set of permissions to a
  user across one or more forms (a multi-select — adding a table never adds a
  column). `rights_permissions()` resolves those rules into the `can_*`
  functions each `form_server()` consumes, reactively, so buttons hide/show live
  (`hide_forbidden = TRUE`). Admins are superusers and manage the rights table in
  an admin-only tab, which also carries a short guide to what each `can_*` column
  grants — tick one in a rule, log in as that user, and watch the matching
  control follow. Requires the optional `shinymanager` package.

- **app_german** — a fully German UI from one global switch. `use_german()`
  flips every default user-facing string to German (buttons, dialogs,
  notifications, records-table headers, the Yes/No deleted flag, and the audit
  log) — English stays the package default for CRAN. Resolution order is English
  default → `use_german()` → per-form argument, so a second table renames just
  `open_edit` to "Editieren" while everything else stays German. The config is
  editable lists: `german_labels()`, `german_messages()`, `german_table_labels()`;
  `use_english()` clears the switch.

- **app_table_style** — styling and transforming the records DataTable through
  `form_server()` arguments. `table_options = list(paging = FALSE, dom = "t")`
  drops the pager and global chrome; `table_filter = "top"` adds per-column
  search controls that adapt to each column's type (range slider for a numeric
  column, dropdown for a factor, text box otherwise); `table_format` runs
  `DT::formatStyle()` to colour cells by value band and category.

- **app_shape_map** — districts with a fixed, non-editable geometry. Shows
  `shape_field()` + `attach_shapes()`: each record has editable
  attributes plus a boundary that the form never touches, drawn on a leaflet map
  via `decode_shape()`. Geometry is the North Carolina counties shapefile
  bundled with the **sf** package, so no external data is needed. Requires the
  optional `sf` and `leaflet` packages.
