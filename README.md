# shinyformtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/Gwstat/shinyformtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Gwstat/shinyformtools/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`shinyformtools` builds database-backed [Shiny](https://shiny.posit.co/) form
modules from a single declarative schema. A form is described once with `form()`
and `form_field()`; the package derives the database schema, CRUD operations,
rendering, and the Shiny module from that one description. It is aimed at
production-style workflows where records are created, edited, audited, restored,
and displayed through configurable table views.

It is not limited to classic data-entry forms: the same declarative description
drives **questionnaires and surveys, feedback boxes, and admin tables** — anything
backed by records. A wide range of input types is built in (text, password,
multi-line text, select / selectize, radio and checkbox groups, multi-select,
numeric, slider, date and date-range, time, IBAN, ...) and every field is
customizable.

## Background

shinyformtools started back in 2021, and I finally found the time to refactor it
into a proper, tested package — with the help of
[Claude Code](https://claude.com/claude-code). Early role models were Dean
Attali's [shinyforms](https://github.com/daattali/shinyforms) and Niels van der
Velden's write-up on
[editable DataTables in Shiny backed by SQL](https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/).

## Features

- **Declarative core** — describe a form once with `form()` / `form_field()`;
  the schema, CRUD, rendering, and Shiny module are derived from it.
- **Database-backed CRUD** with soft delete only (no hard deletes), a full audit
  log, and record restore from any past version.
- **Additive schema migrations** — the schema is reconciled to the form
  definition automatically; safe changes apply themselves, unsafe ones throw.
- **Database-level uniqueness** via composite unique indexes, so a value can be
  reused after a soft delete while live duplicates are rejected.
- **Configurable table views** with user-selectable, saveable column layouts.
- **Dynamic and cross-referencing inputs** through `dynamic_choices()`,
  `dynamic_value()`, and `reference_choices()`, plus display-only derived columns
  via `display_transform`.
- **Server-side validation** with `validation_rule()`, `required_if()`,
  `forbid_if()`, and `warning_if()` — rules that cannot be bypassed from the
  client.
- **Permissions** — fine-grained `can_*` controls on `form_server()`, a rights
  table built with `permissions_form()` / `rights_permissions()`, and a
  `shinymanager` adapter.
- **Shape fields** — attach a fixed, non-editable geometry to each record with
  `shape_field()` / `attach_shapes()`, stored backend-neutrally as text.
- **Three backends** — `SQLite`, `MariaDB`, and `DuckDB` behind one interface.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Gwstat/shinyformtools")
```

## Minimal example

```r
library(shiny)
library(shinyformtools)

contacts <- form(
  form_id = "contacts",
  table_name = "contacts",
  db = db_sqlite("contacts.sqlite"),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE),
    form_field(id = "phone", label = "Phone"),
    form_field(id = "birth_date", label = "Birth date", input_type = "dateInput")
  )
)

ui <- fluidPage(
  form_ui("contacts", title = "Contacts")
)

server <- function(input, output, session) {
  form_server(id = "contacts", form = contacts)
}

shinyApp(ui, server)
```

The table, audit log, and supporting system tables are created on first contact;
no manual migration step is required.

## Backends

Pick a backend with one of the explicit helpers and pass it to `form(db = ...)`.

```r
# Local single-user or demo app (default local backend)
db_sqlite("app.sqlite")

# Local analytical backend, useful for larger local data
db_duckdb("app.duckdb")

# Production multi-user backend
db_mariadb(
  dbname = "shinyformtools",
  host = "127.0.0.1",
  user = Sys.getenv("SFT_MARIADB_USER"),
  password = Sys.getenv("SFT_MARIADB_PASSWORD")
)
```

`SQLite` is the default local backend; `MariaDB` is recommended for multi-user
deployments; `DuckDB` is supported as an aligned local backend for CRUD, audit,
and preferences.

## Server-side validation

Cross-field rules are evaluated on the server during insert and update, so they
cannot be bypassed by client-side manipulation.

```r
checks <- form(
  form_id = "checks",
  table_name = "checks",
  db = db_sqlite("checks.sqlite"),
  fields = list(
    form_field(id = "status", label = "Status"),
    form_field(id = "reason", label = "Reason")
  ),
  validation_rules = list(
    required_if(
      id = "reason_required_when_rejected",
      condition = function(values) identical(values$status, "Rejected"),
      fields = "reason",
      message = "A reason is required when the status is Rejected."
    )
  )
)
```

`changelog_box()` can be used in `modal_header` hooks to show a compact audit
history for the edited record.

## Permissions

`form_server()` exposes a `can_*` argument for each action (add, edit, delete,
restore, view versions, view the audit log, and so on); with the default
`hide_forbidden = TRUE` the matching controls are hidden when a permission is
`FALSE`, and the server-side guards are enforced regardless. For multi-user
apps, `permissions_form()` stores permissions as an editable rights table and
`rights_permissions()` resolves them into the `can_*` functions, reactively.

## Security

All database access uses parameterized queries: values are bound with `?`
placeholders and `params`, and are never interpolated into SQL. Identifiers
(table, column, and index names) are quoted with `DBI::dbQuoteIdentifier()`, and
because every identifier originates from a form or field definition, they are
additionally validated at definition time against a strict allowlist
(`^[A-Za-z][A-Za-z0-9_]*$`, with the `sft_` prefix reserved). The two DDL spots
that cannot be parameterized (a `PRAGMA table_info()` table name and a column
`DEFAULT` clause) use `DBI::dbQuoteString()` on developer-defined metadata. As a
result, neither a user-entered value nor a form definition can inject SQL. This
is locked in by regression tests in
[`tests/testthat/test-sql-injection.R`](tests/testthat/test-sql-injection.R).

## Example apps

Thirteen self-contained demo apps ship with the package; each shows a
**"How it is built"** walkthrough beside the running form. List and run them:

```r
list_examples()
run_example("app_crud_basic")
```

- **app_crud_basic** — the smallest complete app: one `form()` drives the schema,
  the add/edit/delete dialogs, the records table, soft-delete with restore, and
  the audit log. **Start here.**
- **app_input_types** — a tour of every supported input type, plus the
  `html_field()` and `output_field()` field kinds and a server-fed live preview.
- **app_field_control** — constraining fields: per-user `editable`, locked/derived
  fields, hidden-but-stored columns (`show = FALSE`), and conditional inputs
  (`dynamic_visibility()`).
- **app_cascading_inputs** — chained inputs via `dynamic_choices()` /
  `dynamic_value()` (street → house number → suffix → derived ZIP), no
  hand-written observers. Needs `dplyr`.
- **app_calculated_columns** — render-time derived columns with
  `display_transform()`, both within a row and joined from another table (live via
  `refresh_triggers`).
- **app_inline_forms** — `form_layout = "inline"`: the add/edit form sits in a
  panel above the table instead of a modal dialog.
- **app_table_style** — style and transform the records table through
  `table_options` / `table_filter` / `table_format` (`DT::formatStyle()`).
- **app_markdown** — `form_field(markdown = TRUE)` renders stored text as
  (HTML-sanitized) Markdown in the table. Needs `commonmark`.
- **app_backends** — one form, two backends side by side: **SQLite** and
  **DuckDB** (the DuckDB tab appears when `duckdb` is installed).
- **app_mariadb** — the same form on a **MariaDB / MySQL** server, with an in-app
  setup tutorial (copy-paste Docker command) when no server is reachable. Needs
  `RMariaDB`.
- **app_shinymanager** — a support desk with per-user CRUD across two tables,
  driven by an editable rights table (`permissions_form()` /
  `rights_permissions()`) on top of a `shinymanager` login. Needs `shinymanager`.
- **app_german** — a fully German UI from one `use_german()` switch (English stays
  the default), overridable per form.
- **app_shape_map** — records with a fixed, non-editable geometry:
  `shape_field()` + `attach_shapes()` drawn on a leaflet map via `decode_shape()`.
  Needs `sf` and `leaflet`.

Each example is documented in more detail in
[`inst/examples/README.md`](inst/examples/README.md).

## Contributing

Bug reports, ideas, and pull requests are welcome — see
[CONTRIBUTING](.github/CONTRIBUTING.md) for the development workflow and
conventions.
