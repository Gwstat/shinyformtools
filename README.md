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

List and run the bundled examples:

```r
list_examples()
run_example("app_shape_map")          # shape fields + a leaflet map
run_example("app_cascading_inputs")   # cascading inputs + cross-table references
run_example("app_shinymanager")       # shinymanager login + a rights table
```

Each example is documented in
[`inst/examples/README.md`](inst/examples/README.md).

## Development

```r
devtools::document()
devtools::load_all()
devtools::test()
devtools::check()
```

- DuckDB tests run only when the `duckdb` package is installed.
- MariaDB tests run only when the `SFT_MARIADB_USER` / `SFT_MARIADB_PASSWORD`
  environment variables are set.
