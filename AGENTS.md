# shinyformtools (sft)

Declarative, database-backed Shiny forms toolkit. A form is described once with
`form()` / `form_field()`; the package derives the database schema, CRUD,
rendering, and Shiny modules from that description. Stack: R6, DBI, Shiny.

@.ai/PROJECT_STATE.md
@.ai/TODO.md

## Build, test, document
- Reload during development: `devtools::load_all()`.
- Run tests: `devtools::test()` (testthat). Run the full suite before committing.
- After adding/removing `@export` or roxygen, regenerate docs: `devtools::document()`.
- Full check before release: `devtools::check()`.
- Internal helpers do NOT need `@export`; they are visible to other package code
  and to tests automatically. Only export functions meant for package users.

## Code conventions
- Comments and roxygen are in English. snake_case for all names.
- Exported (public) functions use bare, descriptive names without a prefix
  (`form()`, `form_field()`, `form_server()`, `db_connect()`, `insert_record()`,
  ...). `form_field()` (not `field()`) avoids a collision with `vctrs::field`.
  Internal (non-exported) helpers and DB system columns keep the `sft_` prefix
  (`sft_quote_identifier()`, `sft_db_with_transaction()`, `sft_id`,
  `sft_easy_id`, ...). The S3 classes are still `sft_form` / `sft_field` /
  `sft_migration_plan`, so leave quoted class strings and `S3method` lines alone.
- All SQL uses parameterized queries (`params = list(...)`, `?` placeholders).
  Never interpolate values into SQL.
- Quote identifiers with `sft_quote_identifier()` (wraps `DBI::dbQuoteIdentifier`).
  Never hand-build quoted identifiers.
- Mutations run inside `sft_db_with_transaction()` and write an audit-log entry.
- Bundled data uses the two-object pattern: full RDS + slim `.rda` via
  `usethis::use_data()`.

## Backends
- SQLite, DuckDB, MariaDB. Dispatch on
  `sft_db_backend(conn)` -> "sqlite" | "duckdb" | "mariadb".
- Backend-specific SQL lives in small helpers (auto-id, short-text type,
  last-insert-id, index listing/dropping). New cross-backend SQL branches inside
  one helper, never inline at the call site.
- DuckDB needs explicit record ids (`sft_next_sft_id()`); MariaDB credentials
  come from env `SFT_MARIADB_USER` / `SFT_MARIADB_PASSWORD`.

## Schema / migration model (do not regress)
- Reconciliation is gated by a cheap probe: `sft_schema_is_current()` runs on
  every CRUD call; the full migration
  (`init_db` -> `plan_migration` -> `apply_migration`) runs only on
  first contact or genuine drift. Do NOT reintroduce unconditional schema
  reconciliation in the CRUD hot path.
- Drift is detected by `sft_schema_signature()`, a backend-neutral signature over
  expected columns AND expected indexes, stored in `sft_forms.schema_hash` and on
  each record.
- Migrations are additive: safe actions (create_table, add_column, retire_column,
  create_index, drop_index) auto-apply; unsafe actions (type changes, adding a
  system primary key) throw and require manual handling.
- Indexes are first-class migration objects: the planner emits
  `create_index` / `drop_index`, the applier executes them and logs to
  `sft_schema_migrations`. Do not manage indexes as a side channel.

## Uniqueness (sft_unique_slot mechanism)
- `unique = TRUE` is enforced by a composite UNIQUE index
  `(db_column, sft_unique_slot)`, not only by the R-side check.
- Live rows have `sft_unique_slot = 0`; soft-deleted rows get
  `sft_unique_slot = sft_id`. A value can be reused after soft-delete while live
  duplicates are rejected at the database level.
- Restore resets the slot to 0; a restore whose unique value is now held by a
  live row must fail with a clear message.
- Empty values of unique fields are stored as NULL (distinct in the index),
  matching the R-side check that exempts empty values. Preserve this in
  `sft_record_field_values`.
- The probe verifies expected unique indexes actually exist, because DDL is not
  transactional on every backend.

## Soft-delete & audit
- No hard deletes: `sft_is_deleted` flag plus `sft_deleted_at` / `sft_deleted_by`.
  The audit log records old/new data, changed fields, user, and reason. Restore
  reads the latest non-deleted snapshot from the audit log.

## Gotchas
- Line endings are mixed per file (some CRLF, some LF). Preserve each file's
  existing endings when editing.
- For sharing/release, build with `devtools::build()` — never zip the working
  directory (it pulls in caches / node_modules).
- Schema tests use membership checks (`%in%`), so a new system column is usually
  fine; still check the `nrow(plan$actions)` assertions in test-db-schema.R.

## Layout
- `R/` (~31 files). Core: form.R, field.R (declarative); db_*.R
  (connect/schema/migration/crud/restore/audit); render_*.R; mod_*.R (Shiny
  modules — mod_form.R is the large orchestrator); input_*.R; validate*.R.
- `tests/testthat/` testthat suite; `man/` generated docs; `vignettes/`;
  `inst/examples/` runnable demos.
