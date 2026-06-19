# Contributing to shinyformtools

Thanks for taking the time to contribute! Bug reports, feature ideas, and pull
requests are all welcome.

## Reporting bugs and requesting features

Open an issue at
<https://github.com/Gwstat/shinyformtools/issues>. For bugs, please include:

- a minimal reproducible example (a small `form()` + the failing call),
- the backend you used (SQLite / DuckDB / MariaDB),
- your R version and the output of `sessionInfo()`.

## Development setup

```r
# from a fork/clone of the repo
install.packages("devtools")
devtools::install_dev_deps()   # installs Imports + Suggests
```

## Everyday workflow

```r
devtools::load_all()   # reload the package during development
devtools::document()   # regenerate man/ and NAMESPACE after roxygen changes
devtools::test()       # run the testthat suite
devtools::check()      # full R CMD check before opening a PR
```

`R CMD check` must stay clean: **0 errors, 0 warnings, 0 notes**.

## Running the tests

Run the full suite before committing. Some tests are conditional on optional
backends:

- **DuckDB** tests run only when the `duckdb` package is installed.
- **MariaDB** tests run only when `SFT_MARIADB_USER` / `SFT_MARIADB_PASSWORD`
  are set and point at a reachable server. A ready-to-use server is bundled at
  `inst/docker/docker-compose.mariadb.yml`:

  ```bash
  docker compose -f inst/docker/docker-compose.mariadb.yml up -d
  ```

## Code conventions

- Comments and roxygen are written in **English**; names are **snake_case**.
- Exported (public) functions use bare, descriptive names without a prefix
  (`form()`, `form_field()`, `form_server()`, `db_connect()`, ...). Internal
  helpers and the database system columns keep the `sft_` prefix
  (`sft_quote_identifier()`, `sft_id`, ...). The S3 classes are `sft_form` /
  `sft_field` / `sft_migration_plan`.
- **All SQL uses parameterized queries** (`?` placeholders + `params`); never
  interpolate values into SQL. Quote identifiers with the internal
  `sft_quote_identifier()` wrapper rather than building quoted strings by hand.
- Mutations run inside a transaction and write an audit-log entry; deletes are
  soft only.
- Keep R source **ASCII** (escape non-ASCII as `\uXXXX`).
- Every exported function needs an `@examples` block — runnable where feasible,
  or wrapped in `\dontrun{}` for examples that need a live Shiny session, an
  external service, or an optional (Suggests) package.

## Pull requests

1. Branch from `main` and keep each PR focused on one change.
2. Add or update tests for the behaviour you change.
3. Run `devtools::document()` and `devtools::check()` (expect 0/0/0).
4. Add a bullet to `NEWS.md` for any user-facing change.

## License

By contributing, you agree that your contributions are licensed under the
project's [MIT license](../LICENSE).
