## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* Local: Windows 11, R 4.6.0 — 0 errors, 0 warnings, 0 notes.
* (Before submission: win-builder devel/release and R-hub.)

## Notes for CRAN

* Examples that open a database connection write to `tempfile()` and clean up
  after themselves. Examples for the 'Shiny' module functions (`form_ui()`,
  `form_server()`) are wrapped in `\dontrun{}` because they require a running
  'Shiny' session.
* Optional backends and integrations ('DuckDB', 'MariaDB', 'shinymanager', 'sf')
  are in Suggests; code guards on them with `requireNamespace()`.
