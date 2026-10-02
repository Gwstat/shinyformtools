## Test environments

* Local: Windows 11, R 4.6.0.

External checks are pending and will be run before submission:

* win-builder (devel and release)
* a multi-platform check (R-hub)

## R CMD check results

To be completed here from the actual check output (local and the external
checks above) before submission. No results are claimed until they have
actually been produced.

## Notes for CRAN

* Examples that open a database connection write to `tempfile()` and clean up
  after themselves.
* Examples for the 'Shiny' module functions (`form_ui()`, `form_server()`) are
  wrapped in `\dontrun{}` because they require a running 'Shiny' session.
* Optional backends and integrations ('DuckDB', 'MariaDB', 'shinymanager', 'sf')
  are in `Suggests`; code guards on them with `requireNamespace()`.
