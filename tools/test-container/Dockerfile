# Test environment for shinyformtools, used because Windows Smart App Control
# blocks some R package DLLs on the host. r2u serves CRAN as Ubuntu binaries,
# so the install takes minutes, not an hour of compiling sf.
FROM rocker/r2u:24.04

RUN install.r \
    devtools testthat withr pkgload \
    DBI DT jsonlite RSQLite shiny shinyjs shinyTime shinyWidgets uuid \
    commonmark dplyr duckdb knitr rmarkdown RMariaDB openxlsx \
    shinymanager shinyglide sf geojsonsf leaflet

WORKDIR /pkg
