# One form definition, two database backends side by side: SQLite and DuckDB.
#
# The point of this example is that the backend is a single line. The same
# field list drives both tables; only the `db = ...` argument differs
# (db_sqlite() vs db_duckdb()). Everything downstream - schema, migrations,
# CRUD, soft-delete, audit log, the records table - is derived identically, so
# the two tabs behave the same although they store their data in different
# engines.
#
# SQLite ships with the package (RSQLite). DuckDB is optional (Suggests); the
# DuckDB tab is shown only when the 'duckdb' package is installed.
#
# Run with: shinyformtools::run_example("app_backends")

library(shiny)
library(shinyformtools)

have_duckdb <- requireNamespace("duckdb", quietly = TRUE)

#> STEP: One field list, two backends
#> NOTE: The same product_fields() drives both forms; only db = ... differs
#> NOTE: (db_sqlite vs db_duckdb). Everything downstream is derived identically.
# A small function so the two forms are byte-for-byte identical except for the
# backend; this is what makes "swap the backend" a one-line change.
product_fields <- function() {
  list(
    form_field(id = "sku", label = "SKU", mandatory = TRUE, unique = TRUE, col = 1, pos = 1),
    form_field(id = "name", label = "Product", mandatory = TRUE, col = 1, pos = 2),
    form_field(
      id = "category", label = "Category", input_type = "selectInput",
      args = list(choices = c("Hardware", "Software", "Service"), selected = "Hardware"),
      col = 1, pos = 3
    ),
    form_field(
      id = "price", label = "Price", input_type = "numericInput",
      args = list(value = 0, min = 0, max = 1e6), col = 2, pos = 1
    ),
    form_field(
      id = "in_stock", label = "In stock", input_type = "checkboxInput",
      args = list(value = TRUE), col = 2, pos = 2
    ),
    form_field(
      id = "added", label = "Added", input_type = "dateInput",
      args = list(value = Sys.Date()), col = 2, pos = 3
    )
  )
}

sqlite_path <- tempfile(fileext = ".sqlite")
duckdb_path <- tempfile(fileext = ".duckdb")

# Same form_id / table_name on purpose; the two live in separate databases.
sqlite_form <- form(
  form_id = "products",
  form_name = "Products (SQLite)",
  table_name = "products",
  db = db_sqlite(sqlite_path),
  fields = product_fields()
)

duckdb_form <- if (have_duckdb) {
  form(
    form_id = "products",
    form_name = "Products (DuckDB)",
    table_name = "products",
    db = db_duckdb(duckdb_path),
    fields = product_fields()
  )
} else {
  NULL
}
#> END

# --- Seed the same demo rows into whichever backends are available -----------
seed_rows <- list(
  list(sku = "HW-001", name = "Mechanical keyboard", category = "Hardware",
       price = 89.0, in_stock = TRUE),
  list(sku = "SW-014", name = "Backup license (1 yr)", category = "Software",
       price = 49.5, in_stock = TRUE),
  list(sku = "SV-200", name = "Onboarding session", category = "Service",
       price = 250.0, in_stock = FALSE)
)

seed_backend <- function(form_def) {
  conn <- db_connect(form_def$db)
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(form_def, conn = conn, user = "demo")

  if (nrow(fetch_records(form_def, conn = conn)) == 0L) {
    for (row in seed_rows) {
      insert_record(form_def, row, conn = conn, user = "demo")
    }
  }
}

seed_backend(sqlite_form)
if (have_duckdb) seed_backend(duckdb_form)

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Same form, two engines"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "The two tabs are the same ", shiny::tags$code("form()"),
      " - identical fields, validation and CRUD - but one stores its data in ",
      shiny::tags$b("SQLite"), " and the other in ", shiny::tags$b("DuckDB"),
      ". The only difference in the code is ", shiny::tags$code("db_sqlite()"),
      " versus ", shiny::tags$code("db_duckdb()"),
      ". Add or edit a product in each tab; the unique-SKU check and the audit ",
      "log work identically on both.",
      if (!have_duckdb) {
        shiny::tagList(
          shiny::tags$br(),
          shiny::tags$em(
            "The DuckDB tab is hidden because the 'duckdb' package is not ",
            "installed - run install.packages('duckdb') to see it."
          )
        )
      }
    )
  )
}

backend_panel <- function(id, title) {
  shiny::tabPanel(
    title = title,
    shiny::br(),
    form_ui(
      id = id,
      title = title
    )
  )
}

#> STEP: Wire one server per backend
#> NOTE: The two form_server() calls are identical apart from the form they are
#> NOTE: given - the backend never leaks into the module code.
server <- function(input, output, session) {
  shown_columns <- c("sft_id", "sku", "name", "category", "price", "in_stock")

  form_server(
    id = "sqlite",
    form = sqlite_form,
    user = "demo",
    table_columns = shown_columns,
    persist_column_settings = FALSE
  )

  if (have_duckdb) {
    form_server(
      id = "duckdb",
      form = duckdb_form,
      user = "demo",
      table_columns = shown_columns,
      persist_column_settings = FALSE
    )
  }
}
#> END

#> STEP: Build the UI
#> NOTE: Two form_ui() calls, one per backend tab, sit inside a tabsetPanel
#> NOTE: built with do.call() so the DuckDB tab can be omitted entirely when
#> NOTE: the 'duckdb' package is not installed.
ui <- fluidPage(
  titlePanel("Backends: SQLite & DuckDB"),
  how_to(),
  do.call(
    tabsetPanel,
    c(
      list(backend_panel("sqlite", "SQLite")),
      if (have_duckdb) list(backend_panel("duckdb", "DuckDB"))
    )
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_backends")
#> DEMO END

shinyApp(ui, server)
