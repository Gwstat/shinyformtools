# Grid entry over several records: election advertising counted per polling
# station, party and distance zone, on two days.
#
# One stored record per station x day x party; the three zone counts are its
# numeric fields. grid_server() shows one station's parties as a grid whose
# rows are the records and whose columns are the counts:
#
#   * saving is automatic half a second after the last change, in ONE
#     transaction through upsert_records(): new rows are inserted, changed
#     rows updated, unchanged rows left alone, and rows with nothing in them
#     are soft-deleted, so the table holds only what actually hangs;
#   * on election day the Friday counts show greyed inside the cells (`hint`),
#     as a reference, never as the value;
#   * Enter and the arrow keys walk the cells, a block from a spreadsheet can
#     be pasted, sums follow the entries;
#   * the records table below is the regular form module on the same table,
#     refreshed by the grid's `changed`, with the export buttons.
#
# Run with: shinyformtools::run_example("app_grid_entry")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the records behind the grid
#> NOTE: `tag` and `standort` identify the group the grid shows, `partei` the
#> NOTE: row. The three numeric fields become the grid's columns; a text field
#> NOTE: would stay out of the grid.
tage <- c("Freitag" = "fr", "Wahltag" = "so")
standorte <- c("Grundschule Nord" = "1", "Rathaus" = "2", "Turnhalle West" = "3")
parteien <- c("Partei A", "Partei B", "Partei C", "Sonstige")

beobachtungen_form <- form(
  form_id = "beobachtungen",
  form_name = "Wahlwerbung",
  table_name = "beobachtungen",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "tag", label = "Tag", input_type = "selectInput", args = list(choices = tage)),
    form_field(id = "standort", label = "Standort", input_type = "selectInput", args = list(choices = standorte)),
    form_field(id = "partei", label = "Partei", input_type = "selectInput", args = list(choices = parteien)),
    form_field(id = "zone_1", label = "Unmittelbar", input_type = "numericInput", args = list(value = 0, min = 0)),
    form_field(id = "zone_2", label = "10 bis 20 m", input_type = "numericInput", args = list(value = 0, min = 0)),
    form_field(id = "zone_3", label = "Weiter weg", input_type = "numericInput", args = list(value = 0, min = 0))
  ),
  validation_rules = list(
    forbid_if(
      id = "keine_negativen_zahlen",
      condition = function(values) any(unlist(values[c("zone_1", "zone_2", "zone_3")]) < 0, na.rm = TRUE),
      fields = c("zone_1", "zone_2", "zone_3"),
      message = "Stückzahlen dürfen nicht negativ sein."
    )
  )
)
#> END

# --- Seed Friday's counts for one station ------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(beobachtungen_form, conn = conn, user = "demo")

  if (nrow(fetch_records(beobachtungen_form, conn = conn)) == 0L) {
    upsert_records(
      beobachtungen_form,
      data.frame(
        tag = "fr", standort = "1", partei = c("Partei A", "Partei B", "Partei C"),
        zone_1 = c(2, 0, 1), zone_2 = c(4, 3, 0), zone_3 = c(1, 2, 0)
      ),
      key = c("tag", "standort", "partei"), conn = conn, user = "demo"
    )
  }
})

#> STEP: The Friday counts as a hint on election day
#> NOTE: `hint` gets the group and returns a matrix of the grid's shape or
#> NOTE: NULL. Here it reads the other day's records of the same station.
freitag_als_hinweis <- function(gruppe) {
  if (!identical(gruppe$tag, "so")) {
    return(NULL)
  }
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  alle <- fetch_records(beobachtungen_form, conn = conn)
  vortag <- alle[alle$tag == "fr" & alle$standort == gruppe$standort, , drop = FALSE]
  m <- matrix(NA_real_, nrow = length(parteien), ncol = 3)
  i <- match(vortag$partei, parteien)
  m[i, ] <- as.matrix(vortag[, c("zone_1", "zone_2", "zone_3")])
  m
}
#> END

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Hinweis"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Tag und Standort wählen, Zahlen in das Raster tippen: Enter springt zur nächsten Partei, ",
      "die Pfeiltasten wechseln die Zone, ein Block aus Excel lässt sich einfügen. ",
      "Gespeichert wird von selbst; jede Zeile ist ein Datensatz in der Tabelle unten. ",
      "Am Wahltag steht der Freitagsstand grau in den Zellen."
    )
  )
}

#> STEP: Wire the grid and the records table
#> NOTE: `group` is a reactive over the two selectors, so choosing another
#> NOTE: station re-renders the grid with that station's records. The form
#> NOTE: module below shows the same table and refreshes on the grid's `changed`.
de <- german()

server <- function(input, output, session) {
  raster <- grid_server(
    "raster", beobachtungen_form,
    rows = parteien, key = "partei",
    group = reactive(list(tag = input$tag, standort = input$standort)),
    hint = freitag_als_hinweis,
    user = "demo", language = de
  )

  form_server(
    "erfasst", beobachtungen_form, user = "demo", language = de,
    columns = list(visible = c("tag", "standort", "partei", "zone_1", "zone_2", "zone_3"), persist = FALSE),
    refresh_triggers = raster$changed
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Wahlwerbung zählen"),
  how_to(),
  fluidRow(
    column(3, selectInput("tag", "Tag", choices = tage)),
    column(4, selectInput("standort", "Standort", choices = standorte))
  ),
  grid_ui("raster", label = "Stück je Partei und Zone"),
  hr(),
  form_ui("erfasst", title = "Erfasste Datensätze", language = de, show_export = TRUE)
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_grid_entry")
#> DEMO END

shinyApp(ui, server)
