# Districts with a fixed (non-editable) geometry per record.
#
# Demonstrates shape_field() + attach_shapes(): each record has editable
# attributes plus a boundary geometry the form never touches. The page shows the
# source on the left and the running app (form + map) on the right.
#
# Geometry is open data with no external dependency: the North Carolina counties
# shapefile bundled with the sf package. Swap in any sf object the same way.
#
# Run with: shinyformtools::run_example("app_shape_map")

library(shiny)
library(shinyformtools)

for (pkg in c("sf", "leaflet")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "The 'app_shape_map' example needs the '", pkg, "' package. ",
      "install.packages('", pkg, "').",
      call. = FALSE
    )
  }
}

# --- Open geometry: NC counties, the shapefile bundled with sf ---------------
districts_sf <- sf::st_read(
  system.file("shape/nc.shp", package = "sf"),
  quiet = TRUE
)

# --- Declarative form: editable attributes + one fixed shape field -----------
db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form (with a shape field)
#> NOTE: shape_field() declares a fixed, non-editable geometry column alongside
#> NOTE: ordinary editable attributes. The form derives the schema, dialogs and
#> NOTE: records table; the geometry is loaded out of band and never touched by
#> NOTE: the add/edit machinery.
district_form <- form(
  form_id = "districts",
  table_name = "districts",
  db_path = db_path,
  fields = list(
    form_field(id = "fips", label = "FIPS code", mandatory = TRUE, editable = FALSE),
    form_field(id = "name", label = "District", mandatory = TRUE),
    form_field(id = "births", label = "Births (1974)", input_type = "numericInput"),
    shape_field(id = "geometry", label = "Boundary", crs = 4326)
  )
)
#> END

#> STEP: Attach the fixed geometries
#> NOTE: Insert the attribute rows, then attach_shapes() loads each record's
#> NOTE: boundary from the sf object, matching features to records by the FIPS
#> NOTE: key. The geometry is stored backend-neutrally as text, transactionally.
# --- One-time setup: insert records, then attach the fixed geometries ---------
setup_conn <- db_connect(db_path)
onStop(function() db_disconnect(setup_conn))

init_db(district_form, conn = setup_conn)

if (nrow(fetch_records(district_form, conn = setup_conn)) == 0L) {
  attrs <- sf::st_drop_geometry(districts_sf)

  for (i in seq_len(nrow(attrs))) {
    insert_record(
      district_form,
      record = list(
        fips = as.character(attrs$FIPS[i]),
        name = as.character(attrs$NAME[i]),
        births = as.numeric(attrs$BIR74[i])
      ),
      conn = setup_conn,
      user = "loader"
    )
  }

  attach_shapes(
    district_form,
    shapes = districts_sf,
    key = c(fips = "FIPS"),
    conn = setup_conn,
    user = "loader"
  )
}
#> END

shape_field <- Filter(
  function(field) identical(field$type, "shape"),
  district_form$fields
)[[1L]]

#> STEP: Wire the server and draw the map
#> NOTE: form_server() returns reactive helpers; state$records() invalidates on
#> NOTE: every edit / delete / restore. decode_shape() turns each record's stored
#> NOTE: geometry text back into an sf geometry so the leaflet map redraws live.
# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # The module returns reactive helpers. `state$records` invalidates on every
  # edit / delete / restore, so the map below redraws immediately.
  state <- form_server(
    "districts",
    form = district_form,
    conn = setup_conn,
    table_columns = c("fips", "name", "births"),
    can_add = FALSE,
    # Restoring a specific older version is offered in the view-case accordion;
    # restoring a deleted record (latest version) is one click in the deleted
    # records dialog.
    can_view_versions = TRUE,
    can_change_column_settings = FALSE,
    can_select_column_view = FALSE,
    persist_column_settings = FALSE
  )

  output$map <- leaflet::renderLeaflet({
    records <- state$records() # reactive: tracks every mutation
    records <- records[!is.na(records$geometry) & nzchar(records$geometry), , drop = FALSE]

    map <- leaflet::addTiles(leaflet::leaflet())

    if (nrow(records) == 0L) {
      return(map)
    }

    records$births <- suppressWarnings(as.numeric(records$births))
    geometry <- decode_shape(records$geometry, shape_field)
    data <- sf::st_sf(records[, c("fips", "name", "births")], geometry = geometry)
    pal <- leaflet::colorNumeric("YlOrRd", domain = data$births)

    map <- leaflet::addPolygons(
      leaflet::addTiles(leaflet::leaflet(data)),
      weight = 1,
      color = "#666666",
      fillColor = ~ pal(births),
      fillOpacity = 0.7,
      label = ~ paste0(name, ": ", births)
    )
    leaflet::addLegend(
      map,
      pal = pal,
      values = data$births,
      title = "Births (1974)",
      opacity = 0.7
    )
  })
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Districts with a fixed geometry"),
  form_ui(
    "districts",
    show_add = FALSE,
    show_refresh_table = FALSE,
    show_versions = FALSE
  ),
  # The map gets its own contained div so it never overlaps the table.
  shiny::tags$div(
    style = "margin-top: 1.5rem;",
    shiny::h4("Map"),
    shiny::tags$div(
      style = paste(
        "position: relative; height: 420px; border: 1px solid #cccccc;",
        "border-radius: 4px; overflow: hidden;"
      ),
      leaflet::leafletOutput("map", height = "100%")
    )
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_shape_map")
#> DEMO END

shinyApp(ui, server)
