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
    show_audit = FALSE,
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

# --- "How it is built" demo scaffolding (not part of the form API) -----------
# Self-contained: renders this file's own #> STEP / #> NOTE / #> END blocks as
# numbered cards beside the running app. Only the form()/form_server() code
# above is shinyformtools; everything in this block just draws the demo page.
neutral_buttons <- list(
  button_classes = list(
    open_add = "btn-default", open_edit = "btn-default", delete = "btn-default",
    open_deleted_records = "btn-default", open_column_selection = "btn-default"
  )
)

demo_steps <- function(path) {
  lines <- readLines(path, warn = FALSE)
  out <- list()
  cur <- NULL
  push <- function() {
    if (is.null(cur)) return(invisible())
    code <- cur$code
    while (length(code) && !nzchar(trimws(code[1]))) code <- code[-1]
    while (length(code) && !nzchar(trimws(code[length(code)]))) code <- code[-length(code)]
    cur$code <- code
    out[[length(out) + 1L]] <<- cur
    cur <<- NULL
  }
  for (ln in lines) {
    title <- sub("^#>\\s*STEP:\\s*", "", ln)
    if (!identical(title, ln)) {
      push()
      cur <- list(title = trimws(title), notes = character(), code = character())
      next
    }
    if (grepl("^#>\\s*END\\s*$", ln)) {
      push()
      next
    }
    note <- sub("^#>\\s*NOTE:\\s*", "", ln)
    if (!identical(note, ln) && !is.null(cur)) {
      cur$notes <- c(cur$notes, trimws(note))
      next
    }
    if (!is.null(cur)) cur$code <- c(cur$code, ln)
  }
  push()
  out
}

# Resolve the file actually being run (dev vs installed can diverge), so the
# walkthrough always reflects THIS source. Works under source(), Shiny
# sourceUTF8 (parse+eval) and Rscript; falls back to the bundled copy by name.
demo_self_path <- function() {
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) {
      return(normalizePath(of, winslash = "/", mustWork = FALSE))
    }
  }
  for (i in seq_len(sys.nframe())) {
    sr <- attr(sys.call(i), "srcref")
    sf <- if (!is.null(sr)) attr(sr, "srcfile") else NULL
    if (!is.null(sf) && !is.null(sf$filename) && nzchar(sf$filename)) {
      return(normalizePath(sf$filename, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

how_built <- function(example) {
  path <- demo_self_path()
  if (is.null(path) || !file.exists(path)) path <- example_path(example)
  steps <- tryCatch(demo_steps(path), error = function(e) list())
  src <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  libs <- src[startsWith(src, "library(")]
  launch <- src[startsWith(src, "shinyApp(") | startsWith(src, "shiny::shinyApp(")]
  if (length(libs)) {
    steps <- c(list(list(title = "Load the libraries", notes = character(), code = libs)), steps)
  }
  if (length(launch)) {
    steps <- c(steps, list(list(title = "Run the app", notes = character(), code = launch)))
  }
  shiny::tagList(lapply(seq_along(steps), function(i) {
    st <- steps[[i]]
    shiny::div(
      style = "margin-bottom:1rem;border:1px solid #e1e4e8;border-radius:6px;overflow:hidden;background:#fff;",
      shiny::div(
        style = "padding:0.5rem 0.75rem;background:#f1f5fb;border-bottom:1px solid #e1e4e8;",
        shiny::tags$strong(paste0(i, ". ", st$title))
      ),
      if (length(st$notes)) {
        shiny::tags$p(
          style = "margin:0;padding:0.5rem 0.75rem 0;color:#586069;font-size:0.85rem;",
          paste(st$notes, collapse = " ")
        )
      },
      shiny::tags$pre(
        style = "margin:0.5rem 0 0;padding:0.6rem 0.75rem;background:#f7f7f7;font-size:0.8rem;line-height:1.35;overflow:auto;",
        paste(st$code, collapse = "\n")
      )
    )
  }))
}

demo_page <- function(title, example, app_ui) {
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::fluidRow(
      shiny::column(
        6, shiny::h4("How it is built"),
        shiny::div(style = "height:80vh;overflow:auto;padding-right:0.4rem;", how_built(example))
      ),
      shiny::column(6, shiny::h4("App"), app_ui)
    )
  )
}

ui <- demo_page(
  "Districts with a fixed geometry", "app_shape_map",
  shiny::tagList(
    form_ui(
      "districts",
      show_user = FALSE,
      show_audit = FALSE,
      show_add = FALSE,
      show_refresh_table = FALSE,
      show_versions = FALSE,
      show_column_settings = FALSE,
      show_column_selection = FALSE,
      button_options = neutral_buttons
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
)

shinyApp(ui, server)
