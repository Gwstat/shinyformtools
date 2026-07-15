# Styling and transforming the records DataTable (no pagination, per-column
# search, colorization) - all through form_server() arguments.
#
#   1. No pagination - table_options = list(paging = FALSE, dom = "t"):
#      `dom = "t"` shows just the table (no length menu, global search box, info
#      line or pager); `paging = FALSE` puts every row on one scrolling page.
#
#   2. Per-column search adapting to the column type - table_filter = "top":
#      DT adds a search control to each column header that matches the column's
#      R type: a range slider for the numeric "Score", a dropdown for the
#      "Team" factor, and a text box for "Name".
#
#   3. Colorization - table_format = function(table) ...:
#      table_format receives the built DT widget (its columns already carry their
#      display labels) and returns a styled widget. Here DT::formatStyle() colours
#      the Score cell background by value band and the Team text by category.
#
# Run with: shinyformtools::run_example("app_table_style")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: A plain form() with three fields (Name, Team, Score). The records table
#> NOTE: styling below is layered on entirely through form_server() arguments -
#> NOTE: the form description itself is unchanged.
people_form <- form(
  form_id = "people_style",
  form_name = "People",
  table_name = "people_style",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "team", label = "Team", input_type = "selectInput",
      args = list(choices = c("Sales", "Support", "Engineering"), selected = "Sales"),
      col = 1, pos = 2
    ),
    form_field(
      id = "score", label = "Score", input_type = "numericInput",
      args = list(value = 50, min = 0, max = 100), col = 2, pos = 1
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(people_form, conn = conn, user = "demo")

  if (nrow(fetch_records(people_form, conn = conn)) == 0L) {
    seed <- list(
      list(name = "Ada Lovelace", team = "Engineering", score = 92),
      list(name = "Grace Hopper", team = "Engineering", score = 78),
      list(name = "Alan Turing", team = "Support", score = 65),
      list(name = "Katherine Johnson", team = "Sales", score = 88),
      list(name = "Edsger Dijkstra", team = "Support", score = 35),
      list(name = "Margaret Hamilton", team = "Sales", score = 55)
    )
    for (row in seed) insert_record(people_form, row, conn = conn, user = "demo")
  }
})

#> STEP: Style the records table
#> NOTE: This whole example is about styling the records table. style_table()
#> NOTE: receives the built DT widget (columns already carry their display
#> NOTE: labels) and colours the Score background by band and the Team text by
#> NOTE: category, then returns the styled widget.
# Colour the Score background by band and the Team text by category. Columns are
# referenced by their displayed label (the header text the table shows).
style_table <- function(table) {
  table |>
    DT::formatStyle(
      "Score",
      backgroundColor = DT::styleInterval(c(50, 75), c("#f8d7da", "#fff3cd", "#d4edda")),
      fontWeight = "bold"
    ) |>
    DT::formatStyle(
      "Team",
      color = DT::styleEqual(
        c("Sales", "Support", "Engineering"),
        c("#1565c0", "#6a1b9a", "#2e7d32")
      ),
      fontWeight = "bold"
    )
}
#> END

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("What to look for"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(shiny::tags$b("No pager: "),
        "all rows on one page, no length menu or global search."),
      shiny::tags$li(shiny::tags$b("Type-aware filters: "),
        "the header search is a range slider for ", shiny::tags$em("Score"),
        ", a dropdown for ", shiny::tags$em("Team"), " and a text box for ",
        shiny::tags$em("Name"), "."),
      shiny::tags$li(shiny::tags$b("Colours: "),
        "Score cells are banded red/amber/green; Team text is coloured by group.")
    )
  )
}

#> STEP: Wire the server with table styling
#> NOTE: Every table style is just a form_server() argument: table_options turns
#> NOTE: off paging and chrome, table_filter = "top" adds type-aware per-column
#> NOTE: search controls, and table_format applies style_table() to the widget.
server <- function(input, output, session) {
  form_server(
    id = "people",
    form = people_form,
    user = "demo",
    table_columns = c("sft_id", "name", "team", "score"),
    # 1. No pagination / no global chrome - just the table.
    table_options = list(paging = FALSE, dom = "t"),
    # 2. Per-column search controls that adapt to each column's type.
    table_filter = "top",
    # 3. Colorization applied to the built DT widget.
    table_format = function(table) style_table(table),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Table styling & transforms"),
  how_to(),
  form_ui(
    id = "people",
    title = "People"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_table_style")
#> DEMO END

shinyApp(ui, server)
