# Teaching shinyformtools a custom input with register_input().
#
# The built-in input types cover the common cases, but any Shiny input function -
# for example one from shinyWidgets - can be plugged in with register_input() and
# then used as an input_type in form_field(), exactly like a built-in. The widget
# is rendered in the add/edit dialogs, its value is stored, restored and shown in
# the records table, and (with an update_fun) it joins dynamic values/choices.
#
# This app registers two shinyWidgets inputs:
#   * knobInput   - a single numeric dial (value_arg = "value", stored in a REAL
#                   column, with updateKnobInput for dynamic values).
#   * pickerInput - a multi-select (value_arg = "selected", multiple = TRUE, so
#                   the chosen values are stored as a JSON array and restored to a
#                   vector, and shown joined in the table).
#
# Requires the 'shinyWidgets' package (a shinyformtools dependency).
#
# Run with: shinyformtools::run_example("app_custom_input")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Register the custom inputs
#> NOTE: register_input() teaches the package an additional input function. Call
#> NOTE: it BEFORE the form_field() that uses the new input_type. knobInput is a
#> NOTE: single numeric dial; pickerInput is multi-valued, so multiple = TRUE
#> NOTE: stores the selection as a JSON array and decodes it back to a vector.
register_input(
  "knobInput",
  fun = shinyWidgets::knobInput,
  value_arg = "value",
  update_fun = shinyWidgets::updateKnobInput
)

register_input(
  "pickerInput",
  fun = shinyWidgets::pickerInput,
  value_arg = "selected",
  multiple = TRUE
)
#> END

#> STEP: Describe the form
#> NOTE: The two registered names are used as input_type just like a built-in.
#> NOTE: A custom input that stores a number sets db_type explicitly (the default
#> NOTE: column type for an unknown input is TEXT); args are passed straight to
#> NOTE: the widget function.
tracks_form <- form(
  form_id = "tracks_custom",
  form_name = "Tracks",
  table_name = "tracks_custom",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "title", label = "Title", mandatory = TRUE, col = 1, pos = 1),
    # A single numeric dial, stored in a REAL column.
    form_field(
      id = "rating", label = "Rating", input_type = "knobInput",
      args = list(min = 0, max = 10, value = 5, displayPrevious = TRUE,
                  width = 90, height = 90),
      db_type = "REAL", col = 2, pos = 1
    ),
    # A multi-select; the chosen genres are stored as a JSON array.
    form_field(
      id = "genres", label = "Genres", input_type = "pickerInput",
      args = list(
        choices = c("Rock", "Jazz", "Folk", "Electronic", "Classical"),
        options = list(`actions-box` = TRUE)
      ),
      col = 1, pos = 2
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(tracks_form, conn = conn, user = "demo")

  if (nrow(fetch_records(tracks_form, conn = conn)) == 0L) {
    insert_record(tracks_form, list(
      title = "Midnight Drive", rating = 8, genres = c("Rock", "Electronic")
    ), conn = conn, user = "demo")
    insert_record(tracks_form, list(
      title = "Slow Sunday", rating = 6, genres = c("Jazz", "Folk")
    ), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Try it"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Add or edit a track. The ", shiny::tags$strong("Rating"), " dial and the ",
      shiny::tags$strong("Genres"), " multi-select are both shinyWidgets inputs ",
      "registered with ", shiny::tags$code("register_input()"),
      ". The dial value round-trips through a REAL column; the genres are stored ",
      "as a JSON array and shown joined in the table."
    )
  )
}

#> STEP: Wire the server
#> NOTE: Nothing special is needed here: once the inputs are registered, the form
#> NOTE: server renders, validates, stores and restores them like any built-in.
server <- function(input, output, session) {
  form_server(
    id = "tracks",
    form = tracks_form,
    user = "demo",
    table_columns = c("sft_easy_id", "title", "rating", "genres"),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Custom input (register_input)"),
  how_to(),
  form_ui(
    id = "tracks",
    title = "Tracks"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_custom_input")
#> DEMO END

shinyApp(ui, server)
