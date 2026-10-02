# A German talk-registration form spread over three slides, each with a heading.
#
# Two things this example shows:
#
#   1. Slide headings. `form(slide_labels = c(...))` names the slides of a
#      multi-slide form; the name appears as a heading above each slide, in the
#      add and the edit dialog alike. A single field can override its slide's
#      heading with `form_field(slide_label = ...)`. Slides without a label get
#      no heading, so existing slide forms look as before.
#
#   2. German throughout. `german()` translates the package's own text (buttons,
#      dialogs, messages, the Back / Next controls of the wizard, table headers);
#      the field labels and slide headings are your own German text.
#
# Requires the optional 'shinyglide' package (for the slides).
#
# Run with: shinyformtools::run_example("app_presentation_german")

library(shiny)
library(shinyformtools)

if (!requireNamespace("shinyglide", quietly = TRUE)) {
  stop(
    "The 'app_presentation_german' example needs the 'shinyglide' package. ",
    "install.packages('shinyglide').",
    call. = FALSE
  )
}

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form, one slide per topic
#> NOTE: `slide` groups the fields; `slide_labels` names the groups in order
#> NOTE: (slide 0, 1, 2). Fields on one slide are laid out by `col` / `pos`.
talks_form <- form(
  form_id = "talks",
  form_name = "Vorträge",
  table_name = "talks",
  db = db_sqlite(db_path),
  slide_labels = c("Zur Person", "Zum Vortrag", "Technik und Termin"),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, slide = 0, pos = 1),
    form_field(id = "email", label = "E-Mail", mandatory = TRUE, unique = TRUE, slide = 0, pos = 2),
    form_field(
      id = "affiliation", label = "Einrichtung", slide = 0, pos = 3
    ),
    form_field(id = "title", label = "Titel des Vortrags", mandatory = TRUE, slide = 1, pos = 1),
    form_field(
      id = "abstract", label = "Kurzfassung", input_type = "textAreaInput",
      args = list(rows = 5), slide = 1, pos = 2
    ),
    form_field(
      id = "duration", label = "Dauer (Minuten)", input_type = "numericInput",
      args = list(value = 20, min = 5, max = 90, step = 5), slide = 1, pos = 3
    ),
    form_field(
      id = "equipment", label = "Benötigte Technik", input_type = "checkboxGroupInput",
      args = list(choices = c("Beamer", "Mikrofon", "Whiteboard", "Internet")),
      slide = 2, pos = 1
    ),
    form_field(
      id = "preferred_day", label = "Wunschtag", input_type = "selectInput",
      args = list(choices = c("Egal", "Donnerstag", "Freitag"), selected = "Egal"),
      slide = 2, pos = 2
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(talks_form, conn = conn, user = "demo")

  if (nrow(fetch_records(talks_form, conn = conn)) == 0L) {
    insert_record(
      talks_form,
      list(
        name = "Ada Lovelace", email = "ada@example.org", affiliation = "Analytical Engine Ltd.",
        title = "Programme für eine Maschine, die es noch nicht gibt",
        abstract = "Wie man Rechenschritte notiert, bevor jemand sie ausführen kann.",
        duration = 30, equipment = c("Beamer", "Mikrofon"), preferred_day = "Donnerstag"
      ),
      conn = conn, user = "demo"
    )
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Hinweis"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "„Vortrag anmelden“ öffnet einen Dialog mit drei Seiten. ",
      "Die Überschrift jeder Seite kommt aus ", shiny::tags$code("slide_labels"),
      ", die Schaltflächen Zurück / Weiter aus ", shiny::tags$code("german()"), "."
    )
  )
}

#> STEP: Choose the language
#> NOTE: german() covers the package's text. One button is renamed by
#> NOTE: assigning into the object.
de <- german()
de$labels$open_add <- "Vortrag anmelden"
#> END

#> STEP: Wire the server
server <- function(input, output, session) {
  form_server(
    id = "talks", form = talks_form, user = "demo", language = de,
    columns = list(visible = c("sft_id", "name", "title", "duration", "preferred_day"), persist = FALSE)
  )
}
#> END

#> STEP: Build the UI
#> NOTE: form_ui() gets the same language object, so the buttons it draws are
#> NOTE: German as well.
ui <- fluidPage(
  titlePanel("Vortragsanmeldung"),
  how_to(),
  form_ui("talks", title = "Angemeldete Vorträge", language = de)
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_presentation_german")
#> DEMO END

shinyApp(ui, server)
