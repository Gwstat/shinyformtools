# Two forms in one app, each in its own language.
#
# use_german() switches the whole R process: every form of every session turns
# German. A language() object is scoped to the form it is passed to, so one app
# can serve a German and an English form side by side - or pick the language
# per user. german() and english() are ready-made; language() builds your own.
#
#   - Both tables show the same records. Add or edit on either side.
#   - Leave "Name" empty and save: the refusal comes in that form's language.
#   - Look at the audit tables: headers and action names follow the form too.
#   - The third form switches language while the app runs: `language` may be a
#     reactive. Its buttons are redrawn because form_ui() sits in a renderUI().
#
# Run with: shinyformtools::run_example("app_language")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: One description, used twice below. Nothing in it is language-specific:
#> NOTE: the field labels are your own text, the package's text (buttons,
#> NOTE: dialogs, messages, table headers) comes from the language.
guests_form <- form(
  form_id = "guests_language",
  form_name = "Guests",
  table_name = "guests_language",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "email", label = "E-Mail", unique = TRUE, col = 1, pos = 2),
    form_field(
      id = "diet", label = "Diet", input_type = "selectInput",
      args = list(choices = c("No preference", "Vegetarian", "Vegan"), selected = "No preference"),
      col = 2, pos = 1
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(guests_form, conn = conn, user = "demo")

  if (nrow(fetch_records(guests_form, conn = conn)) == 0L) {
    insert_record(guests_form, list(name = "Ada Lovelace", email = "ada@example.com",
                  diet = "Vegetarian"), conn = conn, user = "demo")
    insert_record(guests_form, list(name = "Grace Hopper", email = "grace@example.com",
                  diet = "No preference"), conn = conn, user = "demo")
  }
})

#> STEP: Choose the languages
#> NOTE: german() is the bundled German pack; start from it and rename a single
#> NOTE: button by assigning into it. language_keys() lists every key with its
#> NOTE: English default, so a translation of your own is one language() call.
de <- german()
de$labels$open_add <- "Gast anlegen"

en <- language(labels = list(open_add = "Add guest"))
#> END

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Note"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Both sides edit the same table. Try saving with an empty ",
      shiny::tags$em("Name"), " on each side: buttons, dialog, the refusal and the ",
      "audit table each follow the ", shiny::tags$code("language"),
      " that form was given."
    )
  )
}

#> STEP: Wire the server
#> NOTE: The same language object goes to form_server(): validation messages,
#> NOTE: table and audit headers and the DataTables chrome are computed on the
#> NOTE: server. Each form's `changed` refreshes the other, so both sides stay in
#> NOTE: step while showing the same records in two languages.
server <- function(input, output, session) {
  german_side <- form_server(
    id = "guests_de", form = guests_form, user = "demo", language = de,
    columns = list(visible = c("sft_id", "name", "email", "diet"), persist = FALSE),
    refresh_triggers = function() english_side$changed()
  )

  english_side <- form_server(
    id = "guests_en", form = guests_form, user = "demo", language = en,
    columns = list(visible = c("sft_id", "name", "email", "diet"), persist = FALSE),
    refresh_triggers = function() german_side$changed()
  )

  # A language that changes while the app runs: pass a reactive. Tables,
  # dialogs and messages follow it at once. The buttons form_ui() draws are
  # static HTML, so form_ui() is rendered from the same reactive.
  live_language <- reactive(if (identical(input$live_language, "de")) de else en)

  form_server(
    id = "guests_live", form = guests_form, user = "demo", language = live_language,
    columns = list(visible = c("sft_id", "name", "email", "diet"), persist = FALSE),
    refresh_triggers = list(function() german_side$changed(), function() english_side$changed())
  )

  output$live_form <- renderUI({
    form_ui("guests_live", title = "Live", language = live_language(), show_audit = TRUE)
  })
}
#> END

#> STEP: Build the UI
#> NOTE: form_ui() gets the same language as its form_server(): the buttons and
#> NOTE: the audit title are rendered here.
ui <- fluidPage(
  titlePanel("One app, two languages"),
  how_to(),
  fluidRow(
    column(6, form_ui("guests_de", title = "Deutsch", language = de, show_audit = TRUE)),
    column(6, form_ui("guests_en", title = "English", language = en, show_audit = TRUE))
  ),
  hr(),
  radioButtons(
    "live_language", "Language of the third form",
    choices = c("Deutsch" = "de", "English" = "en"), selected = "de", inline = TRUE
  ),
  uiOutput("live_form")
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_language")
#> DEMO END

shinyApp(ui, server)
