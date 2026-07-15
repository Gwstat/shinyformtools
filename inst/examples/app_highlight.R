# Reactively highlighting fields (and their tab), plus an automatic "changed" glow.
#
# Two independent highlight channels, both driven from form_server():
#
#   1. Caller-driven red glow - highlight_fields (+ highlight_tab):
#      Tick fields in the "Flag for attention" box. The chosen inputs glow red in
#      the add AND edit forms, and (highlight_tab = TRUE) the tab that holds a
#      flagged field glows too - handy when the field lives on a tab you are not
#      looking at. highlight_fields is a reactive, so the glow tracks the box
#      live; clearing the box clears the glow.
#
#   2. Automatic blue "changed" glow - show_changed (on by default):
#      Open Edit on a row: any field whose current value differs from the value
#      it had when the record was first added (its original audit-log version)
#      glows blue. It marks fields that have been edited at some point since
#      creation. The comparison is type-tolerant (5 vs "5", TRUE vs 1, a date vs
#      its string do not false-positive), so only real changes light up.
#
# The form is laid out over two tabs (form_field(tab = ...)) so the tab glow has
# something to point at. Colours are overridable via highlight_color /
# changed_color.
#
# Run with: shinyformtools::run_example("app_highlight")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Lay the form out over two tabs
#> NOTE: form_field(tab = 0/1) splits the inputs across two tabs. With the tab
#> NOTE: glow on, flagging a field on a tab you are not viewing still draws the
#> NOTE: eye to the right tab.
profile_form <- form(
  form_id = "profiles",
  form_name = "Profiles",
  table_name = "profiles",
  db = db_sqlite(db_path),
  fields = list(
    # Tab 1 - Personal
    form_field(id = "name", label = "Name", mandatory = TRUE, tab = 0, pos = 1),
    form_field(id = "email", label = "Email", tab = 0, pos = 2),
    form_field(
      id = "age", label = "Age", input_type = "numericInput",
      args = list(value = 30, min = 0), tab = 0, pos = 3
    ),
    # Tab 2 - Address
    form_field(id = "city", label = "City", tab = 1, pos = 1),
    form_field(id = "postcode", label = "Postcode", tab = 1, pos = 2),
    form_field(
      id = "country", label = "Country", input_type = "selectInput",
      args = list(choices = c("NL", "DE", "FR", "UK"), selected = "NL"),
      tab = 1, pos = 3
    )
  ),
  tab_labels = c("Personal", "Address")
)
#> END

# Field ids/labels offered in the "flag" picker below.
flag_choices <- c(
  Name = "name", Email = "email", Age = "age",
  City = "city", Postcode = "postcode", Country = "country"
)

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(profile_form, conn = conn, user = "demo")

  if (nrow(fetch_records(profile_form, conn = conn)) == 0L) {
    ada <- insert_record(profile_form, list(name = "Ada Lovelace", email = "ada@example.org",
                  age = 36, city = "London", postcode = "EC1", country = "UK"),
                  conn = conn, user = "demo")
    insert_record(profile_form, list(name = "Guido van Rossum", email = "guido@example.org",
                  age = 70, city = "Haarlem", postcode = "2011", country = "NL"),
                  conn = conn, user = "demo")

    # Edit Ada's City after creation, so the blue "changed since add" glow has
    # something to point at the first time you open her Edit dialog.
    update_record(profile_form, list(name = "Ada Lovelace", email = "ada@example.org",
                  age = 36, city = "Manchester", postcode = "EC1", country = "UK"),
                  record_id = ada$sft_id[1], conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Try it"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(shiny::tags$b("Red flag: "),
        "Tick some fields in ", shiny::tags$em("Flag for attention"), ", then ",
        "open Add or Edit - the chosen inputs glow red, and the tab holding a ",
        "flagged field glows too. Untick to clear."),
      shiny::tags$li(shiny::tags$b("Blue changed: "),
        "Open Edit on a row - any field that has been changed since the record ",
        "was first added glows blue (Ada's City was edited below, so it glows). ",
        "Edit and save another field to see it light up too.")
    )
  )
}

#> STEP: Wire the highlight arguments on the server
#> NOTE: highlight_fields takes a reactive, so the red glow tracks the checkbox
#> NOTE: group live; highlight_tab also glows the owning tab. show_changed (on by
#> NOTE: default) adds the automatic blue glow on edited fields. Colours are
#> NOTE: overridable via highlight_color / changed_color.
server <- function(input, output, session) {
  form_server(
    id = "profile",
    form = profile_form,
    user = "demo",
    persist_column_settings = FALSE,
    table_columns = c("sft_id", "name", "email", "age",
                      "city", "postcode", "country"),
    # 1. Reactive red glow on the flagged fields, plus their tab.
    highlight_fields = reactive(input$flag),
    highlight_tab = TRUE,
    # 2. Automatic blue glow when an edit-form value differs from the stored one.
    show_changed = TRUE
  )
}
#> END

#> STEP: Build the UI
#> NOTE: form_ui() injects the reactive highlight stylesheet (a single <style>
#> NOTE: block kept in sync with highlight_fields/show_changed) alongside the
#> NOTE: action buttons, records table and audit log.
ui <- fluidPage(
  titlePanel("Reactive field & tab highlighting"),
  shiny::checkboxGroupInput(
    "flag", "Flag for attention (red glow)",
    choices = flag_choices, inline = TRUE
  ),
  how_to(),
  form_ui(
    id = "profile",
    title = "Profiles"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_highlight")
#> DEMO END

shinyApp(ui, server)
