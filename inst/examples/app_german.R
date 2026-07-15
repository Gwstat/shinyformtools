# A fully German UI from one global switch, with per-form overrides.
#
# English is the package default (for CRAN). use_german() flips every default
# user-facing string to German in one call: button labels, dialog titles,
# notifications, the records-table column headers, the Yes/No deleted flag, the
# audit-log action names ("Erstellt" / "Bearbeitet" / "Gel\u00f6scht") and the audit
# table headers. Call it ONCE, at the top of the app, before form_ui() - it sets
# global options, so both the UI and the server pick it up.
#
# Resolution order is:  English default  <-  use_german()  <-  per-form argument.
# So a per-form `labels` / `messages` argument still wins for situational
# wording. Here the second form renames just "open_edit" to "Editieren" while
# everything else stays German.
#
# The config is plain lists you can edit: german_labels(), german_messages(),
# german_table_labels(). use_english() clears the switch again.
#
# Run with: shinyformtools::run_example("app_german")

library(shiny)
library(shinyformtools)

# One switch -> all defaults become German. (Global; call before building the UI.)
use_german()

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form (German labels)
#> NOTE: The form() description is unchanged from the English examples - only the
#> NOTE: field labels and choices are German here. The German UI chrome (buttons,
#> NOTE: headers, notifications) comes entirely from use_german() above, not from
#> NOTE: the form definition.
contacts_form <- form(
  form_id = "kontakte",
  form_name = "Kontakte",
  table_name = "kontakte",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "email", label = "E-Mail", unique = TRUE, col = 1, pos = 2),
    form_field(
      id = "team", label = "Team", input_type = "selectInput",
      args = list(choices = c("Vertrieb", "Support", "Technik"), selected = "Vertrieb"),
      col = 2, pos = 1
    ),
    form_field(
      id = "notizen", label = "Notizen", input_type = "textAreaInput",
      args = list(value = "", rows = 3), col = 1, pos = 3
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(contacts_form, conn = conn, user = "demo")

  if (nrow(fetch_records(contacts_form, conn = conn)) == 0L) {
    insert_record(contacts_form, list(name = "Ada Lovelace", email = "ada@example.org",
                  team = "Technik", notizen = "Erste Ingenieurin."), conn = conn, user = "demo")
    insert_record(contacts_form, list(name = "Grace Hopper", email = "grace@example.org",
                  team = "Support", notizen = ""), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("What to look for"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(
        shiny::tags$b("One switch: "),
        "every label is German - buttons, the column headers, the deleted flag ",
        "(Ja/Nein) and the audit log (Erstellt / Bearbeitet / Gel\u00f6scht). ",
        "Soft-delete a row to see the German deleted-records dialog."
      ),
      shiny::tags$li(
        shiny::tags$b("Per-form override: "),
        "the button on the second table reads ", shiny::tags$em("Editieren"),
        ", because that form passes labels = list(open_edit = \"Editieren\"); ",
        "everything else stays German."
      )
    )
  )
}

#> STEP: Wire the server (global German + a per-form override)
#> NOTE: Both tables share one form. The first picks up the global German
#> NOTE: defaults from use_german(); the second passes labels = list(open_edit =
#> NOTE: "Editieren") to rename just that one action, showing that a per-form
#> NOTE: argument still wins over the global switch.
server <- function(input, output, session) {
  form_server(
    id = "contacts",
    form = contacts_form,
    user = "demo",
    table_columns = c("sft_easy_id", "name", "email", "team", "sft_is_deleted", "sft_updated_at"),
    persist_column_settings = FALSE
  )

  # Same data, but one situational rename overrides the global German default.
  # ...and to form_server too, so the edit dialog's title matches the button.
  form_server(
    id = "contacts_override",
    form = contacts_form,
    user = "demo",
    labels = list(open_edit = "Editieren"),
    table_columns = c("sft_easy_id", "name", "team"),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
#> NOTE: Two form_ui() calls share the German UI set by use_german(); the second
#> NOTE: also passes labels = list(open_edit = "Editieren") so its button text
#> NOTE: matches the server-side override.
ui <- fluidPage(
  titlePanel("Deutsche Oberfl\u00e4che (use_german)"),
  how_to(),
  shiny::h5("Standard (use_german)"),
  form_ui(
    id = "contacts",
    title = "Kontakte"
  ),
  shiny::tags$hr(),
  shiny::h5("Per-form override: open_edit = \"Editieren\""),
  form_ui(
    id = "contacts_override",
    title = "Kontakte",
    # The button text is built in the UI, so the override goes here...
    labels = list(open_edit = "Editieren")
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_german")
#> DEMO END

shinyApp(ui, server)
