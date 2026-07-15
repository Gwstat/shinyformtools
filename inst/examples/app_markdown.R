# Rendering a field as Markdown in the records table.
#
# A field declared with form_field(markdown = TRUE) has its stored text rendered
# as Markdown in the records table (and the versions table) instead of being shown
# as plain text. You type Markdown in the edit dialog - **bold**, bullet lists,
# [links](https://example.org) - and the table shows it formatted.
#
# It is safe for user-entered content: the value is HTML-escaped before rendering
# and the output is URL-sanitized, so a pasted <script> or javascript: link cannot
# execute. Try it - paste "<script>alert(1)</script>" into a description and it
# shows as inert text.
#
# Requires the 'commonmark' package (a lightweight Suggests).
#
# Run with: shinyformtools::run_example("app_markdown")

library(shiny)
library(shinyformtools)

if (!requireNamespace("commonmark", quietly = TRUE)) {
  stop(
    "The 'app_markdown' example needs the 'commonmark' package. ",
    "install.packages('commonmark').",
    call. = FALSE
  )
}

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: One form() description derives the schema, the add/edit dialogs and the
#> NOTE: records table. The Description field is declared with markdown = TRUE, so
#> NOTE: its stored text is rendered as Markdown in the records and versions
#> NOTE: tables (the edit dialog still shows the raw source).
notes_form <- form(
  form_id = "notes_md",
  form_name = "Notes",
  table_name = "notes_md",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "title", label = "Title", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "status", label = "Status", input_type = "selectInput",
      args = list(choices = c("Open", "Done"), selected = "Open"), col = 2, pos = 1
    ),
    # The value is typed as Markdown and rendered formatted in the table.
    form_field(
      id = "description", label = "Description (Markdown)",
      input_type = "textAreaInput", args = list(value = "", rows = 6),
      markdown = TRUE, col = 1, pos = 2
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(notes_form, conn = conn, user = "demo")

  if (nrow(fetch_records(notes_form, conn = conn)) == 0L) {
    insert_record(notes_form, list(
      title = "Release checklist", status = "Open",
      description = paste(
        "Steps **before** shipping:",
        "",
        "- run `R CMD check`",
        "- update the [changelog](https://example.org/changelog)",
        "- tag the release",
        sep = "\n"
      )
    ), conn = conn, user = "demo")
    insert_record(notes_form, list(
      title = "Meeting note", status = "Done",
      description = "Agreed on the *inline forms* API. See **form_layout**."
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
      "Edit a row and type Markdown in the Description - ", shiny::tags$code("**bold**"),
      ", ", shiny::tags$code("- a list"), ", a ", shiny::tags$code("[link](https://...)"),
      ". The table shows it formatted. Pasting a <script> tag renders as harmless text."
    )
  )
}

#> STEP: Wire the server
#> NOTE: form_server() with the same id renders the records table and runs every
#> NOTE: CRUD action. The markdown = TRUE field is rendered as formatted Markdown
#> NOTE: in the table; the value is HTML-escaped and URL-sanitized first, so
#> NOTE: user-entered HTML is rendered as inert text rather than live markup.
server <- function(input, output, session) {
  form_server(
    id = "notes",
    form = notes_form,
    user = "demo",
    table_columns = c("sft_id", "title", "status", "description"),
    table_options = list(columnDefs = list(list(width = "55%", targets = 3))),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Markdown field"),
  how_to(),
  form_ui(
    id = "notes",
    title = "Notes"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_markdown")
#> DEMO END

shinyApp(ui, server)
