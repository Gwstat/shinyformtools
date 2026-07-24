# Table design presets: switch the module's visual style live.
#
# form_ui(table_style = ...) picks a cosmetic preset for every table the module
# renders (records, audit log, deleted records, versions):
#
#   - "classic" - the unmodified DT look (the default).
#   - "clean"   - a card-style, reactable-like look: rounded border, uppercase
#                 gray headers, row separators instead of stripes.
#   - "publication" - a booktabs-style journal table: serif type, horizontal
#                 rules only, no stripes.
#   - "compact" - dense rows under a dark header.
#
# The preset is pure CSS, scoped to this module's tables - host-app tables are
# untouched. Instead of the argument, an app can also set the look once with
# options(shinyformtools.table_style = "clean") and override it per form.
#
# Pick a preset on the left and the tables restyle immediately. The audit log is
# shown so you can see that every module table follows the preset, not just the
# records table.
#
# Run with: shinyformtools::run_example("app_design_presets")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: An ordinary form - the design preset is independent of the form
#> NOTE: definition. A handful of columns and rows make the styling visible.
tasks_form <- form(
  form_id = "tasks_designs",
  form_name = "Tasks",
  table_name = "tasks_designs",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "title", label = "Task", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "status", label = "Status", input_type = "selectInput",
      args = list(choices = c("Open", "In progress", "Done"), selected = "Open"),
      col = 1, pos = 2
    ),
    form_field(id = "owner", label = "Owner", col = 2, pos = 1),
    form_field(
      id = "due", label = "Due", input_type = "dateInput",
      args = list(value = Sys.Date() + 7), col = 2, pos = 2
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(tasks_form, conn = conn, user = "demo")

  if (nrow(fetch_records(tasks_form, conn = conn)) == 0L) {
    seed <- list(
      list(title = "Collect requirements", status = "Done", owner = "Ada"),
      list(title = "Design the schema", status = "Done", owner = "Grace"),
      list(title = "Build the form", status = "In progress", owner = "Ada"),
      list(title = "Review permissions", status = "In progress", owner = "Linus"),
      list(title = "Write the handbook", status = "Open", owner = "Grace"),
      list(title = "Plan the rollout", status = "Open", owner = "Linus")
    )
    for (i in seq_along(seed)) {
      values <- seed[[i]]
      values$due <- as.character(Sys.Date() + 3 * i)
      insert_record(tasks_form, values, conn = conn, user = "demo")
    }
  }
})

#> STEP: Wire the server
#> NOTE: form_server() runs once; only the UI is re-rendered when a different
#> NOTE: preset is picked, because table_style is a form_ui() argument. A real
#> NOTE: app simply passes table_style = "clean" (or sets the global option)
#> NOTE: and skips the renderUI.
server <- function(input, output, session) {
  form_server(
    id = "tasks",
    form = tasks_form,
    user = "demo",
    show_audit = TRUE,
    table_columns = c("sft_id", "title", "status", "owner", "due"),
    persist_column_settings = FALSE
  )

  output$form_area <- renderUI({
    form_ui(
      id = "tasks",
      title = "Tasks",
      show_audit = TRUE,
      show_deleted_records = TRUE,
      table_style = input$table_style
    )
  })
}
#> END

#> STEP: Build the UI
#> NOTE: The radio buttons drive the preset; everything below them is the
#> NOTE: ordinary form module UI.
ui <- fluidPage(
  titlePanel("Table design presets"),
  radioButtons(
    inputId = "table_style",
    label = "Table style (form_ui argument table_style)",
    choices = c(
      "classic (default)" = "classic",
      "clean" = "clean",
      "publication" = "publication",
      "compact" = "compact"
    ),
    selected = "clean",
    inline = TRUE
  ),
  uiOutput("form_area")
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_design_presets")
#> DEMO END

shinyApp(ui, server)
