# Add/edit forms rendered inline instead of in a modal dialog.
#
# Pass form_layout = "inline" to BOTH form_ui() and form_server(): the add and
# edit forms then open in a panel above the records table instead of a pop-up
# dialog. Add and edit are mutually exclusive - opening one closes the other.
# Everything else (validation, soft-delete/restore, the audit log, permissions)
# behaves exactly as in the default modal layout.
#
#   - Click "Add" - the form appears above the table. Save or Cancel closes it.
#   - Select a row and "Edit" - the same panel switches to that record.
#
# Run with: shinyformtools::run_example("app_inline_forms")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: A plain form() description - nothing here is specific to inline forms.
#> NOTE: The same description drives the schema, the add/edit forms, the records
#> NOTE: table, soft-delete and the audit log. Inline vs modal is purely a
#> NOTE: rendering choice made later in form_ui()/form_server().
tasks_form <- form(
  form_id = "tasks_inline",
  form_name = "Tasks",
  table_name = "tasks_inline",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "title", label = "Title", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "status", label = "Status", input_type = "selectInput",
      args = list(choices = c("Open", "In progress", "Done"), selected = "Open"),
      col = 1, pos = 2
    ),
    form_field(
      id = "priority", label = "Priority", input_type = "selectInput",
      args = list(choices = c("Low", "Medium", "High"), selected = "Medium"),
      col = 2, pos = 1
    ),
    form_field(
      id = "details", label = "Details", input_type = "textAreaInput",
      args = list(value = "", rows = 3), col = 2, pos = 2
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
    insert_record(tasks_form, list(title = "Draft the release notes", status = "Open",
                  priority = "High", details = ""), conn = conn, user = "demo")
    insert_record(tasks_form, list(title = "Review pull request", status = "In progress",
                  priority = "Medium", details = ""), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Note"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Add and Edit open a panel ", shiny::tags$em("above the table"),
      " instead of a modal. The only change from a normal app is ",
      shiny::tags$code('form_layout = "inline"'), " on both ", shiny::tags$code("form_ui()"),
      " and ", shiny::tags$code("form_server()"), "."
    )
  )
}

#> STEP: Wire the server inline
#> NOTE: The only thing that makes the add/edit forms inline is
#> NOTE: form_layout = "inline" on form_server() (and the matching argument on
#> NOTE: form_ui()). The add and edit forms then open in a panel above the
#> NOTE: records table instead of a modal dialog; opening one closes the other.
#> NOTE: Everything else (validation, soft-delete/restore, the audit log) is
#> NOTE: unchanged.
server <- function(input, output, session) {
  form_server(
    id = "tasks",
    form = tasks_form,
    user = "demo",
    form_layout = "inline",
    table_columns = c("sft_easy_id", "title", "status", "priority", "sft_updated_at"),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
#> NOTE: form_layout = "inline" here matches the argument passed to
#> NOTE: form_server() above - both sides must agree for the inline panel to work.
ui <- fluidPage(
  titlePanel("Inline add/edit forms"),
  how_to(),
  form_ui(
    id = "tasks",
    title = "Tasks",
    form_layout = "inline"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_inline_forms")
#> DEMO END

shinyApp(ui, server)
