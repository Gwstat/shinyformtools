# The smallest complete shinyformtools app: one table, full CRUD + validation.
#
# Start here. A single "Contacts" form drives everything - the database schema,
# the add/edit/delete dialogs, the records table, soft-delete with restore, and
# the audit log - all derived from the form() description below. Nothing else is
# wired by hand.
#
#   - Add a contact (several input types: text, select, numeric, date, checkbox,
#     multi-line text).
#   - Select a row and Edit or Delete it. Deletes are soft: open "Deleted
#     records" to restore one, or open a record to pick an older version.
#   - Every change is recorded in the audit log shown below the table.
#
# Two server-side validations are shown (they run on insert AND update, so they
# cannot be bypassed from the client):
#
#   - Unique email: form_field(unique = TRUE) rejects a second live record with
#     the same email at the database level (a value freed by soft-delete can be
#     reused).
#   - Conditional required: a validation_rules entry built with required_if()
#     makes Notes mandatory only when Gender is "Other" - so the field is
#     required exactly when it needs to be.
#
# Run with: shinyformtools::run_example("app_crud_basic")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: One form() description derives the schema, the add/edit dialogs, the
#> NOTE: records table, soft-delete and the audit log. unique = TRUE on email is
#> NOTE: enforced at the database level; validation_rules adds a conditional
#> NOTE: required field.
contacts_form <- form(
  form_id = "contacts_basic",
  form_name = "Contacts",
  table_name = "contacts_basic",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "email", label = "Email", unique = TRUE, col = 1, pos = 2),
    form_field(
      id = "team", label = "Team", input_type = "selectInput",
      args = list(choices = c("Sales", "Support", "Engineering"), selected = "Sales"),
      col = 1, pos = 3
    ),
    form_field(
      id = "notes", label = "Notes", input_type = "textAreaInput",
      args = list(value = "", rows = 3), col = 1, pos = 4
    ),
    form_field(
      id = "headcount", label = "Reports", input_type = "numericInput",
      args = list(value = 0, min = 0, max = 99), col = 2, pos = 1
    ),
    form_field(
      id = "start_date", label = "Start date", input_type = "dateInput",
      args = list(value = Sys.Date()), col = 2, pos = 2
    ),
    form_field(
      id = "active", label = "Active", input_type = "checkboxInput",
      args = list(value = TRUE), col = 2, pos = 3
    ),
    form_field(
      id = "gender", label = "Gender", input_type = "selectInput",
      args = list(
        choices = c("Prefer not to say", "Female", "Male", "Other"),
        selected = "Prefer not to say"
      ),
      col = 2, pos = 4
    )
  ),
  # Conditional required field: Notes is mandatory only when Gender is "Other".
  validation_rules = list(
    required_if(
      id = "explain_other_gender",
      condition = function(values) identical(values$gender, "Other"),
      fields = "notes",
      message = "When Gender is 'Other', please use Notes to self-describe."
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
                  team = "Engineering", headcount = 3, active = TRUE, gender = "Female",
                  notes = "Founding engineer."), conn = conn, user = "demo")
    insert_record(contacts_form, list(name = "Grace Hopper", email = "grace@example.org",
                  team = "Support", headcount = 5, active = TRUE, gender = "Female",
                  notes = ""), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Two validations to try"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(
        shiny::tags$b("Unique email: "),
        "add a contact whose ", shiny::tags$em("Email"),
        " matches an existing one - the save is rejected."
      ),
      shiny::tags$li(
        shiny::tags$b("Conditional required: "),
        "set ", shiny::tags$em("Gender"), " to ", shiny::tags$em("Other"),
        " and leave ", shiny::tags$em("Notes"), " empty - the save is blocked ",
        "until you fill Notes in. Any other Gender leaves Notes optional."
      )
    )
  )
}

#> STEP: Wire the server
#> NOTE: form_server() with the same id renders the table and runs every CRUD
#> NOTE: action. The two validations run on insert AND update, server-side.
server <- function(input, output, session) {
  form_server(
    id = "contacts",
    form = contacts_form,
    user = "demo",
    show_audit = TRUE,
    table_columns = c("sft_id", "name", "email", "team", "gender", "active", "sft_updated_at"),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
#> NOTE: form_ui() with the same id draws the action buttons and the records
#> NOTE: table. Only the CRUD core is on by default, so this example opts into
#> NOTE: the two extras it wants to show: the audit log (show_audit, on both
#> NOTE: form_ui and form_server) and the deleted-records dialog you restore
#> NOTE: from.
ui <- fluidPage(
  titlePanel("Basic CRUD"),
  how_to(),
  form_ui(
    id = "contacts",
    title = "Contacts",
    show_audit = TRUE,
    show_deleted_records = TRUE
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_crud_basic")
#> DEMO END

shinyApp(ui, server)
