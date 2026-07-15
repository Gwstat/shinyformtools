# Controlling individual fields: blocking, invisible and conditional inputs.
#
# Four ways to constrain a field, all declared on form_field() (plus one binding):
#
#   1. Per-user blocking - editable = function(user):
#      The "Salary" field is editable only for the admin. Use the "Act as"
#      switch: as "viewer", open Edit and Salary is greyed out; any change you
#      try to make is dropped on save (enforced server-side, not just hidden).
#      As "admin" it is editable.
#
#   2. Static blocking - editable = FALSE:
#      "Grade" can never be typed into. Here it is filled automatically from the
#      chosen Level by a dynamic_value() binding - the common pattern for a
#      derived, read-only field.
#
#   3. Invisible input - show = FALSE:
#      "Cost center" has no input in the form at all. It is still a stored column
#      (here with a database default) and still appears in the records table.
#
#   4. Conditional ("pop-up") input - dynamic_visibility():
#      "Contract end" appears only when Employment type is "Contractor". The same
#      predicate runs again on save, so if the field is hidden its value is
#      dropped - a field that does not apply never stores a stale value.
#
# Run with: shinyformtools::run_example("app_field_control")

library(shiny)
library(shinyformtools)

first_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) "" else as.character(x[[1L]])
}

grade_for_level <- function(level) {
  switch(level, Junior = "A", Senior = "B", Lead = "C", "")
}

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Constrain fields on the form description
#> NOTE: Four controls declared on form_field(): editable = function(user) blocks
#> NOTE: per user, editable = FALSE blocks always, show = FALSE hides the input
#> NOTE: (still stored), and a binding (below) drives conditional visibility.
comp_form <- form(
  form_id = "compensation",
  form_name = "Compensation",
  table_name = "compensation",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "level", label = "Level", input_type = "selectInput",
      args = list(choices = c("Junior", "Senior", "Lead"), selected = "Junior"),
      col = 1, pos = 2
    ),
    # 1. Per-user blocking: editable only resolves TRUE for the admin.
    form_field(
      id = "salary", label = "Salary", input_type = "numericInput",
      args = list(value = 50000, min = 0, step = 1000),
      editable = function(user) identical(user, "admin"),
      col = 2, pos = 1
    ),
    # 2. Static blocking: never editable; filled from Level by a binding below.
    form_field(id = "grade", label = "Grade (derived)", editable = FALSE, col = 2, pos = 2),
    # 4. Conditional input: shown only for contractors (see the binding below).
    form_field(
      id = "employment_type", label = "Employment type", input_type = "selectInput",
      args = list(choices = c("Employee", "Contractor"), selected = "Employee"),
      col = 1, pos = 3
    ),
    form_field(
      id = "contract_end", label = "Contract end", input_type = "dateInput",
      args = list(value = Sys.Date()), col = 2, pos = 3
    ),
    # 3. Invisible input: no form input, but a stored column with a default.
    form_field(id = "cost_center", label = "Cost center", show = FALSE,
               db_default = "CC-000")
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(comp_form, conn = conn, user = "admin")

  if (nrow(fetch_records(comp_form, conn = conn)) == 0L) {
    insert_record(comp_form, list(name = "Ada Lovelace", level = "Lead",
                  salary = 120000, grade = "C", employment_type = "Employee",
                  cost_center = "CC-014"), conn = conn, user = "admin")
    insert_record(comp_form, list(name = "Grace Hopper", level = "Senior",
                  salary = 95000, grade = "B", employment_type = "Contractor",
                  contract_end = "2026-12-31", cost_center = "CC-009"),
                  conn = conn, user = "admin")
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
      shiny::tags$li(shiny::tags$b("Per-user: "),
        "Set ", shiny::tags$em("Act as"), " to viewer, then Edit a row - Salary ",
        "is greyed out and changes to it are ignored on save. As admin it edits."),
      shiny::tags$li(shiny::tags$b("Static: "),
        "Grade is never editable; it follows the chosen Level automatically."),
      shiny::tags$li(shiny::tags$b("Invisible: "),
        "Cost center has no input, but it is stored and shown in the table."),
      shiny::tags$li(shiny::tags$b("Conditional: "),
        "Set ", shiny::tags$em("Employment type"), " to Contractor and a ",
        shiny::tags$em("Contract end"), " field pops up; switch back to Employee ",
        "and it hides - and is cleared on save.")
    )
  )
}

#> STEP: Wire the server
#> NOTE: user = function() input$act_as drives the per-user editable() on Salary;
#> NOTE: dynamic_value fills the read-only Grade, dynamic_visibility shows
#> NOTE: Contract end only for contractors (and clears it on save when hidden).
server <- function(input, output, session) {
  form_server(
    id = "comp",
    form = comp_form,
    # The current user drives the per-user `editable` function on Salary.
    user = function() input$act_as,
    table_columns = c("sft_easy_id", "name", "level", "salary", "grade",
                      "employment_type", "contract_end", "cost_center"),
    persist_column_settings = FALSE,
    input_bindings = list(
      dynamic_value(
        field = "grade",
        depends_on = "level",
        value = function(values) grade_for_level(first_or_empty(values$level))
      ),
      # 4. Conditional visibility: Contract end shows only for contractors and is
      # cleared on save when hidden.
      dynamic_visibility(
        field = "contract_end",
        depends_on = "employment_type",
        visible = function(values) identical(values$employment_type, "Contractor")
      )
    )
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Blocking, invisible & conditional inputs"),
  selectInput("act_as", "Act as", choices = c("admin", "viewer"), selected = "viewer"),
  how_to(),
  form_ui(
    id = "comp",
    title = "Compensation"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_field_control")
#> DEMO END

shinyApp(ui, server)
