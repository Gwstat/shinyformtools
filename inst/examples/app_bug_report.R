# A "Report a bug" button in an application's header.
#
# The pattern: an app has its own chrome (here a top bar), and you want a bug /
# feedback form reachable from a button in that header - not from a records
# toolbar. form_buttons() renders the form module's action buttons anywhere you
# like, using the same module id, so a button in the header opens the very same
# add dialog that form_server() drives. Hide the module's own button row with
# form_ui(button_options = list(placement = "none")).
#
#   - The "Report a bug" button sits in the app header, visible all the time.
#   - Clicking it opens the report form (a modal here).
#   - Submitted reports land in the table below (shown as an "admin" view so you
#     can watch your report arrive; a real app would hide it from end users with
#     form_server(can_view_table = FALSE)).
#
# Run with: shinyformtools::run_example("app_bug_report")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the bug-report form
#> NOTE: An ordinary form() - a few fields capturing a report. Nothing here is
#> NOTE: aware that its button will live in the app header.
bugs_form <- form(
  form_id = "bugs",
  form_name = "Bug reports",
  table_name = "bug_reports",
  db = db_sqlite(db_path),
  # A ticket id gets read out and pasted into chat, so this form opts into
  # sft_easy_id ("3-QZ") instead of the bare sft_id every form has. It is the
  # only difference: easy_id is cosmetic, not a stronger identifier.
  easy_id = TRUE,
  fields = list(
    form_field(id = "summary", label = "Summary", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "severity", label = "Severity", input_type = "selectInput",
      args = list(choices = c("Low", "Medium", "High", "Critical"), selected = "Medium"),
      col = 1, pos = 2
    ),
    form_field(
      id = "area", label = "Where did it happen?", input_type = "selectInput",
      args = list(choices = c("Dashboard", "Upload", "Reports", "Settings", "Other")),
      col = 2, pos = 1
    ),
    form_field(
      id = "steps", label = "Steps to reproduce", input_type = "textAreaInput",
      args = list(rows = 4, value = ""), col = 1, pos = 3
    ),
    form_field(id = "email", label = "Email (optional)", col = 2, pos = 2)
  )
)
#> END

# Seed one report so the table is not empty.
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(bugs_form, conn = conn, user = "demo")
  if (nrow(fetch_records(bugs_form, conn = conn)) == 0L) {
    insert_record(
      bugs_form,
      list(summary = "Export button does nothing on Safari", severity = "High",
           area = "Reports", steps = "Open Reports, click Export, nothing downloads.",
           email = "ada@example.org"),
      conn = conn, user = "demo"
    )
  }
})

#> STEP: Put the report button in the app header
#> NOTE: form_buttons("bugs", ...) renders the module's "Add" button using the
#> NOTE: same id, so it triggers the matching form_server(). We keep only that
#> NOTE: button, relabel it "Report a bug", and place it in the app's top bar.
app_header <- function() {
  shiny::div(
    style = paste(
      "display:flex; align-items:center; justify-content:space-between;",
      "padding:0.6rem 1rem; background:#1f2d3d; color:#fff; border-radius:6px;",
      "margin-bottom:1rem;"
    ),
    shiny::tags$strong("AcmeDash"),
    form_buttons(
      "bugs",
      show_edit = FALSE, show_delete = FALSE, show_refresh_table = FALSE,
      labels = list(open_add = "Report a bug"),
      button_options = list(button_classes = list(open_add = "btn-danger btn-sm"))
    )
  )
}
#> END

#> STEP: Wire the server
#> NOTE: A plain form_server(); it drives the modal opened by the header button
#> NOTE: and the reports table below. form_ui() hides its own button row with
#> NOTE: button_options = list(placement = "none").
server <- function(input, output, session) {
  form_server(
    id = "bugs",
    form = bugs_form,
    user = "demo",
    table_columns = c("sft_easy_id", "summary", "severity", "area", "sft_updated_at"),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
#> NOTE: form_ui() here shows only the records table (no title, no audit, no
#> NOTE: button row) since the only way in is the "Report a bug" button above.
ui <- fluidPage(
  titlePanel("Bug-report button in the app header"),
  app_header(),
  shiny::p(
    style = "color:#586069;",
    "The application's content would go here. Submitted reports (admin view):"
  ),
  form_ui(
    id = "bugs",
    title = NULL,
    button_options = list(placement = "none")
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_bug_report")
#> DEMO END

shinyApp(ui, server)
