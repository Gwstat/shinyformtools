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
    table_columns = c("sft_easy_id", "name", "email", "team", "gender", "active", "sft_updated_at"),
    persist_column_settings = FALSE
  )
}
#> END

# --- "How it is built" demo scaffolding (not part of the form API) -----------
# Self-contained: renders this file's own #> STEP / #> NOTE / #> END blocks as
# numbered cards beside the running app. Only the form()/form_server() code
# above is shinyformtools; everything in this block just draws the demo page.
neutral_buttons <- list(
  button_classes = list(
    open_add = "btn-default", open_edit = "btn-default", delete = "btn-default",
    open_deleted_records = "btn-default", open_column_selection = "btn-default"
  )
)

demo_steps <- function(path) {
  lines <- readLines(path, warn = FALSE)
  out <- list()
  cur <- NULL
  push <- function() {
    if (is.null(cur)) return(invisible())
    code <- cur$code
    while (length(code) && !nzchar(trimws(code[1]))) code <- code[-1]
    while (length(code) && !nzchar(trimws(code[length(code)]))) code <- code[-length(code)]
    cur$code <- code
    out[[length(out) + 1L]] <<- cur
    cur <<- NULL
  }
  for (ln in lines) {
    title <- sub("^#>\\s*STEP:\\s*", "", ln)
    if (!identical(title, ln)) {
      push()
      cur <- list(title = trimws(title), notes = character(), code = character())
      next
    }
    if (grepl("^#>\\s*END\\s*$", ln)) {
      push()
      next
    }
    note <- sub("^#>\\s*NOTE:\\s*", "", ln)
    if (!identical(note, ln) && !is.null(cur)) {
      cur$notes <- c(cur$notes, trimws(note))
      next
    }
    if (!is.null(cur)) cur$code <- c(cur$code, ln)
  }
  push()
  out
}

# Resolve the file actually being run (dev vs installed can diverge), so the
# walkthrough always reflects THIS source. Works under source(), Shiny
# sourceUTF8 (parse+eval) and Rscript; falls back to the bundled copy by name.
demo_self_path <- function() {
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) {
      return(normalizePath(of, winslash = "/", mustWork = FALSE))
    }
  }
  for (i in seq_len(sys.nframe())) {
    sr <- attr(sys.call(i), "srcref")
    sf <- if (!is.null(sr)) attr(sr, "srcfile") else NULL
    if (!is.null(sf) && !is.null(sf$filename) && nzchar(sf$filename)) {
      return(normalizePath(sf$filename, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

how_built <- function(example) {
  path <- demo_self_path()
  if (is.null(path) || !file.exists(path)) path <- example_path(example)
  steps <- tryCatch(demo_steps(path), error = function(e) list())
  src <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  libs <- src[startsWith(src, "library(")]
  launch <- src[startsWith(src, "shinyApp(") | startsWith(src, "shiny::shinyApp(")]
  if (length(libs)) {
    steps <- c(list(list(title = "Load the libraries", notes = character(), code = libs)), steps)
  }
  if (length(launch)) {
    steps <- c(steps, list(list(title = "Run the app", notes = character(), code = launch)))
  }
  shiny::tagList(lapply(seq_along(steps), function(i) {
    st <- steps[[i]]
    shiny::div(
      style = "margin-bottom:1rem;border:1px solid #e1e4e8;border-radius:6px;overflow:hidden;background:#fff;",
      shiny::div(
        style = "padding:0.5rem 0.75rem;background:#f1f5fb;border-bottom:1px solid #e1e4e8;",
        shiny::tags$strong(paste0(i, ". ", st$title))
      ),
      if (length(st$notes)) {
        shiny::tags$p(
          style = "margin:0;padding:0.5rem 0.75rem 0;color:#586069;font-size:0.85rem;",
          paste(st$notes, collapse = " ")
        )
      },
      shiny::tags$pre(
        style = "margin:0.5rem 0 0;padding:0.6rem 0.75rem;background:#f7f7f7;font-size:0.8rem;line-height:1.35;overflow:auto;",
        paste(st$code, collapse = "\n")
      )
    )
  }))
}

demo_page <- function(title, example, app_ui) {
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::fluidRow(
      shiny::column(
        6, shiny::h4("How it is built"),
        shiny::div(style = "height:80vh;overflow:auto;padding-right:0.4rem;", how_built(example))
      ),
      shiny::column(6, shiny::h4("App"), app_ui)
    )
  )
}

ui <- demo_page(
  "Basic CRUD", "app_crud_basic",
  shiny::tagList(
    how_to(),
    form_ui(
      id = "contacts",
      title = "Contacts",
      show_user = FALSE,
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
