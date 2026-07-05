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
    show_audit = FALSE,
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
  "Blocking, invisible & conditional inputs", "app_field_control",
  shiny::tagList(
    selectInput("act_as", "Act as", choices = c("admin", "viewer"), selected = "viewer"),
    how_to(),
    form_ui(
      id = "comp",
      title = "Compensation",
      show_user = FALSE,
      show_audit = FALSE,
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
