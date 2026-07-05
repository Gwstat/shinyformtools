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
      show_deleted_records = FALSE, show_column_settings = FALSE,
      show_column_selection = FALSE,
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
    show_audit = FALSE,
    table_columns = c("sft_easy_id", "summary", "severity", "area", "sft_updated_at"),
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
  "Bug-report button in the app header", "app_bug_report",
  shiny::tagList(
    app_header(),
    shiny::p(
      style = "color:#586069;",
      "The application's content would go here. Submitted reports (admin view):"
    ),
    form_ui(
      id = "bugs",
      title = NULL,
      show_user = FALSE,
      show_audit = FALSE,
      button_options = list(placement = "none")
    )
  )
)

shinyApp(ui, server)
