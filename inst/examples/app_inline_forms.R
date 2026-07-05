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
    show_audit = FALSE,
    form_layout = "inline",
    table_columns = c("sft_easy_id", "title", "status", "priority", "sft_updated_at"),
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
  "Inline add/edit forms", "app_inline_forms",
  shiny::tagList(
    how_to(),
    form_ui(
      id = "tasks",
      title = "Tasks",
      show_user = FALSE,
      show_audit = FALSE,
      form_layout = "inline",
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
