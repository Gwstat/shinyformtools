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
#> NOTE: user-entered HTML cannot execute.
server <- function(input, output, session) {
  form_server(
    id = "notes",
    form = notes_form,
    user = "demo",
    show_audit = FALSE,
    table_columns = c("sft_easy_id", "title", "status", "description"),
    table_options = list(columnDefs = list(list(width = "55%", targets = 3))),
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
  "Markdown field", "app_markdown",
  shiny::tagList(
    how_to(),
    form_ui(
      id = "notes",
      title = "Notes",
      show_user = FALSE,
      show_audit = FALSE,
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
