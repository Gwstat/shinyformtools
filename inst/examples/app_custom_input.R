# Teaching shinyformtools a custom input with register_input().
#
# The built-in input types cover the common cases, but any Shiny input function -
# for example one from shinyWidgets - can be plugged in with register_input() and
# then used as an input_type in form_field(), exactly like a built-in. The widget
# is rendered in the add/edit dialogs, its value is stored, restored and shown in
# the records table, and (with an update_fun) it joins dynamic values/choices.
#
# This app registers two shinyWidgets inputs:
#   * knobInput   - a single numeric dial (value_arg = "value", stored in a REAL
#                   column, with updateKnobInput for dynamic values).
#   * pickerInput - a multi-select (value_arg = "selected", multiple = TRUE, so
#                   the chosen values are stored as a JSON array and restored to a
#                   vector, and shown joined in the table).
#
# Requires the 'shinyWidgets' package (a shinyformtools dependency).
#
# Run with: shinyformtools::run_example("app_custom_input")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Register the custom inputs
#> NOTE: register_input() teaches the package an additional input function. Call
#> NOTE: it BEFORE the form_field() that uses the new input_type. knobInput is a
#> NOTE: single numeric dial; pickerInput is multi-valued, so multiple = TRUE
#> NOTE: stores the selection as a JSON array and decodes it back to a vector.
register_input(
  "knobInput",
  fun = shinyWidgets::knobInput,
  value_arg = "value",
  update_fun = shinyWidgets::updateKnobInput
)

register_input(
  "pickerInput",
  fun = shinyWidgets::pickerInput,
  value_arg = "selected",
  multiple = TRUE
)
#> END

#> STEP: Describe the form
#> NOTE: The two registered names are used as input_type just like a built-in.
#> NOTE: A custom input that stores a number sets db_type explicitly (the default
#> NOTE: column type for an unknown input is TEXT); args are passed straight to
#> NOTE: the widget function.
tracks_form <- form(
  form_id = "tracks_custom",
  form_name = "Tracks",
  table_name = "tracks_custom",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "title", label = "Title", mandatory = TRUE, col = 1, pos = 1),
    # A single numeric dial, stored in a REAL column.
    form_field(
      id = "rating", label = "Rating", input_type = "knobInput",
      args = list(min = 0, max = 10, value = 5, displayPrevious = TRUE,
                  width = 90, height = 90),
      db_type = "REAL", col = 2, pos = 1
    ),
    # A multi-select; the chosen genres are stored as a JSON array.
    form_field(
      id = "genres", label = "Genres", input_type = "pickerInput",
      args = list(
        choices = c("Rock", "Jazz", "Folk", "Electronic", "Classical"),
        options = list(`actions-box` = TRUE)
      ),
      col = 1, pos = 2
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(tracks_form, conn = conn, user = "demo")

  if (nrow(fetch_records(tracks_form, conn = conn)) == 0L) {
    insert_record(tracks_form, list(
      title = "Midnight Drive", rating = 8, genres = c("Rock", "Electronic")
    ), conn = conn, user = "demo")
    insert_record(tracks_form, list(
      title = "Slow Sunday", rating = 6, genres = c("Jazz", "Folk")
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
      "Add or edit a track. The ", shiny::tags$strong("Rating"), " dial and the ",
      shiny::tags$strong("Genres"), " multi-select are both shinyWidgets inputs ",
      "registered with ", shiny::tags$code("register_input()"),
      ". The dial value round-trips through a REAL column; the genres are stored ",
      "as a JSON array and shown joined in the table."
    )
  )
}

#> STEP: Wire the server
#> NOTE: Nothing special is needed here: once the inputs are registered, the form
#> NOTE: server renders, validates, stores and restores them like any built-in.
server <- function(input, output, session) {
  form_server(
    id = "tracks",
    form = tracks_form,
    user = "demo",
    show_audit = FALSE,
    table_columns = c("sft_easy_id", "title", "rating", "genres"),
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
  "Custom input (register_input)", "app_custom_input",
  shiny::tagList(
    how_to(),
    form_ui(
      id = "tracks",
      title = "Tracks",
      show_user = FALSE,
      show_audit = FALSE,
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
