# A questionnaire / survey: one question (or a pair) per slide, always visible.
#
# Two things make a survey different from the CRUD examples:
#
#   1. Slides. Each field carries a `slide` index (form_field(slide = N)); the
#      package renders the slides as a shinyglide wizard with Back / Next
#      controls. Fields that share a slide are arranged by `col` / `pos`, so a
#      slide can hold two inputs (e.g. "what did you like" + "what to improve").
#
#   2. No "Add" step, no records table. A respondent should just see the
#      questions, so instead of the records-management module we render the form
#      directly with render_form_fields() - it is simply on the page, all the
#      time. On Submit, collect_input_values() reads the answers and
#      insert_record() stores the response (validation and the audit log still
#      apply); shinyjs::reset() clears the form for the next person.
#
# Requires the optional 'shinyglide' package (for the slides).
#
# Run with: shinyformtools::run_example("app_questionnaire")

library(shiny)
library(shinyformtools)

if (!requireNamespace("shinyglide", quietly = TRUE)) {
  stop(
    "The 'app_questionnaire' example needs the 'shinyglide' package. ",
    "install.packages('shinyglide').",
    call. = FALSE
  )
}

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the questionnaire as a slide form
#> NOTE: Each question lives on its own slide via form_field(slide = N), and
#> NOTE: shinyglide turns the slides into a Back/Next wizard. Fields that share a
#> NOTE: slide are placed by col/pos, so slides 2 and 3 below carry two inputs.
#> NOTE: slide_labels names each step.
survey_form <- form(
  form_id = "survey",
  form_name = "Reader survey",
  table_name = "survey",
  db = db_sqlite(db_path),
  slide_labels = c("About you", "Overall", "What stood out", "Spread the word"),
  fields = list(
    # Slide 0 - two inputs.
    form_field(id = "name", label = "Your name (optional)", slide = 0, col = 1, pos = 1),
    form_field(
      id = "role", label = "Your role", input_type = "selectInput",
      args = list(choices = c("Student", "Analyst", "Developer", "Manager", "Other"),
                  selected = "Analyst"),
      slide = 0, col = 1, pos = 2
    ),
    # Slide 1 - one input.
    form_field(
      id = "satisfaction", label = "How satisfied are you overall?",
      input_type = "sliderInput", args = list(min = 1, max = 10, value = 7),
      mandatory = TRUE, slide = 1, col = 1, pos = 1
    ),
    # Slide 2 - two inputs.
    form_field(
      id = "liked", label = "What did you like most?", input_type = "textAreaInput",
      args = list(rows = 3, value = ""), slide = 2, col = 1, pos = 1
    ),
    form_field(
      id = "improve", label = "What should we improve?", input_type = "textAreaInput",
      args = list(rows = 3, value = ""), slide = 2, col = 1, pos = 2
    ),
    # Slide 3 - two inputs.
    form_field(
      id = "recommend", label = "Would you recommend us?", input_type = "radioButtons",
      args = list(choices = c("Yes", "Maybe", "No"), selected = "Yes", inline = TRUE),
      slide = 3, col = 1, pos = 1
    ),
    form_field(id = "email", label = "Email (optional, for follow-up)",
               slide = 3, col = 1, pos = 2)
  )
)
#> END

# Create the table up front so the first response has somewhere to go.
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(survey_form, conn = conn, user = "respondent")
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Take the survey"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "The questions are right here - no Add button. Step through the slides with ",
      shiny::tags$em("Back / Next"), ", then ", shiny::tags$em("Submit response"),
      ". The form clears itself for the next respondent."
    )
  )
}

#> STEP: Render the form directly, so it is always visible
#> NOTE: render_form_fields() turns the form description into the input controls
#> NOTE: (here a shinyglide wizard, because the fields use slides). We wrap it in
#> NOTE: a div we can reset, and add our own Submit button - no add/edit module,
#> NOTE: no records table, the form is simply on the page.
survey_panel <- function() {
  shiny::tagList(
    shiny::div(
      id = "survey_fields",
      render_form_fields(survey_form, prefix = "q_")
    ),
    shiny::actionButton("submit", "Submit response", class = "btn-primary"),
    shiny::div(
      style = "margin-top: 0.6rem; color: #586069; font-size: 0.85rem;",
      shiny::textOutput("count", inline = TRUE)
    )
  )
}
#> END

#> STEP: Store each response on Submit
#> NOTE: collect_input_values() reads the answers (matched by the "q_" prefix),
#> NOTE: insert_record() writes the response to the database (it validates the
#> NOTE: mandatory rating and throws on a problem, which we surface), and
#> NOTE: shinyjs::reset() clears the inputs for the next person.
server <- function(input, output, session) {
  responses <- shiny::reactiveVal(0L)
  output$count <- shiny::renderText({
    sprintf("%d response(s) recorded this session.", responses())
  })

  shiny::observeEvent(input$submit, {
    values <- collect_input_values(survey_form, input, prefix = "q_")

    ok <- tryCatch({
      conn <- db_connect(survey_form$db)
      on.exit(db_disconnect(conn), add = TRUE)
      insert_record(survey_form, values, conn = conn, user = "respondent")
      TRUE
    }, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      FALSE
    })

    if (isTRUE(ok)) {
      responses(responses() + 1L)
      shiny::showNotification("Thanks for your response!", type = "message")
      shinyjs::reset("survey_fields")
    }
  })
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
  "Questionnaire (slides)", "app_questionnaire",
  shiny::tagList(
    shinyjs::useShinyjs(),
    how_to(),
    survey_panel()
  )
)

shinyApp(ui, server)
