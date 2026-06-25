# Reactively highlighting fields (and their tab), plus an automatic "changed" glow.
#
# Two independent highlight channels, both driven from form_server():
#
#   1. Caller-driven red glow - highlight_fields (+ highlight_tab):
#      Tick fields in the "Flag for attention" box. The chosen inputs glow red in
#      the add AND edit forms, and (highlight_tab = TRUE) the tab that holds a
#      flagged field glows too - handy when the field lives on a tab you are not
#      looking at. highlight_fields is a reactive, so the glow tracks the box
#      live; clearing the box clears the glow.
#
#   2. Automatic blue "changed" glow - show_changed (on by default):
#      Open Edit on a row and change a value: that field glows blue as soon as it
#      differs from the stored value, and stops glowing if you set it back. The
#      comparison is type-tolerant (5 vs "5", TRUE vs 1, a date vs its string do
#      not false-positive), so only real edits light up.
#
# The form is laid out over two tabs (form_field(tab = ...)) so the tab glow has
# something to point at. Colours are overridable via highlight_color /
# changed_color.
#
# Run with: shinyformtools::run_example("app_highlight")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Lay the form out over two tabs
#> NOTE: form_field(tab = 0/1) splits the inputs across two tabs. With the tab
#> NOTE: glow on, flagging a field on a tab you are not viewing still draws the
#> NOTE: eye to the right tab.
profile_form <- form(
  form_id = "profiles",
  form_name = "Profiles",
  table_name = "profiles",
  db = db_sqlite(db_path),
  fields = list(
    # Tab 1 - Personal
    form_field(id = "name", label = "Name", mandatory = TRUE, tab = 0, pos = 1),
    form_field(id = "email", label = "Email", tab = 0, pos = 2),
    form_field(
      id = "age", label = "Age", input_type = "numericInput",
      args = list(value = 30, min = 0), tab = 0, pos = 3
    ),
    # Tab 2 - Address
    form_field(id = "city", label = "City", tab = 1, pos = 1),
    form_field(id = "postcode", label = "Postcode", tab = 1, pos = 2),
    form_field(
      id = "country", label = "Country", input_type = "selectInput",
      args = list(choices = c("NL", "DE", "FR", "UK"), selected = "NL"),
      tab = 1, pos = 3
    )
  ),
  tab_labels = c("Personal", "Address")
)
#> END

# Field ids/labels offered in the "flag" picker below.
flag_choices <- c(
  Name = "name", Email = "email", Age = "age",
  City = "city", Postcode = "postcode", Country = "country"
)

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(profile_form, conn = conn, user = "demo")

  if (nrow(fetch_records(profile_form, conn = conn)) == 0L) {
    insert_record(profile_form, list(name = "Ada Lovelace", email = "ada@example.org",
                  age = 36, city = "London", postcode = "EC1", country = "UK"),
                  conn = conn, user = "demo")
    insert_record(profile_form, list(name = "Guido van Rossum", email = "guido@example.org",
                  age = 70, city = "Haarlem", postcode = "2011", country = "NL"),
                  conn = conn, user = "demo")
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
      shiny::tags$li(shiny::tags$b("Red flag: "),
        "Tick some fields in ", shiny::tags$em("Flag for attention"), ", then ",
        "open Add or Edit - the chosen inputs glow red, and the tab holding a ",
        "flagged field glows too. Untick to clear."),
      shiny::tags$li(shiny::tags$b("Blue changed: "),
        "Open Edit on a row and change a value - it glows blue once it differs ",
        "from the stored value, and stops if you set it back.")
    )
  )
}

#> STEP: Wire the highlight arguments on the server
#> NOTE: highlight_fields takes a reactive, so the red glow tracks the checkbox
#> NOTE: group live; highlight_tab also glows the owning tab. show_changed (on by
#> NOTE: default) adds the automatic blue glow on edited fields. Colours are
#> NOTE: overridable via highlight_color / changed_color.
server <- function(input, output, session) {
  form_server(
    id = "profile",
    form = profile_form,
    user = "demo",
    show_audit = FALSE,
    persist_column_settings = FALSE,
    table_columns = c("sft_easy_id", "name", "email", "age",
                      "city", "postcode", "country"),
    # 1. Reactive red glow on the flagged fields, plus their tab.
    highlight_fields = reactive(input$flag),
    highlight_tab = TRUE,
    # 2. Automatic blue glow when an edit-form value differs from the stored one.
    show_changed = TRUE
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
  "Reactive field & tab highlighting", "app_highlight",
  shiny::tagList(
    shiny::checkboxGroupInput(
      "flag", "Flag for attention (red glow)",
      choices = flag_choices, inline = TRUE
    ),
    how_to(),
    form_ui(
      id = "profile",
      title = "Profiles",
      show_user = FALSE,
      show_audit = FALSE,
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
