# A fully German UI from one global switch, with per-form overrides.
#
# English is the package default (for CRAN). use_german() flips every default
# user-facing string to German in one call: button labels, dialog titles,
# notifications, the records-table column headers, the Yes/No deleted flag, the
# audit-log action names ("Erstellt" / "Bearbeitet" / "Gel\u00f6scht") and the audit
# table headers. Call it ONCE, at the top of the app, before form_ui() - it sets
# global options, so both the UI and the server pick it up.
#
# Resolution order is:  English default  <-  use_german()  <-  per-form argument.
# So a per-form `labels` / `messages` argument still wins for situational
# wording. Here the second form renames just "open_edit" to "Editieren" while
# everything else stays German.
#
# The config is plain lists you can edit: german_labels(), german_messages(),
# german_table_labels(). use_english() clears the switch again.
#
# Run with: shinyformtools::run_example("app_german")

library(shiny)
library(shinyformtools)

# One switch -> all defaults become German. (Global; call before building the UI.)
use_german()

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form (German labels)
#> NOTE: The form() description is unchanged from the English examples - only the
#> NOTE: field labels and choices are German here. The German UI chrome (buttons,
#> NOTE: headers, notifications) comes entirely from use_german() above, not from
#> NOTE: the form definition.
contacts_form <- form(
  form_id = "kontakte",
  form_name = "Kontakte",
  table_name = "kontakte",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "email", label = "E-Mail", unique = TRUE, col = 1, pos = 2),
    form_field(
      id = "team", label = "Team", input_type = "selectInput",
      args = list(choices = c("Vertrieb", "Support", "Technik"), selected = "Vertrieb"),
      col = 2, pos = 1
    ),
    form_field(
      id = "notizen", label = "Notizen", input_type = "textAreaInput",
      args = list(value = "", rows = 3), col = 1, pos = 3
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
                  team = "Technik", notizen = "Erste Ingenieurin."), conn = conn, user = "demo")
    insert_record(contacts_form, list(name = "Grace Hopper", email = "grace@example.org",
                  team = "Support", notizen = ""), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("What to look for"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(
        shiny::tags$b("One switch: "),
        "every label is German - buttons, the column headers, the deleted flag ",
        "(Ja/Nein) and the audit log (Erstellt / Bearbeitet / Gel\u00f6scht). ",
        "Soft-delete a row to see the German deleted-records dialog."
      ),
      shiny::tags$li(
        shiny::tags$b("Per-form override: "),
        "the button on the second table reads ", shiny::tags$em("Editieren"),
        ", because that form passes labels = list(open_edit = \"Editieren\"); ",
        "everything else stays German."
      )
    )
  )
}

#> STEP: Wire the server (global German + a per-form override)
#> NOTE: Both tables share one form. The first picks up the global German
#> NOTE: defaults from use_german(); the second passes labels = list(open_edit =
#> NOTE: "Editieren") to rename just that one action, showing that a per-form
#> NOTE: argument still wins over the global switch.
server <- function(input, output, session) {
  form_server(
    id = "contacts",
    form = contacts_form,
    user = "demo",
    table_columns = c("sft_easy_id", "name", "email", "team", "sft_is_deleted", "sft_updated_at"),
    persist_column_settings = FALSE
  )

  # Same data, but one situational rename overrides the global German default.
  # ...and to form_server too, so the edit dialog's title matches the button.
  form_server(
    id = "contacts_override",
    form = contacts_form,
    user = "demo",
    show_audit = FALSE,
    labels = list(open_edit = "Editieren"),
    table_columns = c("sft_easy_id", "name", "team"),
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
  "Deutsche Oberfl\u00e4che (use_german)", "app_german",
  shiny::tagList(
    how_to(),
    shiny::h5("Standard (use_german)"),
    form_ui(
      id = "contacts",
      title = "Kontakte",
      show_user = FALSE,
      button_options = neutral_buttons
    ),
    shiny::tags$hr(),
    shiny::h5("Per-form override: open_edit = \"Editieren\""),
    form_ui(
      id = "contacts_override",
      title = "Kontakte",
      show_user = FALSE,
      show_audit = FALSE,
      # The button text is built in the UI, so the override goes here...
      labels = list(open_edit = "Editieren"),
      button_options = neutral_buttons
    )
  )
)

shinyApp(ui, server)
