# Calculated table columns: derived within a row AND derived from another table.
#
# display_transform() adds columns to the records table at render time. They
# exist only in the table - never in the database - and recompute every time the
# data changes. Two flavours are shown side by side:
#
#   People table (within-row):
#     - "Full name" = first + last name
#     - "Age"       = whole years between birthday and today
#
#   Locations table (cross-table / table-extending):
#     A location stores only a *reference* to a person (their generated
#     sft_easy_id, picked with reference_choices()). The table shows that
#     person's name and city by looking them up in the People records at render
#     time - the columns live in neither table's schema. Editing a person
#     updates the Locations table live, because the Locations form depends on the
#     People form's `changed` signal (refresh_triggers).
#
# display_transform() receives the fetched records and returns a data frame with
# the extra columns added (it must keep sft_id so row selections still map back
# to the underlying record). The derived columns are referenced in table_columns
# and named with display_column_labels.
#
# Run with: shinyformtools::run_example("app_calculated_columns")

library(shiny)
library(shinyformtools)

#> STEP: Helpers for the derived columns
#> NOTE: Plain functions; they run inside display_transform() at render time, so
#> NOTE: their results live only in the table, never in the database.
age_in_years <- function(birthday) {
  bday <- suppressWarnings(as.Date(birthday))
  ifelse(
    is.na(bday),
    NA_integer_,
    as.integer(floor(as.numeric(Sys.Date() - bday) / 365.25))
  )
}

full_name_of <- function(first, last) trimws(paste(first, last))
#> END

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the two forms
#> NOTE: People stores first/last name, birthday and city. A Location stores only
#> NOTE: a reference (contact_id) to a person's generated sft_easy_id.
people_form <- form(
  form_id = "people_calc",
  form_name = "People",
  table_name = "people_calc",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "first_name", label = "First name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "last_name", label = "Last name", mandatory = TRUE, col = 1, pos = 2),
    form_field(
      id = "birthday", label = "Birthday", input_type = "dateInput",
      args = list(value = "1990-01-01"), col = 2, pos = 1
    ),
    form_field(id = "city", label = "City", col = 2, pos = 2)
  )
)

locations_form <- form(
  form_id = "locations_calc",
  form_name = "Locations",
  table_name = "locations_calc",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "location_name", label = "Location name", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "contact_id", label = "Contact (from People)", input_type = "selectizeInput",
      args = list(
        choices = character(),
        options = list(placeholder = "Pick a person")
      ),
      col = 2, pos = 1
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(people_form, conn = conn, user = "demo")
  init_db(locations_form, conn = conn, user = "demo")

  if (nrow(fetch_records(people_form, conn = conn)) == 0L) {
    insert_record(people_form, list(first_name = "Ada", last_name = "Lovelace",
                  birthday = "1815-12-10", city = "London"), conn = conn, user = "demo")
    insert_record(people_form, list(first_name = "Grace", last_name = "Hopper",
                  birthday = "1906-12-09", city = "New York"), conn = conn, user = "demo")
    insert_record(people_form, list(first_name = "Alan", last_name = "Turing",
                  birthday = "1912-06-23", city = "London"), conn = conn, user = "demo")
  }

  if (nrow(fetch_records(locations_form, conn = conn)) == 0L) {
    ppl <- fetch_records(people_form, conn = conn)
    ada_id <- ppl$sft_easy_id[match("Ada", ppl$first_name)]
    insert_record(locations_form, list(location_name = "Analytical Engine Lab",
                  contact_id = ada_id), conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Two kinds of derived column"),
    shiny::tags$ol(
      style = "margin: 0.4rem 0 0 1rem; padding: 0;",
      shiny::tags$li(
        shiny::tags$b("Within a row: "),
        "the People table's ", shiny::tags$em("Full name"), " and ",
        shiny::tags$em("Age"), " columns are computed from the first/last name ",
        "and birthday. Edit a birthday and the Age updates immediately."
      ),
      shiny::tags$li(
        shiny::tags$b("From another table: "),
        "a Location stores only a reference to a person, but the table shows that ",
        "person's ", shiny::tags$em("Contact"), " name and ",
        shiny::tags$em("Contact city"), " by joining the People records at render ",
        "time. Edit a person's name or city and the Locations table follows."
      )
    )
  )
}

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

app_ui <- shiny::tagList(
  how_to(),
  form_ui(
    id = "people",
    title = "People",
    show_user = FALSE,
    show_audit = FALSE,
    show_refresh_table = FALSE,
    show_deleted_records = FALSE,
    show_column_settings = FALSE,
    show_column_selection = FALSE,
    button_options = neutral_buttons
  ),
  tags$div(
    style = "margin-top: 1.5rem;",
    form_ui(
      id = "locations",
      title = "Locations",
      show_user = FALSE,
      show_audit = FALSE,
      show_refresh_table = FALSE,
      show_deleted_records = FALSE,
      show_column_settings = FALSE,
      show_column_selection = FALSE,
      button_options = neutral_buttons
    )
  )
)

#> STEP: Derive the columns in the servers
#> NOTE: display_transform() adds Full name / Age within a row. The Locations
#> NOTE: form joins People at render time and rides refresh_triggers, so editing
#> NOTE: a person updates its derived Contact columns live.
server <- function(input, output, session) {
  people <- form_server(
    id = "people",
    form = people_form,
    user = "demo",
    show_audit = FALSE,
    table_columns = c("sft_easy_id", "full_name", "birthday", "age", "city"),
    display_column_labels = c(sft_easy_id = "Person ID", full_name = "Full name", age = "Age"),
    display_transform = function(data) {
      data$full_name <- full_name_of(data$first_name, data$last_name)
      data$age <- age_in_years(data$birthday)
      data
    },
    persist_column_settings = FALSE
  )

  form_server(
    id = "locations",
    form = locations_form,
    user = "demo",
    show_audit = FALSE,
    # Re-derive the displayed contact name/city whenever a person changes.
    refresh_triggers = list(people$changed),
    table_columns = c("sft_easy_id", "location_name", "contact_name", "contact_city"),
    display_column_labels = c(
      sft_easy_id = "Location ID", contact_name = "Contact", contact_city = "Contact city"
    ),
    display_transform = function(data, context) {
      ppl <- people$records()
      idx <- match(data$contact_id, ppl$sft_easy_id)
      data$contact_name <- ifelse(
        is.na(idx), NA_character_,
        full_name_of(ppl$first_name[idx], ppl$last_name[idx])
      )
      data$contact_city <- ppl$city[idx]
      data
    },
    input_bindings = list(
      dynamic_choices(
        field = "contact_id",
        choices = function() {
          ppl <- people$records()
          ppl$full_name <- full_name_of(ppl$first_name, ppl$last_name)
          reference_choices(
            data = ppl, value = "sft_easy_id", label = "full_name",
            extra = "city", include_empty = TRUE
          )
        },
        selected = "preserve",
        update_args = list(
          server = TRUE,
          options = list(placeholder = "Pick a person")
        )
      )
    ),
    persist_column_settings = FALSE
  )
}
#> END

ui <- demo_page("Calculated columns", "app_calculated_columns", app_ui)

shinyApp(ui, server)
