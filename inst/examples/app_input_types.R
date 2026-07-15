# A tour of the input types and the non-input field kinds.
#
# Every input type the package supports appears once, grouped into three columns:
#
#   - Text & special: textInput, passwordInput, textAreaInput, ibanInput.
#   - Numbers & dates: numericInput, sliderInput, dateInput, dateRangeInput,
#     timeInput.
#   - Choices: selectInput, selectizeInput (free entry), radioButtons,
#     checkboxInput, checkboxGroupInput, multiInput. Multi-value choices are
#     stored natively as a JSON array.
#
# Two non-input field kinds are shown alongside the inputs:
#
#   - html_field(): static markup placed in the form (here an info banner). It is
#     not stored and never collected on save.
#   - output_field(): a reactive output (text/plot/ui) placed in the form. The
#     package renders the placeholder; you fill it from the server. The only hook
#     into the form module's `output` is modal_header(), which receives
#     `output`, `input`, `ns` and `session` - here it registers a live text
#     "preview" that updates as you type, keyed by the field id with the add_ /
#     edit_ prefix.
#
# Run with: shinyformtools::run_example("app_input_types")

library(shiny)
library(shinyformtools)

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: Every input type appears once, grouped into three columns. html_field()
#> NOTE: adds static markup and output_field() reserves a reactive output slot;
#> NOTE: neither is a stored input.
tour_form <- form(
  form_id = "input_tour",
  form_name = "Input tour",
  table_name = "input_tour",
  db = db_sqlite(db_path),
  fields = list(
    # --- Column 1: text & special, plus an HTML banner ----------------------
    html_field(
      id = "intro",
      html = paste(
        "<div style='padding:0.5rem 0.75rem;background:#eef3fb;",
        "border-left:4px solid #4178be;border-radius:4px;'>",
        "<b>Field tour.</b> Every input type appears once. This blue box is an",
        "<code>html_field()</code>; the live preview on the right is an",
        "<code>output_field()</code>.</div>"
      ),
      col = 1, pos = 1
    ),
    form_field(id = "name", label = "Name (textInput)", mandatory = TRUE, col = 1, pos = 2),
    form_field(
      id = "secret", label = "Secret (passwordInput)",
      input_type = "passwordInput", col = 1, pos = 3
    ),
    form_field(
      id = "notes", label = "Notes (textAreaInput)",
      input_type = "textAreaInput", args = list(rows = 2), col = 1, pos = 4
    ),
    # ibanInput validates client-side; DE = TRUE (its default) accepts only
    # German IBANs, colouring the field green when valid and red when not. A
    # valid example IBAN is prefilled so it starts green; pass args = list(DE =
    # FALSE) to accept any country.
    form_field(
      id = "iban", label = "IBAN (ibanInput, German)",
      input_type = "ibanInput", args = list(value = "DE89370400440532013000"),
      col = 1, pos = 5
    ),

    # --- Column 2: numbers & dates ------------------------------------------
    form_field(
      id = "age", label = "Age (numericInput)",
      input_type = "numericInput", args = list(value = 30, min = 0, max = 120),
      col = 2, pos = 1
    ),
    form_field(
      id = "rating", label = "Rating (sliderInput)",
      input_type = "sliderInput", args = list(min = 1, max = 10, value = 5),
      col = 2, pos = 2
    ),
    form_field(
      id = "start_date", label = "Start (dateInput)",
      input_type = "dateInput", args = list(value = Sys.Date()), col = 2, pos = 3
    ),
    form_field(
      id = "period", label = "Period (dateRangeInput)",
      input_type = "dateRangeInput",
      args = list(start = Sys.Date(), end = Sys.Date() + 7), col = 2, pos = 4
    ),
    form_field(
      id = "reminder", label = "Reminder (timeInput)",
      input_type = "timeInput", args = list(seconds = FALSE), col = 2, pos = 5
    ),

    # --- Column 3: choices --------------------------------------------------
    form_field(
      id = "team", label = "Team (selectInput)",
      input_type = "selectInput",
      args = list(choices = c("Sales", "Support", "Engineering"), selected = "Sales"),
      col = 3, pos = 1
    ),
    form_field(
      id = "city", label = "City (selectizeInput, free entry)",
      input_type = "selectizeInput",
      args = list(
        choices = c("London", "New York", "Berlin"),
        options = list(create = TRUE, placeholder = "Pick or type a city")
      ),
      col = 3, pos = 2
    ),
    form_field(
      id = "size", label = "Size (radioButtons)",
      input_type = "radioButtons",
      args = list(choices = c("S", "M", "L"), selected = "M", inline = TRUE),
      col = 3, pos = 3
    ),
    form_field(
      id = "active", label = "Active (checkboxInput)",
      input_type = "checkboxInput", args = list(value = TRUE), col = 3, pos = 4
    ),
    form_field(
      id = "skills", label = "Skills (checkboxGroupInput)",
      input_type = "checkboxGroupInput",
      args = list(choices = c("R", "Python", "SQL", "JS"), inline = TRUE),
      col = 3, pos = 5
    ),
    form_field(
      id = "languages", label = "Languages (multiInput)",
      input_type = "multiInput",
      args = list(choices = c("English", "German", "French", "Spanish")),
      col = 3, pos = 6
    ),

    # --- Reactive output placed in the form ---------------------------------
    output_field(id = "preview", output_type = "text", label = "Live preview", col = 3, pos = 7)
  )
)
#> END

# --- Seed one record so the table is not empty -------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(tour_form, conn = conn, user = "demo")

  if (nrow(fetch_records(tour_form, conn = conn)) == 0L) {
    insert_record(
      tour_form,
      list(name = "Ada Lovelace", secret = "hunter2", notes = "Founder.",
           age = 36, rating = 9, team = "Engineering", city = "London",
           size = "M", active = TRUE, skills = c("R", "SQL"),
           languages = c("English", "French")),
      conn = conn, user = "demo"
    )
  }
})

#> STEP: Fill the output_field from the server
#> NOTE: The modal_header hook receives output / input / ns / session, so it can
#> NOTE: write the "preview" output, keyed by the field id with the add_/edit_
#> NOTE: prefix - a live preview that updates as you type.
# Fill the output_field "preview" from the form module's output object. This is
# the supported hook: modal_header() receives output / input / ns / session.
preview_header <- function(values, input, output, session, prefix) {
  output[[paste0(prefix, "preview")]] <- shiny::renderText({
    pick <- function(id) {
      v <- input[[paste0(prefix, id)]]
      if (is.null(v) || length(v) == 0L) "" else as.character(v)
    }
    name <- pick("name")
    team <- pick("team")
    rating <- pick("rating")
    skills <- input[[paste0(prefix, "skills")]]
    paste0(
      if (nzchar(name)) name else "(no name)",
      " - ", if (nzchar(team)) team else "(no team)",
      " - rating ", if (!nzchar(rating)) "?" else rating,
      if (length(skills)) paste0(" - ", paste(skills, collapse = ", ")) else ""
    )
  })

  NULL
}
#> END

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("What to look for"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Click ", shiny::tags$em("Add entry"), " to open the form. Every input ",
      "type is there; the blue banner is an ", shiny::tags$em("html_field"),
      " and the ", shiny::tags$em("Live preview"),
      " text is an ", shiny::tags$em("output_field"),
      " that updates as you fill the Name, Team, Rating and Skills. The IBAN ",
      "field turns green for a valid German IBAN and red otherwise."
    )
  )
}

#> STEP: Wire the server
#> NOTE: modal_header = preview_header installs the live preview; modal_sizes
#> NOTE: widens the dialog so all three columns fit.
server <- function(input, output, session) {
  form_server(
    id = "tour",
    form = tour_form,
    user = "demo",
    table_columns = c("sft_easy_id", "name", "age", "team", "city", "active", "skills"),
    modal_header = preview_header,
    modal_sizes = list(
      add = list(size = "l", width = "92vw", max_height = "80vh"),
      edit = list(size = "l", width = "92vw", max_height = "80vh")
    ),
    persist_column_settings = FALSE
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Input types & field kinds"),
  how_to(),
  form_ui(
    id = "tour",
    title = "Input tour"
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_input_types")
#> DEMO END

shinyApp(ui, server)
