# Cascading address inputs driven entirely by binding declarations.
#
# Open "Add" on Locations (or edit a row) to see the cascade:
#   Street drives the House-number choices, the House number drives the Suffix
#   choices, and the ZIP is derived from the chosen address. It is built from
#   dynamic_choices() / dynamic_value() bindings - no hand-written observers.
#
# The modal header echoes the address as it is filled in and shows a short
# change history for the record (changelog_box).
#
# (Cross-table derived columns - showing data joined from another table - are a
# separate idea; see app_calculated_columns for the Locations -> People join.)
#
# Run with: shinyformtools::run_example("app_cascading_inputs")

library(shiny)
library(shinyformtools)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "The 'app_cascading_inputs' example needs the 'dplyr' package. ",
    "install.packages('dplyr').",
    call. = FALSE
  )
}

#> STEP: Reference data the cascade reads from
#> NOTE: A plain lookup table of valid addresses -> ZIP. The binding functions
#> NOTE: below filter it to drive each field's choices and the derived ZIP.
# --- A small reference table of valid addresses -> ZIP ------------------------
address_book <- data.frame(
  street = c(
    "Acker Street", "Acker Street", "Old Market", "Old Market",
    "Broad Way", "Broad Way", "Broad Way"
  ),
  house_no = c("1", "2", "6", "7", "10", "10", "11"),
  house_suffix = c("", "", "", "", "", "a", ""),
  zip = c("39112", "39112", "39104", "39104", "39104", "39104", "39104"),
  stringsAsFactors = FALSE
)

first_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return("")
  }
  as.character(x[[1L]])
}

zip_from_address <- function(values) {
  out <- address_book |>
    dplyr::filter(
      street == first_or_empty(values$street),
      house_no == first_or_empty(values$house_no),
      house_suffix == first_or_empty(values$house_suffix)
    ) |>
    dplyr::pull(zip)

  if (length(out) == 0L) "" else out[[1L]]
}
#> END

# --- Address echo + change history shown in the locations modal header --------
location_modal_header <- function(values, record, context, prefix) {
  location_name <- first_or_empty(values$location_name)
  address <- trimws(paste0(
    first_or_empty(values$street), " ",
    first_or_empty(values$house_no),
    first_or_empty(values$house_suffix)
  ))
  zip <- first_or_empty(values$zip)

  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border: 1px solid #ddd; border-radius: 4px; background: #f8f8f8;"
    ),
    shiny::fluidRow(
      shiny::column(
        width = 7,
        shiny::tags$strong(if (nzchar(location_name)) location_name else "New location"),
        shiny::tags$br(),
        shiny::tags$span(if (nzchar(address)) address else "Address still incomplete"),
        if (nzchar(zip)) shiny::tagList(shiny::tags$br(), shiny::tags$span(zip))
      ),
      shiny::column(
        width = 5,
        changelog_box(
          context = context,
          record = record,
          title = "History",
          limit = 3L,
          empty_text = "No saved changes yet."
        )
      )
    )
  )
}

# --- One form backed by a SQLite database ------------------------------------
db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the form
#> NOTE: Street / House number / Suffix are selectizeInputs with empty choices -
#> NOTE: the cascade fills them at runtime. ZIP is editable = FALSE (derived).
locations_form <- form(
  form_id = "locations_cascade",
  form_name = "Locations",
  table_name = "cascade_locations",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "location_name", label = "Location name", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "street", label = "Street", input_type = "selectizeInput",
      args = list(
        choices = character(),
        options = list(create = TRUE, placeholder = "Pick or add a street")
      ),
      mandatory = TRUE, col = 1, pos = 2
    ),
    form_field(
      id = "house_no", label = "House number", input_type = "selectizeInput",
      args = list(
        choices = character(),
        options = list(create = TRUE, placeholder = "House number")
      ),
      mandatory = TRUE, col = 1, pos = 3
    ),
    form_field(
      id = "house_suffix", label = "Suffix", input_type = "selectizeInput",
      args = list(
        choices = character(),
        options = list(create = TRUE, placeholder = "optional")
      ),
      col = 1, pos = 4
    ),
    form_field(id = "zip", label = "ZIP (derived)", editable = FALSE, col = 2, pos = 1)
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
seed_example_data <- function() {
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(locations_form, conn = conn, user = "demo")

  if (nrow(fetch_records(locations_form, conn = conn)) == 0L) {
    insert_record(
      locations_form,
      list(location_name = "City Hall", street = "Old Market", house_no = "6",
           house_suffix = "", zip = "39104"),
      conn = conn, user = "demo"
    )
  }
}

seed_example_data()

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("What to try"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Click ", shiny::tags$em("Add"), ". Pick a ", shiny::tags$em("Street"),
      " - the House-number list updates; pick one - the Suffix list updates; ",
      "the ", shiny::tags$em("ZIP"), " fills in by itself. The modal header ",
      "echoes the address as you go."
    )
  )
}

# --- Server ------------------------------------------------------------------
#> STEP: Wire the cascade in the server
#> NOTE: dynamic_choices() makes each field's options depend on the previous
#> NOTE: one; dynamic_value() derives ZIP. display_transform builds an Address
#> NOTE: column at render time. No hand-written observers.
server <- function(input, output, session) {
  form_server(
    id = "locations",
    form = locations_form,
    user = "demo",
    table_columns = c("sft_easy_id", "location_name", "address", "zip"),
    display_column_labels = c(sft_easy_id = "Location ID", address = "Address"),
    display_transform = function(data, context) {
      data |>
        dplyr::mutate(address = paste0(street, " ", house_no, house_suffix))
    },
    modal_header = location_modal_header,
    persist_column_settings = FALSE,
    input_bindings = list(
      dynamic_choices(
        field = "street",
        choices = function() sort(unique(address_book$street)),
        selected = "preserve",
        update_args = list(
          server = TRUE,
          options = list(create = TRUE, placeholder = "Pick or add a street")
        )
      ),
      dynamic_choices(
        field = "house_no",
        depends_on = "street",
        choices = function(values) {
          street <- first_or_empty(values$street)
          if (!nzchar(street)) return(character())
          address_book |>
            dplyr::filter(street == .env$street) |>
            dplyr::pull(house_no) |>
            unique() |>
            sort()
        },
        selected = "preserve",
        update_args = list(
          server = TRUE,
          options = list(create = TRUE, placeholder = "House number")
        )
      ),
      dynamic_choices(
        field = "house_suffix",
        depends_on = c("street", "house_no"),
        choices = function(values) {
          street <- first_or_empty(values$street)
          house_no <- first_or_empty(values$house_no)
          if (!nzchar(street) || !nzchar(house_no)) return(setNames("", ""))
          suffixes <- address_book |>
            dplyr::filter(street == .env$street, house_no == .env$house_no) |>
            dplyr::pull(house_suffix) |>
            unique()
          unique(c(setNames("", ""), suffixes[nzchar(suffixes)]))
        },
        selected = "preserve",
        update_args = list(
          server = TRUE,
          options = list(create = TRUE, placeholder = "optional")
        )
      ),
      dynamic_value(
        field = "zip",
        depends_on = c("street", "house_no", "house_suffix"),
        value = zip_from_address
      )
    )
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Cascading inputs"),
  how_to(),
  form_ui(
    id = "locations",
    title = "Locations",
    show_refresh_table = FALSE
  )
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_cascading_inputs")
#> DEMO END

shinyApp(ui, server)
