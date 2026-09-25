# A cross table as ONE field: election results per party, first and second
# vote, entered in a matrix inside the regular add / edit dialog.
#
# The one-row model of shinyformtools does not change for this. The whole
# matrix is the value of a single field (`stimmen`); `register_input()` teaches
# the package shinyMatrix::matrixInput and how to store and show its value:
#
#   * encode  - the matrix (with row and column names) becomes a JSON text
#   * decode  - the stored JSON becomes a matrix again for the edit dialog
#   * format  - the records table shows the column totals instead of JSON
#
# A validation rule looks INTO the matrix (no negative counts), so the usual
# refusal-with-glow applies to the cell values too. The export writes the same
# totals text the table shows; the full numbers stay in the database as JSON.
#
# Requires the optional 'shinyMatrix' package.
#
# Run with: shinyformtools::run_example("app_matrix_input")

library(shiny)
library(shinyformtools)

if (!requireNamespace("shinyMatrix", quietly = TRUE)) {
  stop(
    "The 'app_matrix_input' example needs the 'shinyMatrix' package. ",
    "install.packages('shinyMatrix').",
    call. = FALSE
  )
}

db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the cross table
#> NOTE: The parties are the rows, the two vote types the columns. This empty
#> NOTE: matrix is the dialog's starting value and the shape every stored value
#> NOTE: is read back into.
parteien <- c("Partei A", "Partei B", "Partei C", "Sonstige")
stimmarten <- c("Erststimme", "Zweitstimme")

leere_tabelle <- matrix(
  0, nrow = length(parteien), ncol = length(stimmarten),
  dimnames = list(parteien, stimmarten)
)
#> END

#> STEP: Teach the package the matrix input
#> NOTE: register_input() runs BEFORE the form_field() that uses the type.
#> NOTE: encode / decode carry the matrix through a TEXT column as JSON; format
#> NOTE: is what the records table (and the export) show for the field.
matrix_to_json <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- as.matrix(value)
  storage.mode(value) <- "numeric"
  jsonlite::toJSON(
    list(rows = rownames(value), cols = colnames(value), values = unname(value)),
    digits = NA
  )
}

json_to_matrix <- function(stored) {
  if (is.null(stored) || is.na(stored) || !nzchar(stored)) {
    return(leere_tabelle)
  }
  parsed <- jsonlite::fromJSON(stored)
  out <- matrix(as.numeric(parsed$values), nrow = length(parsed$rows), byrow = FALSE)
  dimnames(out) <- list(parsed$rows, parsed$cols)
  out
}

totals_text <- function(stored, ...) {
  m <- json_to_matrix(stored)
  paste(sprintf("%s %s", colnames(m), format(colSums(m, na.rm = TRUE), big.mark = ".", decimal.mark = ",")),
        collapse = " / ")
}

register_input(
  "matrixInput",
  fun = shinyMatrix::matrixInput,
  value_arg = "value",
  update_fun = shinyMatrix::updateMatrixInput,
  encode = matrix_to_json,
  decode = json_to_matrix,
  format = totals_text
)
#> END

#> STEP: Describe the form
#> NOTE: `stimmen` is an ordinary field with input_type "matrixInput". The rule
#> NOTE: receives the decoded matrix in values$stimmen, so it can check cells.
ergebnisse_form <- form(
  form_id = "wahlergebnisse",
  form_name = "Wahlergebnisse",
  table_name = "wahlergebnisse",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "wahlbezirk", label = "Wahlbezirk", mandatory = TRUE, unique = TRUE, pos = 1),
    form_field(
      id = "stimmen", label = "Stimmen je Partei", input_type = "matrixInput",
      args = list(
        value = leere_tabelle, class = "numeric",
        rows = list(names = TRUE), cols = list(names = TRUE)
      ),
      pos = 2
    )
  ),
  validation_rules = list(
    forbid_if(
      id = "keine_negativen_stimmen",
      condition = function(values) any(values$stimmen < 0, na.rm = TRUE),
      fields = "stimmen",
      message = "Stimmenzahlen dürfen nicht negativ sein."
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(ergebnisse_form, conn = conn, user = "demo")

  if (nrow(fetch_records(ergebnisse_form, conn = conn)) == 0L) {
    bezirk_1 <- leere_tabelle
    bezirk_1[, "Erststimme"] <- c(412, 388, 205, 61)
    bezirk_1[, "Zweitstimme"] <- c(398, 371, 240, 57)
    insert_record(ergebnisse_form, list(wahlbezirk = "Bezirk 1", stimmen = bezirk_1),
                  conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Hinweis"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "„Ergebnis erfassen“ öffnet den Dialog mit der Kreuztabelle. ",
      "Die Tabelle zeigt die Spaltensummen; eine negative Zahl wird beim Speichern abgewiesen."
    )
  )
}

#> STEP: Wire the server
de <- german()
de$labels$open_add <- "Ergebnis erfassen"

server <- function(input, output, session) {
  form_server(
    id = "ergebnisse", form = ergebnisse_form, user = "demo", language = de,
    columns = list(visible = c("sft_id", "wahlbezirk", "stimmen"), persist = FALSE)
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Wahlergebnisse je Bezirk"),
  how_to(),
  form_ui("ergebnisse", title = "Erfasste Bezirke", language = de, show_export = TRUE)
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_matrix_input")
#> DEMO END

shinyApp(ui, server)
