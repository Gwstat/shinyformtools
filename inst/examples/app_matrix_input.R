# A cross table as ONE field: the units a store sold per product and quarter,
# entered in a matrix inside the regular add / edit dialog.
#
# The one-row model of shinyformtools does not change for this. The whole
# matrix is the value of a single field (`sales`); `register_input()` teaches
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
#> NOTE: The products are the rows, the quarters the columns. This empty
#> NOTE: matrix is the dialog's starting value and the shape every stored value
#> NOTE: is read back into.
products <- c("Coffee", "Tea", "Cocoa", "Other")
quarters <- c("Q1", "Q2", "Q3", "Q4")

empty_table <- matrix(
  0, nrow = length(products), ncol = length(quarters),
  dimnames = list(products, quarters)
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
    return(empty_table)
  }
  parsed <- jsonlite::fromJSON(stored)
  out <- matrix(as.numeric(parsed$values), nrow = length(parsed$rows), byrow = FALSE)
  dimnames(out) <- list(parsed$rows, parsed$cols)
  out
}

totals_text <- function(stored, ...) {
  m <- json_to_matrix(stored)
  paste(sprintf("%s %s", colnames(m), format(colSums(m, na.rm = TRUE), big.mark = ",")),
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
#> NOTE: `sales` is an ordinary field with input_type "matrixInput". The rule
#> NOTE: receives the decoded matrix in values$sales, so it can check cells.
sales_form <- form(
  form_id = "store_sales",
  form_name = "Sales per store",
  table_name = "store_sales",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "store", label = "Store", mandatory = TRUE, unique = TRUE, pos = 1),
    form_field(
      id = "sales", label = "Units sold per product and quarter", input_type = "matrixInput",
      args = list(
        value = empty_table, class = "numeric",
        rows = list(names = TRUE), cols = list(names = TRUE)
      ),
      pos = 2
    )
  ),
  validation_rules = list(
    forbid_if(
      id = "no_negative_sales",
      condition = function(values) any(values$sales < 0, na.rm = TRUE),
      fields = "sales",
      message = "Units sold cannot be negative."
    )
  )
)
#> END

# --- Seed a little demo data once --------------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(sales_form, conn = conn, user = "demo")

  if (nrow(fetch_records(sales_form, conn = conn)) == 0L) {
    harbour <- empty_table
    harbour["Coffee", ] <- c(412, 388, 405, 520)
    harbour["Tea", ] <- c(205, 171, 140, 260)
    harbour["Cocoa", ] <- c(98, 40, 25, 175)
    harbour["Other", ] <- c(61, 57, 66, 80)
    insert_record(sales_form, list(store = "Harbour Street", sales = harbour),
                  conn = conn, user = "demo")
  }
})

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("How to use it"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "\"Add store\" opens the dialog with the cross table. ",
      "The table shows the quarter totals; a negative number is refused on save."
    )
  )
}

#> STEP: Wire the server
en <- language(labels = list(open_add = "Add store"))

server <- function(input, output, session) {
  form_server(
    id = "sales", form = sales_form, user = "demo", language = en,
    columns = list(visible = c("sft_id", "store", "sales"), persist = FALSE)
  )
}
#> END

#> STEP: Build the UI
ui <- fluidPage(
  titlePanel("Sales per store"),
  how_to(),
  form_ui("sales", title = "Stores", language = en, show_export = TRUE)
)
#> END

#> DEMO
# The walkthrough beside the app; not part of the application.
source(example_path("_demo_scaffold"), local = TRUE)
ui <- demo_page(ui, "app_matrix_input")
#> DEMO END

shinyApp(ui, server)
