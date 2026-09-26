.onLoad <- function(libname, pkgname) {
  # The grid input reports {rows, cols, values}; the handler turns that into a
  # matrix before it reaches input$<id>.
  shiny::registerInputHandler("shinyformtools.grid", sft_grid_input_handler, force = TRUE)
  invisible(NULL)
}
