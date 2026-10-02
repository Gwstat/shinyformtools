# The grid and the basket moved to the package shinygridtools in 0.9.0. It
# depends on this package, never the other way round: nothing here loads or
# calls it. The old function names stay for one release and stop with a
# message saying where they went; attaching shinygridtools puts its functions
# in front of these.

sft_moved_input_types <- function() {
  c("grid_input", "cart_input")
}

sft_moved_message <- function(what) {
  paste0(
    what, " moved to the package shinygridtools with shinyformtools 0.9.0. ",
    "Install it with remotes::install_github(\"Gwstat/shinygridtools\") ",
    "and attach it with library(shinygridtools)."
  )
}

sft_moved_stop <- function(name) {
  stop(sft_moved_message(paste0("`", name, "()`")), call. = FALSE)
}

#' Moved to shinygridtools
#'
#' The grid (`grid_input()`, `update_grid_input()`, `grid_ui()`,
#' `grid_server()`) and the basket (`cart_input()`, `update_cart_input()`,
#' `cart_catalog()`, the board) moved to the package \pkg{shinygridtools} in
#' shinyformtools 0.9.0. These names stop with a message saying so. Attach
#' \pkg{shinygridtools} with `library(shinygridtools)`: its functions of the
#' same names are then found first, and it registers the input types
#' `"grid_input"` and `"cart_input"` and the board of
#' `form_ui(show_board = TRUE)`. Stored values are the same as before.
#'
#' @param id,inputId,session,form Ignored; see the functions of the same name
#'   in \pkg{shinygridtools}.
#' @param ... Ignored.
#'
#' @return Nothing; they always raise an error.
#' @name moved_to_shinygridtools
#' @keywords internal
NULL

#' @rdname moved_to_shinygridtools
#' @export
grid_input <- function(inputId, ...) sft_moved_stop("grid_input")

#' @rdname moved_to_shinygridtools
#' @export
update_grid_input <- function(session, inputId, ...) sft_moved_stop("update_grid_input")

#' @rdname moved_to_shinygridtools
#' @export
grid_ui <- function(id, ...) sft_moved_stop("grid_ui")

#' @rdname moved_to_shinygridtools
#' @export
grid_server <- function(id, ...) sft_moved_stop("grid_server")

#' @rdname moved_to_shinygridtools
#' @export
cart_input <- function(inputId, ...) sft_moved_stop("cart_input")

#' @rdname moved_to_shinygridtools
#' @export
update_cart_input <- function(session, inputId, ...) sft_moved_stop("update_cart_input")

#' @rdname moved_to_shinygridtools
#' @export
cart_catalog <- function(form, ...) sft_moved_stop("cart_catalog")
