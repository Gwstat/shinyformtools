# The grid moved to the package shinygridtools in 0.9.0, the basket to
# shinycarttools (via shinygridtools, 2026-10-03). Both depend on this
# package, never the other way round: nothing here loads or calls them. The
# old function names stay for good and stop with a message saying where they
# went; attaching the package puts its functions in front of these, and the
# input types "grid_input" and "cart_input" then work in every form.

sft_moved_input_types <- function() {
  c("grid_input", "cart_input")
}

# Which package each moved name and type lives in now.
sft_moved_package <- function(name) {
  if (grepl("cart|board", name)) "shinycarttools" else "shinygridtools"
}

sft_moved_message <- function(what, pkg = "shinygridtools") {
  paste0(
    what, " moved to the package ", pkg, " (out of shinyformtools since 0.9.0). ",
    "Install it with remotes::install_github(\"Gwstat/", pkg, "\") ",
    "and attach it with library(", pkg, ")."
  )
}

sft_moved_stop <- function(name) {
  stop(sft_moved_message(paste0("`", name, "()`"), sft_moved_package(name)), call. = FALSE)
}

#' Moved to shinygridtools and shinycarttools
#'
#' The grid (`grid_input()`, `update_grid_input()`, `grid_ui()`,
#' `grid_server()`) moved to the package \pkg{shinygridtools}, the basket
#' (`cart_input()`, `update_cart_input()`, `cart_catalog()`, the board) to
#' \pkg{shinycarttools}. These names stop with a message saying so. Attach
#' the package with `library()`: its functions of the same names are then
#' found first, and it registers its input type (`"grid_input"` or
#' `"cart_input"`) and, for shinycarttools, the board of
#' `form_ui(show_board = TRUE)`. Stored values are the same as before.
#'
#' @param id,inputId,session,form Ignored; see the functions of the same name
#'   in \pkg{shinygridtools} or \pkg{shinycarttools}.
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
