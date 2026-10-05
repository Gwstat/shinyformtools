.sft_package_root <- function(path = getwd(), package = "shinyformtools") {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  pattern <- paste0("^Package:\\s*", package, "\\s*$")

  repeat {
    description_path <- file.path(path, "DESCRIPTION")

    if (file.exists(description_path)) {
      description <- readLines(description_path, warn = FALSE)

      if (any(grepl(pattern, description))) {
        return(path)
      }
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop("Could not find ", package, " package root.", call. = FALSE)
    }

    path <- parent
  }
}

# The folder holding the examples of `package`: installed, loaded from source
# (devtools::load_all()), or the source tree around the working directory.
sft_examples_dir <- function(package) {
  if (!sft_is_scalar_character(package)) {
    stop("package must be a non-empty character scalar.", call. = FALSE)
  }

  installed_dir <- system.file("examples", package = package, mustWork = FALSE)
  if (nzchar(installed_dir) && dir.exists(installed_dir)) {
    return(installed_dir)
  }

  candidates <- character()
  loaded <- tryCatch(find.package(package, quiet = TRUE), error = function(err) character())
  if (length(loaded)) candidates <- file.path(loaded[1], "inst", "examples")
  # Outside the source tree (an installed package) there is no root.
  root <- tryCatch(.sft_package_root(package = package), error = function(err) NULL)
  if (!is.null(root)) candidates <- c(candidates, file.path(root, "inst", "examples"))

  for (dir in candidates) {
    if (dir.exists(dir)) {
      return(dir)
    }
  }

  if (!length(loaded) && is.null(root)) {
    stop("Package not found: ", package, ".", call. = FALSE)
  }
  NULL
}

#' Get path to an example app
#'
#' @param example Example name, with or without `.R`.
#' @param package The package whose examples to use. Default
#'   `"shinyformtools"`; an extension built on it (shinygridtools,
#'   shinycarttools, ...) ships its examples the same way.
#'
#' @return Path to the example file.
#' @examples
#' example_path("app_crud_basic")
#' @export
example_path <- function(example, package = "shinyformtools") {
  if (!sft_is_scalar_character(example)) {
    stop("example must be a non-empty character scalar.", call. = FALSE)
  }

  filename <- if (grepl("\\.R$", example)) {
    example
  } else {
    paste0(example, ".R")
  }

  dir <- sft_examples_dir(package)
  path <- if (is.null(dir)) "" else file.path(dir, filename)

  if (nzchar(path) && file.exists(path)) {
    return(path)
  }

  stop(
    "Example not found: ",
    filename,
    ". Available examples: ",
    paste(list_examples(package), collapse = ", "),
    call. = FALSE
  )
}

#' List available example apps
#'
#' Files whose name starts with an underscore are shared helpers sourced by the
#' examples (such as the walkthrough scaffolding), not runnable apps, so they
#' are not listed.
#'
#' @inheritParams example_path
#'
#' @return Character vector of example names.
#' @examples
#' list_examples()
#' @export
list_examples <- function(package = "shinyformtools") {
  dir <- sft_examples_dir(package)

  if (is.null(dir) || !dir.exists(dir)) {
    return(character())
  }

  files <- list.files(dir, pattern = "^[^_].*\\.R$")

  sub("\\.R$", "", files)
}

#' Run an example app
#'
#' @inheritParams example_path
#' @param ... Passed to [shiny::runApp()].
#' @param package The package whose examples to use, by name (it comes after
#'   `...`). Default `"shinyformtools"`.
#'
#' @return Runs a Shiny app.
#' @examples
#' \dontrun{
#' run_example("app_crud_basic")
#' run_example("app_grid_entry", package = "shinygridtools")
#' }
#' @export
run_example <- function(example, ..., package = "shinyformtools") {
  shiny::runApp(
    appDir = example_path(example, package = package),
    ...
  )
}
