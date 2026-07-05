.sft_package_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    description_path <- file.path(path, "DESCRIPTION")

    if (file.exists(description_path)) {
      description <- readLines(description_path, warn = FALSE)

      if (any(grepl("^Package:\\s*shinyformtools\\s*$", description))) {
        return(path)
      }
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop("Could not find shinyformtools package root.", call. = FALSE)
    }

    path <- parent
  }
}

#' Get path to a shinyformtools example app
#'
#' @param example Example name, with or without `.R`.
#'
#' @return Path to the example file.
#' @export
example_path <- function(example) {
  if (!sft_is_scalar_character(example)) {
    stop("example must be a non-empty character scalar.", call. = FALSE)
  }

  filename <- if (grepl("\\.R$", example)) {
    example
  } else {
    paste0(example, ".R")
  }

  installed_path <- system.file(
    "examples",
    filename,
    package = "shinyformtools",
    mustWork = FALSE
  )

  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  dev_path <- file.path(
    .sft_package_root(),
    "inst",
    "examples",
    filename
  )

  if (file.exists(dev_path)) {
    return(dev_path)
  }

  stop(
    "Example not found: ",
    filename,
    ". Available examples: ",
    paste(list_examples(), collapse = ", "),
    call. = FALSE
  )
}

#' List available shinyformtools example apps
#'
#' @return Character vector of example names.
#' @export
list_examples <- function() {
  installed_dir <- system.file(
    "examples",
    package = "shinyformtools",
    mustWork = FALSE
  )

  if (nzchar(installed_dir) && dir.exists(installed_dir)) {
    files <- list.files(installed_dir, pattern = "\\.R$")
    return(sub("\\.R$", "", files))
  }

  dev_dir <- file.path(
    .sft_package_root(),
    "inst",
    "examples"
  )

  if (!dir.exists(dev_dir)) {
    return(character())
  }

  files <- list.files(dev_dir, pattern = "\\.R$")

  sub("\\.R$", "", files)
}

#' Run a shinyformtools example app
#'
#' @param example Example name, with or without `.R`.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Runs a Shiny app.
#' @export
run_example <- function(example, ...) {
  shiny::runApp(
    appDir = example_path(example),
    ...
  )
}