# Internal modal-size option helpers.

sft_default_modal_sizes <- function() {
  list(
    add = list(size = "l", width = NULL, height = NULL, max_height = NULL),
    edit = list(size = "l", width = NULL, height = NULL, max_height = "75vh"),
    delete = list(size = "m", width = NULL, height = NULL, max_height = NULL),
    versions = list(size = "l", width = NULL, height = NULL, max_height = NULL),
    deleted_records = list(size = "l", width = NULL, height = NULL, max_height = NULL),
    column_settings = list(size = "m", width = NULL, height = NULL, max_height = NULL),
    column_selection = list(size = "m", width = NULL, height = NULL, max_height = NULL)
  )
}

sft_validate_css_length <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    stop(name, " must be NULL or a non-empty character scalar.", call. = FALSE)
  }

  value
}

sft_normalize_modal_setting <- function(value, name) {
  valid_sizes <- c("s", "m", "l", "xl")

  if (is.null(value)) {
    return(NULL)
  }

  if (is.character(value) && length(value) == 1L && value %in% valid_sizes) {
    return(list(size = value, width = NULL, height = NULL, max_height = NULL))
  }

  if (is.character(value) && length(value) == 1L && nzchar(value)) {
    return(list(size = "m", width = value, height = NULL, max_height = NULL))
  }

  if (!is.list(value)) {
    stop(
      "modal_sizes$", name,
      " must be a size string, a CSS width string, or a named list.",
      call. = FALSE
    )
  }

  size <- value$size %||% "m"

  if (!is.character(size) || length(size) != 1L || !size %in% valid_sizes) {
    stop(
      "modal_sizes$", name, "$size must be one of: ",
      paste(valid_sizes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  list(
    size = size,
    width = sft_validate_css_length(value$width, paste0("modal_sizes$", name, "$width")),
    height = sft_validate_css_length(value$height, paste0("modal_sizes$", name, "$height")),
    max_height = sft_validate_css_length(value$max_height, paste0("modal_sizes$", name, "$max_height"))
  )
}

sft_modal_sizes <- function(modal_sizes = list()) {
  if (is.null(modal_sizes)) {
    modal_sizes <- list()
  }

  if (!is.list(modal_sizes)) {
    stop("modal_sizes must be a named list.", call. = FALSE)
  }

  raw <- utils::modifyList(
    sft_default_modal_sizes(),
    modal_sizes,
    keep.null = TRUE
  )

  out <- lapply(
    names(raw),
    function(name) {
      sft_normalize_modal_setting(raw[[name]], name)
    }
  )

  names(out) <- names(raw)
  out
}

sft_modal_setting <- function(modal_sizes, key) {
  modal_sizes[[key]] %||% list(size = "m", width = NULL, height = NULL, max_height = NULL)
}

sft_modal_size <- function(modal_sizes, key) {
  sft_modal_setting(modal_sizes, key)$size %||% "m"
}

sft_modal_css <- function(modal_sizes, key) {
  setting <- sft_modal_setting(modal_sizes, key)
  rules <- character()

  # "xl" is a Bootstrap 4+ class with no rule under Shiny's default Bootstrap 3,
  # so back it with an explicit width to widen the dialog on every Bootstrap
  # version. An explicit width always wins.
  width <- setting$width
  if (is.null(width) && identical(setting$size, "xl")) {
    width <- "min(1140px, 95vw)"
  }

  if (!is.null(width)) {
    rules <- c(
      rules,
      paste0(".modal-dialog { width: ", width, " !important; max-width: ", width, " !important; }")
    )
  }

  if (!is.null(setting$height)) {
    rules <- c(
      rules,
      paste0(".modal-content { height: ", setting$height, " !important; }"),
      ".modal-body { overflow-y: auto; }"
    )
  }

  if (!is.null(setting$max_height)) {
    rules <- c(
      rules,
      paste0(".modal-body { max-height: ", setting$max_height, " !important; overflow-y: auto; }")
    )
  }

  if (length(rules) == 0L) {
    return(NULL)
  }

  shiny::tags$style(shiny::HTML(paste(rules, collapse = "\n")))
}

