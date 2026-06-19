`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

sft_reserved_prefix <- "sft_"

sft_is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

sft_is_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

sft_is_scalar_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

sft_check_identifier <- function(x, what = "identifier") {
  if (!sft_is_scalar_character(x)) {
    stop(what, " must be a non-empty character scalar.", call. = FALSE)
  }

  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    stop(
      what,
      " must start with a letter and may only contain letters, numbers, and underscores.",
      call. = FALSE
    )
  }

  if (startsWith(x, sft_reserved_prefix)) {
    stop(
      what,
      " must not start with the reserved prefix 'sft_'.",
      call. = FALSE
    )
  }

  invisible(x)
}

sft_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
}

sft_as_json <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    POSIXt = "ISO8601"
  )
}

sft_quote_identifier <- function(conn, x) {
  as.character(DBI::dbQuoteIdentifier(conn, x))
}

# Comma-separated list of quoted column identifiers, e.g. for INSERT column
# lists and CREATE INDEX column lists. Centralises the quote-and-join idiom.
sft_sql_quoted_columns <- function(conn, columns) {
  paste(
    vapply(
      columns,
      function(column) sft_quote_identifier(conn, column),
      character(1)
    ),
    collapse = ", "
  )
}

sft_sql_literal <- function(conn, x) {
  if (is.null(x)) {
    return("NULL")
  }

  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    return(as.character(x))
  }

  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(ifelse(x, "1", "0"))
  }

  as.character(DBI::dbQuoteString(conn, as.character(x)))
}


sft_db_param <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }

  if (length(x) != 1L) {
    stop("Database parameters must have length 1.", call. = FALSE)
  }

  x
}

sft_random_letters <- function(n = 2L) {
  paste0(sample(LETTERS, n, replace = TRUE), collapse = "")
}

sft_clean_db_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  if (length(x) > 1L) {
    return(as.character(sft_as_json(x)))
  }

  if (inherits(x, "Date")) {
    return(as.character(x))
  }

  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS3%z"))
  }

  if (is.na(x)) {
    return(NA_character_)
  }

  x
}

sft_row_to_list <- function(x) {
  if (!is.data.frame(x) || nrow(x) != 1L) {
    stop("x must be a data frame with exactly one row.", call. = FALSE)
  }

  as.list(x[1, , drop = FALSE])
}