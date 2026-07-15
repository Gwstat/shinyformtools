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

# JSON-encode a vector as a JSON array even when it has length 1. sft_as_json()
# auto-unboxes, which would store a single selection of a multi-value field as
# a bare JSON scalar that the array-expecting decode path cannot parse.
sft_as_json_array <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = FALSE,
    null = "null",
    POSIXt = "ISO8601"
  )
}

# Serializer for audit-log snapshots: keeps NA fields as explicit JSON nulls.
# The default toJSON() drops NA columns from a one-row data frame entirely, so
# restore would silently skip every field that was empty in the snapshot.
sft_as_json_snapshot <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    POSIXt = "ISO8601"
  )
}

# Does this form store a per-record UUID? Opt-in via form(uuid = TRUE); a form
# list built before the option existed has no $uuid, so default to FALSE.
sft_form_has_uuid <- function(form) {
  isTRUE(form$uuid)
}

# Does this form store the short quotable id? Opt-in via form(easy_id = TRUE).
sft_form_has_easy_id <- function(form) {
  isTRUE(form$easy_id)
}

# Looking a record up by record_uuid only works when the form stores one. Say so
# plainly instead of letting the query fail with "no such column: sft_uuid".
sft_check_record_uuid_supported <- function(form, record_uuid) {
  if (is.null(record_uuid) || sft_form_has_uuid(form)) {
    return(invisible(TRUE))
  }

  stop(
    "form '", form$form_id, "' does not store record UUIDs, so record_uuid ",
    "cannot be used. Identify the record by record_id, or create the form with ",
    "form(uuid = TRUE).",
    call. = FALSE
  )
}

# The id column a user is shown: sft_easy_id when the form stores one, else the
# primary key. Everything user-facing (records table, dialog titles) must go
# through this rather than naming sft_easy_id, which may not exist.
sft_display_id_column <- function(form) {
  if (sft_form_has_easy_id(form)) "sft_easy_id" else "sft_id"
}

# The display id of one record row, for dialog titles and messages. Derived from
# the row rather than the form, because the modal helpers do not all carry the
# form. Also covers a database whose sft_easy_id column exists but was never
# populated (written before the form opted in).
sft_row_display_id <- function(row) {
  if ("sft_easy_id" %in% names(row) && !is.na(row$sft_easy_id[1])) {
    return(row$sft_easy_id[1])
  }

  row$sft_id[1]
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