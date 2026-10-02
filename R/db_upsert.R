# Several records of one form written in ONE transaction, matched to the
# stored rows by a key: upsert_records(). Insert, update and soft-delete reuse
# the transaction-free cores of the single-record functions (db_crud.R).

# The caller's records as a list of named lists.
sft_upsert_rows <- function(records) {
  if (is.data.frame(records)) {
    return(lapply(seq_len(nrow(records)), function(i) sft_row_to_list(records[i, , drop = FALSE])))
  }

  if (is.list(records) && all(vapply(records, is.list, logical(1)))) {
    return(lapply(records, as.list))
  }

  stop("records must be a data frame or a list of named lists.", call. = FALSE)
}

# The form's input fields named by `key`, in that order.
sft_key_fields <- function(form, key) {
  if (!is.character(key) || length(key) == 0L || anyNA(key) || !all(nzchar(key))) {
    stop("key must name at least one field of the form.", call. = FALSE)
  }

  fields <- sft_active_input_fields(form)
  ids <- vapply(fields, function(field) field$id, character(1))
  unknown <- setdiff(key, ids)

  if (length(unknown) > 0L) {
    stop(
      "key names fields the form does not have: ",
      paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }

  fields[match(key, ids)]
}

# Stored values of the given fields for one row, as they sit in the database.
# Every key field must be present in the row.
sft_row_key_values <- function(form, fields, row, index) {
  values <- lapply(fields, function(field) {
    if (!sft_record_has_value(row, field)) {
      stop(
        "Row ", index, " has no value for key field '", field$id, "'.",
        call. = FALSE
      )
    }

    sft_field_db_value(field, sft_record_get_value(row, field))
  })

  names(values) <- vapply(fields, function(field) field$db_column, character(1))
  values
}

# WHERE conditions for stored columns equal to `values` (an empty value
# matches NULL, and '' in a text column): list(sql, params). Shared by the
# record lookup and the grid's change stamp.
sft_key_where <- function(conn, form, values) {
  text_columns <- vapply(
    Filter(function(field) identical(toupper(field$db_type %||% "TEXT"), "TEXT"), sft_active_input_fields(form)),
    function(field) field$db_column,
    character(1)
  )

  clauses <- character()
  params <- list()

  for (column in names(values)) {
    quoted <- sft_quote_identifier(conn, column)
    value <- values[[column]]

    if (sft_is_empty_value(value)) {
      # An empty text value is stored as NULL (unique fields, NA) or as ''
      # (text fields), so both count as the empty key. Only for text
      # columns: DuckDB refuses '' for a number, MariaDB reads it as 0.
      clauses <- c(clauses, if (column %in% text_columns) {
        paste0("(", quoted, " IS NULL OR ", quoted, " = '')")
      } else {
        paste0(quoted, " IS NULL")
      })
    } else if (column %in% text_columns && length(value) == 1L && !is.character(value) &&
               is.numeric(value) && is.finite(value)) {
      # A number in a text column is stored as its plain digits (7), but
      # earlier versions stored other spellings ("7.0", "1.0e-05"): match
      # them all.
      spellings <- sft_number_spellings(value)
      clauses <- c(clauses, paste0("(", paste(rep(paste0(quoted, " = ?"), length(spellings)), collapse = " OR "), ")"))
      params <- c(params, as.list(spellings))
    } else if (column %in% text_columns && length(value) == 1L && !is.na(value) &&
               (is.logical(value) || (identical(sft_db_backend(conn), "duckdb") && value %in% c("1", "0")))) {
      # TRUE is stored as "1"; DuckDB stored a flag as "true" before 0.5.4.
      flag <- if (is.logical(value)) value else identical(value, "1")
      clauses <- c(clauses, paste0("(", quoted, " = ? OR ", quoted, " = ?)"))
      params <- c(params, list(if (flag) "1" else "0", if (flag) "true" else "false"))
    } else if (column %in% text_columns && is.character(value) && length(value) == 1L &&
               !is.na(value) && sft_is_decimal_text(value) &&
               identical(sft_number_text(as.numeric(value)), value)) {
      # A number as the key (encoded to its digits) also finds the spellings
      # earlier versions stored. Text that only looks like a number ("007")
      # is not its canonical spelling and matches as it is.
      spellings <- sft_number_spellings(as.numeric(value))
      clauses <- c(clauses, paste0("(", paste(rep(paste0(quoted, " = ?"), length(spellings)), collapse = " OR "), ")"))
      params <- c(params, as.list(spellings))
    } else if (column %in% text_columns && is.character(value) && length(value) == 1L &&
               grepl("^-?[0-9]+$", value)) {
      clauses <- c(clauses, paste0("(", quoted, " = ? OR ", quoted, " = ?)"))
      params <- c(params, list(value, paste0(value, ".0")))
    } else {
      clauses <- c(clauses, paste0(quoted, " = ?"))
      params <- c(params, list(value))
    }
  }

  list(
    sql = if (length(clauses) > 0L) paste(clauses, collapse = " AND ") else "1 = 1",
    params = unname(params)
  )
}

sft_query_with_params <- function(conn, sql, params) {
  if (length(params) > 0L) {
    DBI::dbGetQuery(conn, sql, params = params)
  } else {
    DBI::dbGetQuery(conn, sql)
  }
}

# Live records whose stored columns equal `values`, see sft_key_where().
sft_find_live_records <- function(conn, form, values) {
  where <- sft_key_where(conn, form, values)
  sql <- paste0(
    "SELECT * FROM ", sft_quote_identifier(conn, form$table_name),
    " WHERE sft_is_deleted = 0 AND ", where$sql, " ORDER BY sft_id"
  )

  sft_query_with_params(conn, sql, where$params)
}

# TRUE when the new stored values equal what the row already holds.
sft_row_unchanged <- function(old_record, field_values) {
  for (column in names(field_values)) {
    old <- old_record[[column]][1L]
    new <- field_values[[column]]
    old_empty <- sft_is_empty_value(old)
    new_empty <- sft_is_empty_value(new)

    if (old_empty && new_empty) {
      next
    }

    if (old_empty != new_empty) {
      return(FALSE)
    }

    # Numbers are compared as numbers: a TEXT column hands 12 back as "12.0",
    # and the text of a double is not what the driver returns for it.
    old_number <- suppressWarnings(as.numeric(old))
    new_number <- suppressWarnings(as.numeric(new))

    if (is.numeric(new) && !is.na(old_number) && !is.na(new_number)) {
      if (!identical(old_number, new_number)) {
        return(FALSE)
      }
    } else if (!identical(as.character(old), as.character(new))) {
      return(FALSE)
    }
  }

  TRUE
}

# An unticked checkbox is stored as 0: its input type's empty value, so it
# holds nothing the row would lose.
sft_upsert_stored_empty <- function(field, value) {
  if (sft_is_empty_value(value)) {
    return(TRUE)
  }

  empty <- tryCatch(sft_input_spec(field$input_type)$empty, error = function(e) NULL)
  if (!is.logical(empty) || length(empty) != 1L) {
    return(FALSE)
  }

  decoded <- tryCatch(sft_input_spec(field$input_type)$decode(value), error = function(e) NULL)
  identical(isTRUE(decoded), isTRUE(empty)) && length(decoded) == 1L && !is.na(decoded)
}

# TRUE when the stored record has a non-empty value in an input field that
# is neither a key field nor part of the row.
sft_upsert_holds_more <- function(form, stored, row, key_ids) {
  for (field in sft_active_input_fields(form)) {
    if (field$id %in% key_ids || field$id %in% names(row) || field$db_column %in% names(row)) {
      next
    }

    if (field$db_column %in% names(stored) &&
        !sft_upsert_stored_empty(field, stored[[field$db_column]][1L])) {
      return(TRUE)
    }
  }

  FALSE
}

# Optimistic check of one upsert row against what the caller last saw.
# `expected` names fields (ids) with the values the caller was shown; NA where
# it saw no record or an empty value. Runs inside the transaction, so nobody
# can write between this comparison and the write. `current` is the matching
# live record (zero rows when there is none).
sft_upsert_check_expected <- function(form, expected, current) {
  if (is.null(expected)) {
    return(invisible(TRUE))
  }

  # A single NA: the caller expects NO stored record (it only wants to add).
  if (!is.list(expected) && length(expected) == 1L && is.na(expected)) {
    if (nrow(current) > 0L) {
      stop(sft_edit_conflict_condition(current_record = current, columns = character()))
    }
    return(invisible(TRUE))
  }

  fields <- sft_key_fields(form, names(expected))
  columns <- character()

  for (i in seq_along(fields)) {
    field <- fields[[i]]
    stored <- if (nrow(current) > 0L) current[[field$db_column]][1L]
    seen <- sft_field_db_value(field, expected[[i]])

    if (sft_values_differ(stored, seen)) {
      columns <- c(columns, field$db_column)
    }
  }

  if (length(columns) > 0L) {
    stop(sft_edit_conflict_condition(
      current_record = if (nrow(current) > 0L) current,
      columns = columns
    ))
  }

  invisible(TRUE)
}

# Serialise upserts of one table on MariaDB. The key has no unique index, and
# under REPEATABLE READ two upserts of the same NEW group both find no stored
# row and both insert (measured on 11.8: duplicates whenever two writers
# started within ~100 ms). Every later upsert of that group then fails as
# ambiguous. A named lock per database and table makes the second writer wait;
# it is taken BEFORE the transaction, so the second writer's snapshot already
# holds what the first committed and its rows become updates. Per table rather
# than per scope, so calls with and without `scope` exclude each other too.
# SQLite serialises writers itself and DuckDB is single-process: no-op there.
# GET_LOCK names are limited to 64 characters, hence the hash. Returns a
# function that releases the lock. Internal.
sft_upsert_lock <- function(conn, form, timeout = 10) {
  if (!sft_is_mariadb_connection(conn)) {
    return(function() invisible(NULL))
  }

  name_sql <- "CONCAT('sft_upsert:', LEFT(SHA1(CONCAT(DATABASE(), '.', ?)), 40))"
  got <- DBI::dbGetQuery(
    conn,
    paste0("SELECT GET_LOCK(", name_sql, ", ?) AS got"),
    params = list(form$table_name, timeout)
  )$got

  if (!isTRUE(as.integer(got) == 1L)) {
    stop(structure(
      class = c("sft_write_conflict", "error", "condition"),
      list(
        message = paste0(
          "Someone else is saving to the same table, and the write waited ",
          timeout, " seconds without getting through. Nothing was changed - ",
          "please save again."
        ),
        call = NULL
      )
    ))
  }

  function() {
    try(
      DBI::dbGetQuery(
        conn,
        paste0("SELECT RELEASE_LOCK(", name_sql, ") AS released"),
        params = list(form$table_name)
      ),
      silent = TRUE
    )
    invisible(NULL)
  }
}

# Re-raise an error from one row with the row named, keeping its class (a
# validation error stays a validation error) so callers can still handle it.
sft_upsert_row_error <- function(condition, index, key_values) {
  key_text <- paste(
    names(key_values),
    vapply(key_values, function(value) as.character(value)[1L] %||% "", character(1)),
    sep = " = ", collapse = ", "
  )
  condition$row_message <- conditionMessage(condition)
  condition$message <- paste0("Row ", index, " (", key_text, "): ", conditionMessage(condition))
  condition$call <- NULL
  condition$row <- index
  condition
}

#' Write several records in one transaction, matched by a key
#'
#' Inserts, updates or soft-deletes the records of a form so that the stored
#' rows reflect `records`, all inside one transaction: either every row is
#' written or none is. Each row is matched to a live stored record by the
#' `key` fields. A row with no match is inserted; a row whose match already
#' holds the same values is left alone (no write, no audit entry); any other
#' match is updated. Every write goes through the same validation and audit
#' log as [insert_record()], [update_record()] and [soft_delete_record()].
#'
#' `empty` and `scope` make the function fit a grid of counts, where rows are
#' shown for every product or category but only what was actually entered
#' should be stored: `empty` says which rows count as empty (they are
#' soft-deleted if stored, and skipped otherwise; a stored record that also
#' holds values in fields the row does not carry is not deleted but updated
#' with the row's values, so a 0 the `empty` rule ignores is stored as 0),
#' and `scope` names the group
#' the records describe completely, so that stored rows of that group missing
#' from `records` are soft-deleted as well.
#'
#' On MariaDB, calls for the same table wait for each other (a named lock,
#' at most 10 seconds, then an `sft_write_conflict` error), so two users who
#' save the same new group at the same moment cannot both insert it; the
#' second one updates what the first stored. SQLite serialises writers itself.
#'
#' @param form Object created with [form()].
#' @param records A data frame, or a list of named lists, with one element per
#'   record. Names are field ids (or database columns). Fields not present in
#'   a row are left untouched on update and empty on insert.
#' @param key Character vector of field ids that identify a record, for
#'   example `c("warehouse", "product")`. Every row must carry them. The stored
#'   rows are expected to be unique per key; an ambiguous match is an error.
#' @param conn Optional DBI connection; see [connections].
#' @param user Optional user identifier for the audit log.
#' @param reason Optional reason for the audit log.
#' @param empty Optional function of a row's values without the key fields,
#'   returning `TRUE` when the row is empty, or a logical vector with one
#'   element per record. Default `NULL`: no row is empty.
#'   Example for a grid of counts: `function(values) all(unlist(values) %in%
#'   c(0, NA))`.
#' @param scope Optional named list of field values that describe the group
#'   `records` covers completely, for example `list(warehouse = "1")`. Live
#'   records matching `scope` that this call did not write are soft-deleted.
#'   The scope fields must be part of `key`.
#' @param expected Optional list with one element per record: `NULL` for no
#'   check, or a named list of field values the caller last saw for that
#'   record (`NA` where it saw no record or an empty value). A row whose
#'   stored values differ, because someone else wrote them in the meantime,
#'   stops the whole call with an error of class `"sft_edit_conflict"` that
#'   names the row. Nothing is written then. A single `NA` instead of a list
#'   means the caller expects no stored record for the row (it may only add).
#'
#' @return Invisibly, a data frame with one row per record (and one per scope
#'   deletion): `row` (the index in `records`, `NA` for scope deletions),
#'   `action` (`"insert"`, `"update"`, `"unchanged"`, `"delete"` or `"skip"`)
#'   and `sft_id` (`NA` for a skipped row).
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' stock <- form(
#'   form_id = "stock", table_name = "stock", db = db,
#'   fields = list(
#'     form_field(id = "warehouse", label = "Warehouse"),
#'     form_field(id = "product", label = "Product"),
#'     form_field(id = "count", label = "Count", input_type = "numericInput")
#'   )
#' )
#' counts <- data.frame(
#'   warehouse = "1", product = c("A", "B", "C"), count = c(412, 388, 0)
#' )
#' result <- upsert_records(
#'   stock, counts, key = c("warehouse", "product"),
#'   empty = function(values) all(unlist(values) %in% c(0, NA)),
#'   scope = list(warehouse = "1")
#' )
#' result$action
#' fetch_records(stock)[, c("product", "count")]
#' @export
upsert_records <- function(form,
                           records,
                           key,
                           conn = NULL,
                           user = NULL,
                           reason = NULL,
                           empty = NULL,
                           scope = NULL,
                           expected = NULL) {
  conn <- sft_prepare_mutation(form, conn, user = user)
  rows <- sft_upsert_rows(records)
  key_fields <- sft_key_fields(form, key)
  key_ids <- vapply(key_fields, function(field) field$id, character(1))

  if (!is.null(empty) && !is.function(empty) &&
      !(is.logical(empty) && length(empty) == length(rows) && !anyNA(empty))) {
    stop("empty must be NULL, a function, or a logical vector with one element per record.", call. = FALSE)
  }

  if (!is.null(scope) && (!is.list(scope) || is.null(names(scope)) || !all(nzchar(names(scope))))) {
    stop("scope must be NULL or a named list of field values.", call. = FALSE)
  }

  if (!is.null(expected) && (!is.list(expected) || length(expected) != length(rows))) {
    stop("expected must be NULL or a list with one element per record.", call. = FALSE)
  }

  scope_fields <- if (!is.null(scope)) sft_key_fields(form, names(scope))

  # A row is found by its key alone; a scope field outside the key would let a
  # row match (and move) a record of another group.
  outside <- setdiff(
    vapply(scope_fields, function(field) field$id, character(1)),
    key_ids
  )

  if (length(outside) > 0L) {
    stop(
      "scope fields must be part of key: ", paste(outside, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Released before a self-opened connection is closed (on.exit is LIFO here);
  # inside with_transaction() only when that block ends, so no other writer
  # gets in before it has committed.
  release <- sft_upsert_lock(conn, form)
  on.exit(sft_after_transaction(conn, release), add = TRUE, after = FALSE)

  # invisible() outside: the transaction returns its value visibly.
  result <- sft_db_with_transaction(conn, {
    results <- vector("list", length(rows))
    touched <- integer()

    for (index in seq_along(rows)) {
      row <- rows[[index]]
      key_values <- sft_row_key_values(form, key_fields, row, index)

      results[[index]] <- tryCatch(
        {
          matches <- sft_find_live_records(conn, form, key_values)

          if (nrow(matches) > 1L) {
            stop(
              nrow(matches), " live records share this key; ",
              "upsert_records needs the key to identify one record.",
              call. = FALSE
            )
          }

          sft_upsert_check_expected(form, expected[[index]], matches)

          is_empty <- if (is.logical(empty)) {
            empty[[index]]
          } else {
            !is.null(empty) &&
              isTRUE(empty(row[setdiff(names(row), c(key_ids, names(key_values)))]))
          }

          # An empty row whose record holds something outside the row (a
          # remark someone filled in the form) is emptied, not deleted: the
          # caller only speaks for the fields it sends.
          if (is_empty && nrow(matches) > 0L && sft_upsert_holds_more(form, matches, row, key_ids)) {
            is_empty <- FALSE
          }

          if (is_empty) {
            if (nrow(matches) == 0L) {
              list(action = "skip", sft_id = NA_integer_)
            } else {
              deleted <- sft_soft_delete_record_tx(
                conn, form, record_id = matches$sft_id[1L], user = user, reason = reason
              )
              list(action = "delete", sft_id = deleted$sft_id[1L])
            }
          } else if (nrow(matches) == 0L) {
            inserted <- sft_insert_record_tx(conn, form, row, user = user, reason = reason)
            list(action = "insert", sft_id = inserted$sft_id[1L])
          } else {
            # Only what the update may write counts: a value for a locked
            # field is dropped by the update, so it must not make the row
            # look changed (an audit row with no changed field on every call).
            field_values <- sft_record_field_values(
              sft_resolve_editable(form, user), row,
              include_missing = FALSE, editable_only = TRUE
            )

            if (sft_row_unchanged(matches, field_values)) {
              list(action = "unchanged", sft_id = matches$sft_id[1L])
            } else {
              updated <- sft_update_record_tx(
                conn, form, row, record_id = matches$sft_id[1L], user = user, reason = reason
              )
              list(action = "update", sft_id = updated$sft_id[1L])
            }
          }
        },
        error = function(e) stop(sft_upsert_row_error(e, index, key_values))
      )

      if (!is.na(results[[index]]$sft_id)) {
        touched <- c(touched, as.integer(results[[index]]$sft_id))
      }
    }

    if (!is.null(scope)) {
      scope_values <- lapply(seq_along(scope_fields), function(i) {
        sft_field_db_value(scope_fields[[i]], scope[[i]])
      })
      names(scope_values) <- vapply(scope_fields, function(field) field$db_column, character(1))

      stored <- sft_find_live_records(conn, form, scope_values)

      # By record id, not by comparing key values in R: the database decided
      # which rows matched, and its idea of equality (collation, "12" vs
      # "12.0") is the one that counts.
      for (i in seq_len(nrow(stored))) {
        if (!as.integer(stored$sft_id[i]) %in% touched) {
          deleted <- sft_soft_delete_record_tx(
            conn, form, record_id = stored$sft_id[i], user = user, reason = reason
          )
          results[[length(results) + 1L]] <- list(
            action = "delete", sft_id = deleted$sft_id[1L], row = NA_integer_
          )
        }
      }
    }

    invisible(data.frame(
      row = vapply(seq_along(results), function(i) {
        if (is.null(results[[i]]$row)) i else results[[i]]$row
      }, integer(1)),
      action = vapply(results, function(r) r$action, character(1)),
      sft_id = vapply(results, function(r) as.integer(r$sft_id), integer(1)),
      stringsAsFactors = FALSE
    ))
  })

  invisible(result)
}
