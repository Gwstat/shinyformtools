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

# Live records whose stored columns equal `values` (an NA matches NULL).
sft_find_live_records <- function(conn, form, values) {
  clauses <- vapply(
    names(values),
    function(column) {
      quoted <- sft_quote_identifier(conn, column)

      if (sft_is_empty_value(values[[column]])) {
        paste0(quoted, " IS NULL")
      } else {
        paste0(quoted, " = ?")
      }
    },
    character(1)
  )

  params <- Filter(function(value) !sft_is_empty_value(value), values)

  sql <- paste0(
    "SELECT * FROM ",
    sft_quote_identifier(conn, form$table_name),
    " WHERE sft_is_deleted = 0",
    if (length(clauses) > 0L) paste0(" AND ", paste(clauses, collapse = " AND ")),
    " ORDER BY sft_id"
  )

  if (length(params) > 0L) {
    DBI::dbGetQuery(conn, sql, params = unname(params))
  } else {
    DBI::dbGetQuery(conn, sql)
  }
}

# One string per key, so that stored rows and incoming rows can be matched.
sft_key_signature <- function(values) {
  paste(
    vapply(
      values,
      function(value) if (sft_is_empty_value(value)) "" else as.character(value)[1L],
      character(1)
    ),
    collapse = "\x1f"
  )
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

    if (old_empty != new_empty || !identical(as.character(old), as.character(new))) {
      return(FALSE)
    }
  }

  TRUE
}

# Re-raise an error from one row with the row named, keeping its class (a
# validation error stays a validation error) so callers can still handle it.
sft_upsert_row_error <- function(condition, index, key_values) {
  key_text <- paste(
    names(key_values),
    vapply(key_values, function(value) as.character(value)[1L] %||% "", character(1)),
    sep = " = ", collapse = ", "
  )
  condition$message <- paste0("Row ", index, " (", key_text, "): ", conditionMessage(condition))
  condition$call <- NULL
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
#' shown for every party or category but only what was actually entered
#' should be stored: `empty` says which rows count as empty (they are
#' soft-deleted if stored, and skipped otherwise), and `scope` names the group
#' the records describe completely, so that stored rows of that group missing
#' from `records` are soft-deleted as well.
#'
#' @param form Object created with [form()].
#' @param records A data frame, or a list of named lists, with one element per
#'   record. Names are field ids (or database columns). Fields not present in
#'   a row are left untouched on update and empty on insert.
#' @param key Character vector of field ids that identify a record, for
#'   example `c("district", "party")`. Every row must carry them. The stored
#'   rows are expected to be unique per key; an ambiguous match is an error.
#' @param conn Optional DBI connection; see [connections].
#' @param user Optional user identifier for the audit log.
#' @param reason Optional reason for the audit log.
#' @param empty Optional function of a row's values without the key fields,
#'   returning `TRUE` when the row is empty. Default `NULL`: no row is empty.
#'   Example for a grid of counts: `function(values) all(unlist(values) %in%
#'   c(0, NA))`.
#' @param scope Optional named list of field values that describe the group
#'   `records` covers completely, for example `list(district = "1")`. Live
#'   records matching `scope` whose key is not among `records` are
#'   soft-deleted.
#'
#' @return Invisibly, a data frame with one row per record (and one per scope
#'   deletion): `row` (the index in `records`, `NA` for scope deletions),
#'   `action` (`"insert"`, `"update"`, `"unchanged"`, `"delete"` or `"skip"`)
#'   and `sft_id` (`NA` for a skipped row).
#' @examples
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' votes <- form(
#'   form_id = "votes", table_name = "votes", db = db,
#'   fields = list(
#'     form_field(id = "district", label = "District"),
#'     form_field(id = "party", label = "Party"),
#'     form_field(id = "count", label = "Count", input_type = "numericInput")
#'   )
#' )
#' counts <- data.frame(
#'   district = "1", party = c("A", "B", "C"), count = c(412, 388, 0)
#' )
#' result <- upsert_records(
#'   votes, counts, key = c("district", "party"),
#'   empty = function(values) all(unlist(values) %in% c(0, NA)),
#'   scope = list(district = "1")
#' )
#' result$action
#' fetch_records(votes)[, c("party", "count")]
#' @export
upsert_records <- function(form,
                           records,
                           key,
                           conn = NULL,
                           user = NULL,
                           reason = NULL,
                           empty = NULL,
                           scope = NULL) {
  conn <- sft_prepare_mutation(form, conn, user = user)
  rows <- sft_upsert_rows(records)
  key_fields <- sft_key_fields(form, key)
  key_ids <- vapply(key_fields, function(field) field$id, character(1))

  if (!is.null(empty) && !is.function(empty)) {
    stop("empty must be NULL or a function.", call. = FALSE)
  }

  if (!is.null(scope) && (!is.list(scope) || is.null(names(scope)) || !all(nzchar(names(scope))))) {
    stop("scope must be NULL or a named list of field values.", call. = FALSE)
  }

  scope_fields <- if (!is.null(scope)) sft_key_fields(form, names(scope))

  sft_db_with_transaction(conn, {
    results <- vector("list", length(rows))
    seen <- character()

    for (index in seq_along(rows)) {
      row <- rows[[index]]
      key_values <- sft_row_key_values(form, key_fields, row, index)
      seen <- c(seen, sft_key_signature(key_values))

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

          is_empty <- !is.null(empty) &&
            isTRUE(empty(row[setdiff(names(row), c(key_ids, names(key_values)))]))

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
            field_values <- sft_record_field_values(form, row, include_missing = FALSE)

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
    }

    if (!is.null(scope)) {
      scope_values <- lapply(seq_along(scope_fields), function(i) {
        sft_field_db_value(scope_fields[[i]], scope[[i]])
      })
      names(scope_values) <- vapply(scope_fields, function(field) field$db_column, character(1))

      stored <- sft_find_live_records(conn, form, scope_values)
      key_columns <- vapply(key_fields, function(field) field$db_column, character(1))

      for (i in seq_len(nrow(stored))) {
        signature <- sft_key_signature(as.list(stored[i, key_columns, drop = FALSE]))

        if (!signature %in% seen) {
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
}
