#' Create choices from a referenced records table
#'
#' Builds a named choices vector from a data frame, typically from another
#' `form_server()` return value such as `persons$records()`. The stored
#' choice values should be stable reference ids, for example `sft_easy_id` or
#' `sft_id`; labels can combine one main label column with optional extra
#' display columns such as phone numbers.
#'
#' @param data Data frame or function returning a data frame.
#' @param value Column used as stored reference value. Defaults to
#'   `"sft_easy_id"`.
#' @param label Optional column used as user-facing label. If `NULL`, `value` is
#'   used as label.
#' @param extra Optional character vector of additional columns appended to the
#'   label when non-empty.
#' @param include_empty Logical. Whether to prepend an empty choice.
#' @param empty_label Label for the empty choice.
#' @param label_sep Separator between the main label and extra label parts.
#'
#' @return A named character vector suitable for Shiny choice arguments.
#' @examples
#' people <- data.frame(
#'   sft_easy_id = c("p1", "p2"),
#'   name = c("Ada Lovelace", "Alan Turing"),
#'   phone = c("123", "456"),
#'   stringsAsFactors = FALSE
#' )
#' reference_choices(people, value = "sft_easy_id", label = "name")
#' reference_choices(people, label = "name", extra = "phone")
#' @export
reference_choices <- function(data,
                                  value = "sft_easy_id",
                                  label = NULL,
                                  extra = NULL,
                                  include_empty = FALSE,
                                  empty_label = "",
                                  label_sep = " \u00b7 ") {
  if (is.function(data)) {
    data <- data()
  }

  if (is.null(data)) {
    data <- data.frame()
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame or a function returning a data frame.", call. = FALSE)
  }

  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("value must be a non-empty character scalar.", call. = FALSE)
  }

  if (!value %in% names(data)) {
    stop("Reference value column not found: ", value, ".", call. = FALSE)
  }

  if (!is.null(label)) {
    if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
      stop("label must be NULL or a non-empty character scalar.", call. = FALSE)
    }

    if (!label %in% names(data)) {
      stop("Reference label column not found: ", label, ".", call. = FALSE)
    }
  }

  if (is.null(extra)) {
    extra <- character()
  }

  if (!is.character(extra) || any(is.na(extra)) || any(!nzchar(extra))) {
    stop("extra must be NULL or a character vector of column names.", call. = FALSE)
  }

  missing_extra <- setdiff(extra, names(data))
  if (length(missing_extra) > 0L) {
    stop(
      "Reference extra column(s) not found: ",
      paste(missing_extra, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.logical(include_empty) || length(include_empty) != 1L || is.na(include_empty)) {
    stop("include_empty must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(empty_label) || length(empty_label) != 1L || is.na(empty_label)) {
    stop("empty_label must be a character scalar.", call. = FALSE)
  }

  if (!is.character(label_sep) || length(label_sep) != 1L || is.na(label_sep)) {
    stop("label_sep must be a character scalar.", call. = FALSE)
  }

  values <- as.character(data[[value]])
  keep <- !is.na(values) & nzchar(values)

  if (length(values) == 0L || !any(keep)) {
    out <- character()
  } else {
    values <- values[keep]

    labels <- if (is.null(label)) {
      values
    } else {
      as.character(data[[label]][keep])
    }

    labels[is.na(labels) | !nzchar(labels)] <- values[is.na(labels) | !nzchar(labels)]

    if (length(extra) > 0L) {
      extra_labels <- apply(
        data[keep, extra, drop = FALSE],
        1L,
        function(row) {
          row <- as.character(row)
          row <- row[!is.na(row) & nzchar(row)]
          paste(row, collapse = label_sep)
        }
      )

      has_extra <- nzchar(extra_labels)
      labels[has_extra] <- paste(labels[has_extra], extra_labels[has_extra], sep = label_sep)
    }

    out <- stats::setNames(values, labels)
  }

  if (isTRUE(include_empty)) {
    out <- c(stats::setNames("", empty_label), out)
  }

  out
}


