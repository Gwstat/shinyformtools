# IBAN input and validation helpers.

sft_normalize_iban <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }

  value <- as.character(value[[1L]])

  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }

  toupper(gsub("[^A-Za-z0-9]", "", value))
}

sft_format_iban <- function(value) {
  value <- sft_normalize_iban(value)

  if (is.na(value) || !nzchar(value)) {
    return("")
  }

  parts <- substring(
    value,
    seq(1L, nchar(value), by = 4L),
    pmin(seq(4L, nchar(value) + 3L, by = 4L), nchar(value))
  )

  paste(parts, collapse = " ")
}

sft_iban_mod97 <- function(value) {
  value <- sft_normalize_iban(value)

  if (is.na(value) || nchar(value) < 4L) {
    return(NA_integer_)
  }

  rearranged <- paste0(substr(value, 5L, nchar(value)), substr(value, 1L, 4L))
  chars <- strsplit(rearranged, "", fixed = TRUE)[[1L]]
  remainder <- 0L

  for (char in chars) {
    code <- utf8ToInt(char)

    digits <- if (code >= utf8ToInt("A") && code <= utf8ToInt("Z")) {
      as.character(code - utf8ToInt("A") + 10L)
    } else {
      char
    }

    if (!grepl("^[0-9]+$", digits)) {
      return(NA_integer_)
    }

    for (digit in strsplit(digits, "", fixed = TRUE)[[1L]]) {
      remainder <- (remainder * 10L + as.integer(digit)) %% 97L
    }
  }

  remainder
}

#' Validate an IBAN string
#'
#' Checks the basic IBAN structure and the ISO 13616 mod-97 checksum. When
#' `DE = TRUE`, the value must be a German IBAN with country code `DE` and 22
#' characters. Whitespace in `value` is ignored.
#'
#' @param value IBAN value.
#' @param DE Logical. If `TRUE`, require a German IBAN.
#'
#' @return Logical scalar.
#' @examples
#' # A valid German IBAN passes both the pattern and the mod-97 checksum.
#' is_valid_iban("DE89370400440532013000")
#'
#' # An invalid checksum fails.
#' is_valid_iban("DE00000000000000000000")
#'
#' # Whitespace is ignored.
#' is_valid_iban("DE89 3704 0044 0532 0130 00")
#'
#' # Accept IBANs from any country with DE = FALSE.
#' is_valid_iban("GB82WEST12345698765432", DE = FALSE)
#' @export
is_valid_iban <- function(value, DE = TRUE) {
  value <- sft_normalize_iban(value)

  if (is.na(value) || !nzchar(value)) {
    return(FALSE)
  }

  pattern_ok <- if (isTRUE(DE)) {
    grepl("^DE[0-9]{20}$", value)
  } else {
    grepl("^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$", value) &&
      nchar(value) >= 15L &&
      nchar(value) <= 34L
  }

  isTRUE(pattern_ok) && isTRUE(sft_iban_mod97(value) == 1L)
}

#' Input field for IBAN numbers
#'
#' Creates a Shiny text input with browser-side IBAN formatting and validation.
#' The value is formatted with spaces every four characters and invalid entries
#' are highlighted. Use server-side validation rules for production checks.
#'
#' @param inputId Input id.
#' @param label Input label.
#' @param value Initial value.
#' @param DE Logical. If `TRUE`, require a German IBAN client-side.
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   IBANInput("iban", "Bank account (IBAN)", value = "DE89370400440532013000")
#' )
#' server <- function(input, output, session) {
#'   observe(print(input$iban))
#' }
#' shinyApp(ui, server)
#' }
#' @export
IBANInput <- function(inputId, label, value = "", DE = TRUE) {
  input_id_json <- jsonlite::toJSON(inputId, auto_unbox = TRUE)
  de_json <- jsonlite::toJSON(isTRUE(DE), auto_unbox = TRUE)

  shiny::tagList(
    # A plain <style> (not tags$head): the IBAN input is usually rendered inside
    # a modal, and showModal() -> processDeps() keeps only $html/$deps and drops
    # the extracted <head>, which would silently swallow these rules. A bare
    # style tag rides along in $html and still applies document-wide.
    shiny::tags$style(shiny::HTML(
      "
      .sft-invalid-iban {
        border-color: #dc3545 !important;
        box-shadow: 0 0 0 0.15rem rgba(220, 53, 69, 0.25) !important;
      }
      .sft-valid-iban {
        border-color: #198754 !important;
        box-shadow: 0 0 0 0.15rem rgba(25, 135, 84, 0.20) !important;
      }
      "
    )),
    shiny::textInput(
      inputId = inputId,
      label = label,
      value = sft_format_iban(value)
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "
      (function() {
        var inputId = %s;
        var germanOnly = %s;

        function normalize(value) {
          return (value || '').replace(/\\s+/g, '').replace(/[^a-zA-Z0-9]/g, '').toUpperCase();
        }

        function format(value) {
          value = normalize(value);
          return value.replace(/(.{4})/g, '$1 ').trim();
        }

        function mod97(iban) {
          var rearranged = iban.slice(4) + iban.slice(0, 4);
          var remainder = 0;
          for (var i = 0; i < rearranged.length; i++) {
            var ch = rearranged.charAt(i);
            var digits;
            if (/[A-Z]/.test(ch)) {
              digits = String(ch.charCodeAt(0) - 55);
            } else if (/[0-9]/.test(ch)) {
              digits = ch;
            } else {
              return NaN;
            }
            for (var j = 0; j < digits.length; j++) {
              remainder = (remainder * 10 + Number(digits.charAt(j))) %% 97;
            }
          }
          return remainder;
        }

        function isValid(value) {
          var iban = normalize(value);
          if (!iban) return false;
          var patternOk = germanOnly ? /^DE[0-9]{20}$/.test(iban) : /^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$/.test(iban);
          if (!patternOk || iban.length < 15 || iban.length > 34) return false;
          return mod97(iban) === 1;
        }

        function bind() {
          var input = document.getElementById(inputId);
          if (!input || input.dataset.sftIbanBound === 'true') return;
          input.dataset.sftIbanBound = 'true';

          function applyValidity(el, doFormat) {
            if (doFormat) el.value = format(el.value);
            if (isValid(el.value)) {
              el.classList.remove('sft-invalid-iban');
              el.classList.add('sft-valid-iban');
            } else if (normalize(el.value).length > 0) {
              el.classList.remove('sft-valid-iban');
              el.classList.add('sft-invalid-iban');
            } else {
              el.classList.remove('sft-valid-iban');
              el.classList.remove('sft-invalid-iban');
            }
          }

          input.addEventListener('input', function(e) { applyValidity(e.target, true); });

          // Colour a prefilled value immediately, not only once the user types.
          applyValidity(input, false);
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', bind);
        } else {
          bind();
        }
      })();
      ",
      input_id_json,
      de_json
    )))
  )
}

#' Update an IBAN input field
#'
#' @param session Shiny session.
#' @param inputId Input id.
#' @param label Optional new label.
#' @param value Optional new value.
#'
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   IBANInput("iban", "IBAN"),
#'   actionButton("fill", "Insert example IBAN")
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$fill, {
#'     updateIBANInput(session, "iban", value = "DE89370400440532013000")
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
updateIBANInput <- function(session, inputId, label = NULL, value = NULL) {
  shiny::updateTextInput(
    session = session,
    inputId = inputId,
    label = label,
    value = if (is.null(value)) NULL else sft_format_iban(value)
  )
}
