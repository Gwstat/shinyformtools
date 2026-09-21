# One language object for all user-facing text.
#
# The package has four vocabularies: UI labels (buttons, dialogs,
# notifications), validation messages, table labels (system-column headers,
# audit actions, Yes/No, changelog words) and the DataTables chrome. Each used
# to have its own override route, and the last two only a process-global
# option - so an app could be German or English, but not one per session.
#
# A `language()` bundles the four. Passed to form_ui() / form_server() it is
# SCOPED, not global: while the module renders a table, runs a save or builds a
# dialog, sft_with_language() makes it the active language, and the four
# readers (sft_ui_labels, sft_form_messages, sft_table_labels, the DT options)
# consult the active language first. Layering, weakest to strongest:
#   English default < global option (use_german) < language < explicit argument
# Nothing is threaded through the render functions, and sessions in one R
# process can speak different languages.

.sft_language <- new.env(parent = emptyenv())

sft_active_language <- function() {
  .sft_language$current
}

# The active language's entries for one vocabulary, or an empty list.
sft_language_part <- function(part) {
  language <- sft_active_language()

  if (is.null(language)) {
    return(list())
  }

  language[[part]] %||% list()
}

# Evaluate `expr` with `language` active. Restores the previous language on
# exit, so nested scopes and early returns inside `expr` are safe. A NULL
# language leaves whatever is active in place.
sft_with_language <- function(language, expr) {
  language <- sft_resolve_language(language)

  if (is.null(language)) {
    return(expr)
  }

  previous <- .sft_language$current
  .sft_language$current <- language
  on.exit(.sft_language$current <- previous, add = TRUE)

  expr
}

# `language` may be the object itself or a function / reactive returning one,
# which is what lets an app switch the language while it runs. Called inside a
# render, a reactive takes a dependency here, so the output follows a switch.
sft_resolve_language <- function(language) {
  if (is.function(language)) {
    language <- language()
  }

  if (is.null(language) || inherits(language, "sft_language")) {
    return(language)
  }

  stop(
    "language must be NULL, an object created with language() / german() / ",
    "english(), or a function returning one.",
    call. = FALSE
  )
}

# Argument check at call time. A function is accepted unevaluated: a reactive
# cannot be read outside a reactive context, so it is checked when first used.
sft_check_language <- function(language) {
  if (is.function(language)) {
    return(invisible(language))
  }

  invisible(sft_resolve_language(language))
}

#' Bundle the user-facing text of a form into one language
#'
#' All text the package shows comes from four vocabularies: `labels` (buttons,
#' dialog titles, notifications), `messages` (validation messages),
#' `table_labels` (system-column headers, audit actions, Yes/No, changelog
#' words) and `dt_language` (the 'DataTables' search box, pagination and info
#' line). `language()` bundles overrides for all four so they can be passed to
#' [form_ui()] and [form_server()] as one argument. Unlike [use_german()], which
#' switches the whole R process, a language given to a form applies to that form
#' only - so two sessions of one app can speak different languages.
#'
#' Every entry is optional: what a language does not name falls back to the
#' global option (if any) and then to the English default. [language_keys()]
#' lists every key with its default text. A `labels` or `messages` argument
#' given to a function directly still wins over the language.
#'
#' @param labels,messages,table_labels Named lists overriding entries of the
#'   respective vocabulary. Text may contain `{placeholders}` exactly like the
#'   defaults do.
#' @param dt_language Named list passed to 'DataTables' as its `language`
#'   option.
#'
#' @section Switching the language while the app runs:
#' `form_server(language = )` also accepts a function or reactive that returns
#' a language, for example `reactive(if (input$lang == "de") german() else
#' english())`. Tables, dialogs, notifications and validation messages then
#' follow it at once. The buttons and titles drawn by [form_ui()] are static
#' HTML: call `form_ui()` inside a `renderUI()` that reads the same reactive so
#' that they are redrawn too. A dialog that is already open keeps its language.
#'
#' @return An object of class `sft_language`. `german()` returns the bundled
#'   German pack as one such object. `english()` returns the English defaults
#'   spelled out in full, which is what it takes to pin one form to English while
#'   [use_german()] is active for the rest of the process.
#' @seealso [language_keys()], [use_german()]
#' @examples
#' # Start from German and rename one button.
#' de <- german()
#' de$labels$open_add <- "Neuer Kontakt"
#'
#' # A small language of your own: everything else stays English.
#' short <- language(
#'   labels = list(open_add = "New", save = "OK"),
#'   messages = list(unique = "'{label}' exists already.")
#' )
#'
#' \dontrun{
#' ui <- fluidPage(form_ui("contacts", language = de))
#' server <- function(input, output, session) {
#'   form_server("contacts", contacts_form, language = de)
#' }
#' }
#' @export
language <- function(labels = list(),
                     messages = list(),
                     table_labels = list(),
                     dt_language = list()) {
  parts <- list(
    labels = labels,
    messages = messages,
    table_labels = table_labels,
    dt_language = dt_language
  )

  for (name in names(parts)) {
    part <- parts[[name]]

    if (!is.list(part)) {
      stop(name, " must be a named list.", call. = FALSE)
    }

    if (length(part) > 0L && (is.null(names(part)) || any(!nzchar(names(part))))) {
      stop("Every entry of ", name, " must be named.", call. = FALSE)
    }
  }

  # A misspelt key would silently do nothing, so name it.
  known <- list(
    labels = names(sft_default_ui_labels()),
    messages = names(sft_default_messages()),
    table_labels = names(sft_default_table_labels())
  )

  for (name in names(known)) {
    unknown <- setdiff(names(parts[[name]]), known[[name]])

    if (length(unknown) > 0L) {
      stop(
        "Unknown ", name, " key", if (length(unknown) > 1L) "s" else "", ": ",
        paste(unknown, collapse = ", "),
        ". See language_keys() for the valid ones.",
        call. = FALSE
      )
    }
  }

  structure(parts, class = "sft_language")
}

#' @rdname language
#' @export
german <- function() {
  language(
    labels = german_labels(),
    messages = german_messages(),
    table_labels = german_table_labels(),
    dt_language = german_dt_language()
  )
}

#' @rdname language
#' @export
english <- function() {
  language(
    labels = sft_default_ui_labels(),
    messages = sft_default_messages(),
    table_labels = sft_default_table_labels(),
    dt_language = list()
  )
}

#' @export
print.sft_language <- function(x, ...) {
  cat("<shinyformtools language>\n")

  for (name in names(x)) {
    cat(sprintf("  %-13s %d entr%s\n", name, length(x[[name]]), if (length(x[[name]]) == 1L) "y" else "ies"))
  }

  invisible(x)
}

#' List every text key with its English default
#'
#' The reference for translating the package or renaming a single button: one
#' row per key, for the three keyed vocabularies of a [language()]. (The fourth,
#' `dt_language`, is 'DataTables' own `language` option and documented there.)
#'
#' @return A data frame with the columns `vocabulary` (`"labels"`,
#'   `"messages"` or `"table_labels"`), `key` and `default` (the English text;
#'   `NA` for a key whose default is "not shown"). Placeholders such as
#'   `{label}` are filled in by the package.
#' @examples
#' keys <- language_keys()
#' head(keys[keys$vocabulary == "messages", ])
#' table(keys$vocabulary)
#' @export
language_keys <- function() {
  vocabularies <- list(
    labels = sft_default_ui_labels(),
    messages = sft_default_messages(),
    table_labels = sft_default_table_labels()
  )

  rows <- lapply(names(vocabularies), function(name) {
    entries <- vocabularies[[name]]

    data.frame(
      vocabulary = name,
      key = names(entries),
      default = vapply(
        entries,
        function(entry) if (is.null(entry)) NA_character_ else as.character(entry)[1],
        character(1)
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  do.call(rbind, rows)
}
