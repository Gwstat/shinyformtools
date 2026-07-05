# Field-level Markdown rendering for table cells.
#
# A field declared with markdown = TRUE has its stored value rendered as Markdown
# in the records and versions tables. Values are entered by users, so rendering
# them as raw HTML would be a stored-XSS hole. The pipeline is therefore
# escape -> render -> URL-scrub:
#
#   1. Escape &, <, > in the source first. commonmark renders the resulting
#      entities as inert literal text, so no raw HTML can survive, while Markdown
#      syntax (**, -, []( ), #, `) is untouched.
#   2. Render with commonmark::markdown_html().
#   3. Scrub href/src whose scheme is javascript:/data:/vbscript:/file: (the only
#      links commonmark can emit, since raw HTML was already escaped) down to "#".
#
# The corresponding table columns are excluded from DT's escaping (see
# records_datatable / sft_versions_datatable) so the rendered HTML is shown.

# Database column names of a form's markdown input fields.
sft_markdown_columns <- function(form) {
  fields <- Filter(
    function(field) isTRUE(field$markdown),
    sft_active_input_fields(form)
  )

  vapply(fields, function(field) field$db_column, character(1))
}

sft_escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Neutralize dangerous URL schemes in commonmark-generated markup. Because raw
# HTML in the source was already escaped, the only href/src attributes present are
# those commonmark emitted from Markdown links/images, so a targeted regex is safe
# here. Allowed schemes (http, https, mailto, relative) are left untouched.
sft_scrub_unsafe_urls <- function(html) {
  gsub(
    "(href|src)\\s*=\\s*\"\\s*(?:javascript|data|vbscript|file)\\s*:[^\"]*\"",
    "\\1=\"#\"",
    html,
    ignore.case = TRUE,
    perl = TRUE
  )
}

sft_markdown_to_safe_html <- function(text) {
  escaped <- sft_escape_html(text)
  html <- commonmark::markdown_html(escaped)
  sft_scrub_unsafe_urls(html)
}

# Render a character vector of Markdown source to sanitized HTML. NA and empty
# values become "". Errors with an install hint if commonmark is not available.
sft_render_markdown <- function(x) {
  if (!requireNamespace("commonmark", quietly = TRUE)) {
    stop(
      "Markdown fields require the 'commonmark' package. ",
      "Install it with install.packages(\"commonmark\").",
      call. = FALSE
    )
  }

  x <- as.character(x)

  vapply(
    x,
    function(value) {
      if (is.na(value) || !nzchar(value)) {
        return("")
      }

      sft_markdown_to_safe_html(value)
    },
    character(1),
    USE.NAMES = FALSE
  )
}
