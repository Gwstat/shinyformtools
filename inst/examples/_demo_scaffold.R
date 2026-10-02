# Walkthrough scaffolding shared by the bundled example apps.
#
# NOTHING in this file is part of the shinyformtools API. It only draws the
# two-column page each example is presented in: the running app on the right,
# and on the left a tabset with
#
#   - "How it is built": the example's own #> STEP / #> NOTE / #> END blocks,
#     rendered as numbered cards.
#   - "Complete code": the example file itself, minus the three lines that pull
#     this scaffolding in (its #> DEMO ... #> DEMO END region) and minus the #>
#     markers. What that tab shows is therefore the whole, runnable app - copy
#     it into an app.R and it runs as-is.
#
# An example uses it in its last lines, after its real `ui` and `server` exist:
#
#   #> DEMO
#   source(example_path("_demo_scaffold"), local = TRUE)
#   ui <- demo_page(ui, "app_name")
#   #> DEMO END
#
#   shinyApp(ui, server)
#
# The leading underscore keeps it out of list_examples() / run_example().

# --- Reading the example source ----------------------------------------------

# Resolve the file actually being run (dev vs installed can diverge), so the
# walkthrough always reflects THIS source. Works under source(), Shiny
# sourceUTF8 (parse+eval) and Rscript; falls back to the bundled copy by name.
demo_self_path <- function(example) {
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) {
      path <- normalizePath(of, winslash = "/", mustWork = FALSE)
      if (!identical(basename(path), "_demo_scaffold.R")) return(path)
    }
  }
  for (i in seq_len(sys.nframe())) {
    sr <- attr(sys.call(i), "srcref")
    sf <- if (!is.null(sr)) attr(sr, "srcfile") else NULL
    if (!is.null(sf) && !is.null(sf$filename) && nzchar(sf$filename)) {
      path <- normalizePath(sf$filename, winslash = "/", mustWork = FALSE)
      if (!identical(basename(path), "_demo_scaffold.R")) return(path)
    }
  }
  example_path(example)
}

demo_source <- function(example) {
  path <- demo_self_path(example)
  if (is.null(path) || !file.exists(path)) path <- example_path(example)
  readLines(path, warn = FALSE)
}

# --- "How it is built": the #> STEP blocks as numbered cards ------------------

demo_steps <- function(lines) {
  out <- list()
  cur <- NULL
  push <- function() {
    if (is.null(cur)) return(invisible())
    code <- cur$code
    while (length(code) && !nzchar(trimws(code[1]))) code <- code[-1]
    while (length(code) && !nzchar(trimws(code[length(code)]))) code <- code[-length(code)]
    cur$code <- code
    out[[length(out) + 1L]] <<- cur
    cur <<- NULL
  }
  for (ln in lines) {
    title <- sub("^#>\\s*STEP:\\s*", "", ln)
    if (!identical(title, ln)) {
      push()
      cur <- list(title = trimws(title), notes = character(), code = character())
      next
    }
    if (grepl("^#>\\s*END\\s*$", ln)) {
      push()
      next
    }
    note <- sub("^#>\\s*NOTE:\\s*", "", ln)
    if (!identical(note, ln) && !is.null(cur)) {
      cur$notes <- c(cur$notes, trimws(note))
      next
    }
    if (!is.null(cur)) cur$code <- c(cur$code, ln)
  }
  push()
  out
}

demo_card <- function(header, body) {
  shiny::div(
    style = "margin-bottom:1rem;border:1px solid #e1e4e8;border-radius:6px;overflow:hidden;background:#fff;",
    shiny::div(
      style = "padding:0.5rem 0.75rem;background:#f1f5fb;border-bottom:1px solid #e1e4e8;",
      header
    ),
    body
  )
}

demo_pre <- function(code) {
  shiny::tags$pre(
    style = "margin:0.5rem 0 0;padding:0.6rem 0.75rem;background:#f7f7f7;font-size:0.8rem;line-height:1.35;overflow:auto;",
    paste(code, collapse = "\n")
  )
}

how_built <- function(lines) {
  steps <- tryCatch(demo_steps(lines), error = function(e) list())
  libs <- lines[startsWith(lines, "library(")]
  launch <- lines[startsWith(lines, "shinyApp(") | startsWith(lines, "shiny::shinyApp(")]
  if (length(libs)) {
    steps <- c(list(list(title = "Load the libraries", notes = character(), code = libs)), steps)
  }
  if (length(launch)) {
    steps <- c(steps, list(list(title = "Run the app", notes = character(), code = launch)))
  }
  shiny::tagList(lapply(seq_along(steps), function(i) {
    st <- steps[[i]]
    demo_card(
      shiny::tags$strong(paste0(i, ". ", st$title)),
      shiny::tagList(
        if (length(st$notes)) {
          shiny::tags$p(
            style = "margin:0;padding:0.5rem 0.75rem 0;color:#586069;font-size:0.85rem;",
            paste(st$notes, collapse = " ")
          )
        },
        demo_pre(st$code)
      )
    )
  }))
}

# --- "Complete code": the example file, minus this scaffolding ----------------

# Drop the #> DEMO ... #> DEMO END region (the lines that source this file and
# wrap the app in the demo page) and every #> marker line, then squeeze the
# blank lines that leaves behind. What remains is the app, in full.
demo_code <- function(lines) {
  keep <- rep(TRUE, length(lines))
  in_demo <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^#>\\s*DEMO\\s*$", lines[i])) in_demo <- TRUE
    if (in_demo || grepl("^#>", lines[i])) keep[i] <- FALSE
    if (grepl("^#>\\s*DEMO END\\s*$", lines[i])) in_demo <- FALSE
  }
  code <- lines[keep]

  blank <- !nzchar(trimws(code))
  code <- code[!(blank & c(FALSE, utils::head(blank, -1)))]
  while (length(code) && !nzchar(trimws(code[length(code)]))) code <- code[-length(code)]
  code
}

demo_full_code <- function(lines) {
  code <- paste(demo_code(lines), collapse = "\n")
  demo_card(
    shiny::tagList(
      shiny::tags$strong("The whole app"),
      shiny::tags$button(
        "Copy",
        class = "btn btn-default btn-xs",
        style = "float:right;",
        onclick = paste0(
          "var p=this.closest('div').parentNode.querySelector('pre');",
          "navigator.clipboard.writeText(p.innerText);",
          "var b=this;b.innerHTML='Copied';",
          "setTimeout(function(){b.innerHTML='Copy';},1200);"
        )
      )
    ),
    demo_pre(code)
  )
}

# --- The demo page ------------------------------------------------------------

# `app_ui` is the example's real UI, shown untouched in the right-hand column.
demo_page <- function(app_ui, example) {
  lines <- demo_source(example)
  pane <- function(content) {
    shiny::div(style = "height:82vh;overflow:auto;padding:0.75rem 0.4rem 0 0;", content)
  }
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::tabsetPanel(
          shiny::tabPanel("How it is built", pane(how_built(lines))),
          shiny::tabPanel("Complete code", pane(demo_full_code(lines)))
        )
      ),
      shiny::column(6, app_ui)
    )
  )
}
