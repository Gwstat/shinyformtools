# The same declarative form, backed by a MariaDB / MySQL server.
#
# Switching to MariaDB is a one-line change: db = db_mariadb(...) instead of
# db_sqlite(). Unlike the file-based backends, MariaDB needs a running server,
# so this example shows an in-app setup tutorial (with a copy-paste Docker
# command) and a step-by-step walkthrough of the source instead of crashing
# when it cannot connect.
#
# Quick start (full tutorial is shown in the app):
#   1. install.packages("RMariaDB")
#   2. Start a server:
#        docker run --name shinyformtools-mariadb -p 3306:3306 \
#          -e MARIADB_DATABASE=shinyformtools \
#          -e MARIADB_USER=sft -e MARIADB_PASSWORD=sft \
#          -e MARIADB_ROOT_PASSWORD=root -d mariadb:11
#   3. shinyformtools::run_example("app_mariadb")
#
# The connection defaults to that server (user sft / password sft on
# 127.0.0.1:3306, database shinyformtools). Override any of it with the
# SFT_MARIADB_USER / _PASSWORD / _DB / _HOST / _PORT environment variables.

library(shiny)
library(shinyformtools)

# --- "How it is built" demo scaffolding (not part of the form API) -----------
# Self-contained: renders this file's own #> STEP / #> NOTE / #> END blocks as
# numbered cards beside the running app. Only the form()/form_server() code
# below is shinyformtools; everything in this block just draws the demo page.
# (Defined up front because the UI is assembled before the page is built.)
neutral_buttons <- list(
  button_classes = list(
    open_add = "btn-default", open_edit = "btn-default", delete = "btn-default",
    open_deleted_records = "btn-default", open_column_selection = "btn-default"
  )
)

demo_steps <- function(path) {
  lines <- readLines(path, warn = FALSE)
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

# Resolve the file actually being run (dev vs installed can diverge), so the
# walkthrough always reflects THIS source. Works under source(), Shiny
# sourceUTF8 (parse+eval) and Rscript; falls back to the bundled copy by name.
demo_self_path <- function() {
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) {
      return(normalizePath(of, winslash = "/", mustWork = FALSE))
    }
  }
  for (i in seq_len(sys.nframe())) {
    sr <- attr(sys.call(i), "srcref")
    sf <- if (!is.null(sr)) attr(sr, "srcfile") else NULL
    if (!is.null(sf) && !is.null(sf$filename) && nzchar(sf$filename)) {
      return(normalizePath(sf$filename, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

how_built <- function(example) {
  path <- demo_self_path()
  if (is.null(path) || !file.exists(path)) path <- example_path(example)
  steps <- tryCatch(demo_steps(path), error = function(e) list())
  src <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  libs <- src[startsWith(src, "library(")]
  launch <- src[startsWith(src, "shinyApp(") | startsWith(src, "shiny::shinyApp(")]
  if (length(libs)) {
    steps <- c(list(list(title = "Load the libraries", notes = character(), code = libs)), steps)
  }
  if (length(launch)) {
    steps <- c(steps, list(list(title = "Run the app", notes = character(), code = launch)))
  }
  shiny::tagList(lapply(seq_along(steps), function(i) {
    st <- steps[[i]]
    shiny::div(
      style = "margin-bottom:1rem;border:1px solid #e1e4e8;border-radius:6px;overflow:hidden;background:#fff;",
      shiny::div(
        style = "padding:0.5rem 0.75rem;background:#f1f5fb;border-bottom:1px solid #e1e4e8;",
        shiny::tags$strong(paste0(i, ". ", st$title))
      ),
      if (length(st$notes)) {
        shiny::tags$p(
          style = "margin:0;padding:0.5rem 0.75rem 0;color:#586069;font-size:0.85rem;",
          paste(st$notes, collapse = " ")
        )
      },
      shiny::tags$pre(
        style = "margin:0.5rem 0 0;padding:0.6rem 0.75rem;background:#f7f7f7;font-size:0.8rem;line-height:1.35;overflow:auto;",
        paste(st$code, collapse = "\n")
      )
    )
  }))
}

demo_page <- function(title, example, app_ui) {
  shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::fluidRow(
      shiny::column(
        6, shiny::h4("How it is built"),
        shiny::div(style = "height:80vh;overflow:auto;padding-right:0.4rem;", how_built(example))
      ),
      shiny::column(6, shiny::h4("App"), app_ui)
    )
  )
}

#> STEP: Configure the connection
#> NOTE: db_mariadb() just describes the server; nothing connects yet. The
#> NOTE: defaults below match the Docker command in the setup tutorial.
maria_db <- db_mariadb(
  dbname = Sys.getenv("SFT_MARIADB_DB", unset = "shinyformtools"),
  host = Sys.getenv("SFT_MARIADB_HOST", unset = "127.0.0.1"),
  port = as.integer(Sys.getenv("SFT_MARIADB_PORT", unset = "3306")),
  user = Sys.getenv("SFT_MARIADB_USER", unset = "sft"),
  password = Sys.getenv("SFT_MARIADB_PASSWORD", unset = "sft")
)
#> END

#> STEP: Describe the form
#> NOTE: One form() description drives the database schema, the add/edit
#> NOTE: dialogs, the records table, soft-delete and the audit log.
employees_form <- form(
  form_id = "employees",
  form_name = "Employees",
  table_name = "employees",
  db = maria_db,
  fields = list(
    form_field(id = "staff_no", label = "Staff no.", mandatory = TRUE, unique = TRUE, col = 1, pos = 1),
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 2),
    form_field(
      id = "department", label = "Department", input_type = "selectInput",
      args = list(choices = c("Engineering", "Sales", "Operations"), selected = "Engineering"),
      col = 1, pos = 3
    ),
    form_field(
      id = "salary", label = "Salary", input_type = "numericInput",
      args = list(value = 50000, min = 0, max = 1e6), col = 2, pos = 1
    ),
    form_field(
      id = "full_time", label = "Full time", input_type = "checkboxInput",
      args = list(value = TRUE), col = 2, pos = 2
    ),
    form_field(
      id = "hired", label = "Hired", input_type = "dateInput",
      args = list(value = Sys.Date()), col = 2, pos = 3
    )
  )
)
#> END

#> STEP: Connect, create the schema, seed demo rows
#> NOTE: init_db() derives and migrates the table from the form. We wrap it in
#> NOTE: tryCatch so a missing server shows the setup tutorial instead of a crash.
connection_error <- NULL

if (!requireNamespace("RMariaDB", quietly = TRUE)) {
  connection_error <- "The 'RMariaDB' package is not installed. Run install.packages('RMariaDB')."
} else {
  connection_error <- tryCatch(
    {
      conn <- db_connect(maria_db)
      on.exit(db_disconnect(conn), add = TRUE)
      init_db(employees_form, conn = conn, user = "demo")

      if (nrow(fetch_records(employees_form, conn = conn)) == 0L) {
        insert_record(employees_form, list(staff_no = "E-001", name = "Ada Lovelace",
                      department = "Engineering", salary = 95000, full_time = TRUE),
                      conn = conn, user = "demo")
        insert_record(employees_form, list(staff_no = "E-002", name = "Grace Hopper",
                      department = "Operations", salary = 88000, full_time = TRUE),
                      conn = conn, user = "demo")
      }

      NULL
    },
    error = function(e) conditionMessage(e)
  )
}
#> END

#> STEP: Wire the Shiny UI
#> NOTE: form_ui() renders the table + buttons for the form id "employees".
employees_form_ui <- form_ui(
  id = "employees",
  title = "Employees",
  show_user = FALSE,
  button_options = neutral_buttons
)
#> END

#> STEP: Wire the server
#> NOTE: form_server() with the matching id drives all CRUD against MariaDB.
employees_form_server <- function() {
  form_server(
    id = "employees",
    form = employees_form,
    user = "demo",
    table_columns = c("sft_easy_id", "staff_no", "name", "department", "salary", "full_time"),
    persist_column_settings = FALSE
  )
}
#> END

# --- Demo-page scaffolding (not part of the tutorial) ------------------------
# Everything below renders this side-by-side demo page using the shared helper
# sourced above, plus the MariaDB-specific setup tutorial. None of it is needed
# to use shinyformtools.

# A self-contained "how to set up MariaDB" tutorial shown in the app.
mariadb_setup_tutorial <- function() {
  code_block <- function(...) {
    shiny::tags$pre(
      style = paste(
        "margin: 0.3rem 0 0; padding: 0.5rem 0.6rem; background: #2b2b2b;",
        "color: #f0f0f0; border-radius: 4px; font-size: 0.8rem;",
        "white-space: pre-wrap; word-break: break-word;"
      ),
      ...
    )
  }
  step <- function(n, title, ...) {
    shiny::tags$li(
      style = "margin-bottom: 0.7rem;",
      shiny::tags$b(title),
      ...
    )
  }

  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border: 1px solid #d0d7de; border-radius: 6px; background: #fbfcfe;"
    ),
    shiny::tags$strong("Set up MariaDB in three steps"),
    shiny::tags$ol(
      style = "margin: 0.5rem 0 0 1.1rem; padding: 0;",
      step(
        1, "Install the R driver.",
        code_block("install.packages(\"RMariaDB\")")
      ),
      step(
        2, "Start a server with Docker.",
        " One command creates the database and the sft user this app expects:",
        code_block(
          "docker run --name shinyformtools-mariadb -p 3306:3306 \\\n",
          "  -e MARIADB_DATABASE=shinyformtools \\\n",
          "  -e MARIADB_USER=sft -e MARIADB_PASSWORD=sft \\\n",
          "  -e MARIADB_ROOT_PASSWORD=root -d mariadb:11"
        ),
        shiny::tags$div(
          style = "margin-top: 0.3rem; color: #586069; font-size: 0.82rem;",
          "No Docker? Point the app at any MariaDB/MySQL server via the ",
          shiny::tags$code("SFT_MARIADB_*"), " environment variables."
        )
      ),
      step(
        3, "Reload this app.",
        " Once the server is accepting connections, restart with ",
        shiny::tags$code("shinyformtools::run_example(\"app_mariadb\")"),
        ". It connects automatically (user sft / password sft)."
      )
    )
  )
}

connected_intro <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("Connected to MariaDB"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "The same declarative form you have seen on the file backends, now ",
      "persisting to a MariaDB server. Add, edit and delete employees; the ",
      "unique staff-number check, soft-delete with restore, and the audit log ",
      "all run against the server."
    )
  )
}

setup_panel <- function(message) {
  shiny::tagList(
    shiny::div(
      style = paste(
        "margin-bottom: 1rem; padding: 0.75rem 1rem;",
        "border-left: 4px solid #c0392b; background: #fdecea; border-radius: 4px;"
      ),
      shiny::tags$strong("MariaDB is not available yet"),
      shiny::tags$p(
        style = "margin: 0.4rem 0 0;",
        "The form is fully defined (see the steps on the left); it just needs ",
        "a server to talk to. Follow the tutorial below, then reload."
      ),
      shiny::tags$details(
        style = "margin-top: 0.5rem;",
        shiny::tags$summary(
          style = "cursor: pointer; color: #586069; font-size: 0.85rem;",
          "Underlying connection error"
        ),
        shiny::tags$pre(
          style = "margin-top: 0.4rem; white-space: pre-wrap; font-size: 0.8rem;",
          message
        )
      )
    ),
    mariadb_setup_tutorial()
  )
}

app_ui <- if (is.null(connection_error)) {
  shiny::tagList(connected_intro(), employees_form_ui)
} else {
  setup_panel(connection_error)
}

ui <- demo_page("Backend: MariaDB", "app_mariadb", app_ui)

server <- function(input, output, session) {
  if (is.null(connection_error)) {
    employees_form_server()
  }
}

shinyApp(ui, server)
