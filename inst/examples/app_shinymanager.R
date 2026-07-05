# Per-user CRUD permissions across several tables, managed from a rights table.
#
# A small support-desk app: a Tickets table and an Agents table. shinymanager
# only logs people in (users + an admin flag - no permission columns). All rights
# live in ONE editable table built with permissions_form(): each row is a rule
# granting a set of permissions to a user across one or more forms (the "forms"
# field is a multi-select). Adding another table never adds a column - it just
# becomes another choice in that multi-select.
#
#   - rights_permissions(rules, user, form_id) turns the rules into the can_*
#     functions form_server() expects, resolved per user and per form. Because
#     the rules are an sft form, editing them is reactive and (with the default
#     form_server(hide_forbidden = TRUE)) updates the visible buttons live.
#   - Admins are superusers: they see every table and manage the rights table in
#     an admin-only "Permissions" tab. shinymanager's own admin page is not
#     extensible, so the rights manager lives in the secured app itself; drop the
#     same form_ui()/form_server() into a standalone admin app if you prefer.
#
# The Permissions tab also carries a short guide to what each can_* column
# grants, so you can see every permission's effect right where you set it: tick
# a column in a rule, then log in as that user to watch the matching control
# appear or disappear (form_server(hide_forbidden = TRUE)).
#
# Logins (user / password): admin / admin, editor / editor, viewer / viewer.
#
# Run with: shinyformtools::run_example("app_shinymanager")

library(shiny)
library(shinyformtools)

if (!requireNamespace("shinymanager", quietly = TRUE)) {
  stop(
    "The 'app_shinymanager' example needs the 'shinymanager' package. ",
    "install.packages('shinymanager').",
    call. = FALSE
  )
}

form_ids <- c("tickets", "agents")

# --- Login only: users + admin flag, no permission columns -------------------
initial_credentials <- data.frame(
  user = c("admin", "editor", "viewer"),
  password = c("admin", "editor", "viewer"),
  admin = c(TRUE, FALSE, FALSE),
  name = c("Administrator", "Editor", "Viewer"),
  stringsAsFactors = FALSE
)

credentials_db <- tempfile(fileext = ".sqlite")
credentials_passphrase <- "shinyformtools-demo-passphrase"
shinymanager::create_db(
  credentials_data = initial_credentials,
  sqlite_path = credentials_db,
  passphrase = credentials_passphrase
)

# --- Application database: two data tables + the rights table ----------------
db_path <- tempfile(fileext = ".sqlite")

#> STEP: Describe the data forms
#> NOTE: Two ordinary sft forms - Tickets and Agents. Neither knows anything
#> NOTE: about users or permissions; access is layered on later by the rights
#> NOTE: table, so adding a form never means adding permission columns.
tickets_form <- form(
  form_id = "tickets", form_name = "Tickets", table_name = "tickets",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "subject", label = "Subject", mandatory = TRUE, col = 1, pos = 1),
    form_field(
      id = "status", label = "Status", input_type = "selectInput",
      args = list(choices = c("New", "In progress", "Resolved", "Closed"), selected = "New"),
      col = 1, pos = 2
    ),
    form_field(
      id = "priority", label = "Priority", input_type = "selectInput",
      args = list(choices = c("Low", "Medium", "High", "Urgent"), selected = "Medium"),
      col = 2, pos = 1
    ),
    form_field(
      id = "description", label = "Description", input_type = "textAreaInput",
      args = list(value = "", rows = 3), col = 2, pos = 2
    )
  )
)

agents_form <- form(
  form_id = "agents", form_name = "Agents", table_name = "agents",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "name", label = "Name", mandatory = TRUE, col = 1, pos = 1),
    form_field(id = "email", label = "Email", mandatory = TRUE, unique = TRUE, col = 1, pos = 2),
    form_field(
      id = "team", label = "Team", input_type = "selectInput",
      args = list(choices = c("Frontline", "Escalations", "On-call"), selected = "Frontline"),
      col = 2, pos = 1
    )
  )
)
#> END

#> STEP: Build the rights table
#> NOTE: permissions_form() makes the rights table itself an sft form: each row
#> NOTE: grants a set of can_* permissions to one user across a multi-select of
#> NOTE: forms. User choices come from the credential database via
#> NOTE: shinymanager_users(); form choices are the ids above. Adding a table is
#> NOTE: just another choice in that multi-select - never a new column.
rights_form <- permissions_form(
  form_ids = form_ids,
  db = db_sqlite(db_path),
  users = shinymanager_users(initial_credentials)
)
#> END

# --- Seed data + a couple of rules once --------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  for (f in list(tickets_form, agents_form, rights_form)) {
    init_db(f, conn = conn, user = "system")
  }

  if (nrow(fetch_records(agents_form, conn = conn)) == 0L) {
    insert_record(agents_form, list(name = "Ada Lovelace", email = "ada@example.org", team = "Escalations"),
                  conn = conn, user = "system")
    insert_record(agents_form, list(name = "Grace Hopper", email = "grace@example.org", team = "Frontline"),
                  conn = conn, user = "system")
  }
  if (nrow(fetch_records(tickets_form, conn = conn)) == 0L) {
    insert_record(tickets_form, list(subject = "Login page returns 500", status = "New",
                  priority = "Urgent", description = ""),
                  conn = conn, user = "system")
    insert_record(tickets_form, list(subject = "Export to CSV is slow", status = "In progress",
                  priority = "Medium", description = ""),
                  conn = conn, user = "system")
  }
  if (nrow(fetch_records(rights_form, conn = conn)) == 0L) {
    # editor: full CRUD on Tickets, view-only on Agents.
    insert_record(rights_form, list(user = "editor", forms = "tickets",
                  can_add = TRUE, can_edit = TRUE, can_view_record = TRUE, can_view_versions = TRUE),
                  conn = conn, user = "system")
    insert_record(rights_form, list(user = "editor", forms = "agents", can_view_record = TRUE),
                  conn = conn, user = "system")
    # viewer: open records on both tables, switch column views, nothing else.
    insert_record(rights_form, list(user = "viewer", forms = c("tickets", "agents"),
                  can_view_record = TRUE, can_select_column_view = TRUE),
                  conn = conn, user = "system")
  }
})

# Show the multi-form "forms" cell as a readable list instead of raw JSON.
pretty_forms <- function(data, context) {
  data$forms <- vapply(data$forms, function(v) {
    paste(tryCatch(jsonlite::fromJSON(v), error = function(e) v), collapse = ", ")
  }, character(1))
  data
}

# What each permission column on the rights table grants. These are exactly the
# can_* fields permissions_form() creates; the guide documents their effect right
# next to where an admin ticks them (folded in from the former standalone
# permission playground).
permission_guide <- list(
  can_add                    = "Create new records (the Add button).",
  can_view_record            = "Open a record to view or edit it.",
  can_edit                   = "Save edits to an opened record.",
  can_delete                 = "Soft-delete records.",
  can_restore                = "Restore a deleted record or a previous version.",
  can_view_versions          = "See a record's previous versions.",
  can_view_deleted_records   = "See the list of deleted records.",
  can_change_column_settings = "Save a new preset column order.",
  can_select_column_view     = "Switch column order by loading a saved view.",
  can_view_audit             = "See the audit log of changes."
)

# Render the guide as a collapsible reference so it never crowds the rights table.
permission_guide_ui <- function(guide) {
  shiny::tags$details(
    style = "margin-bottom: 0.75rem;",
    shiny::tags$summary(shiny::tags$strong("What each permission grants")),
    shiny::tags$dl(
      style = "margin: 0.5rem 0 0;",
      lapply(names(guide), function(id) {
        shiny::tagList(
          shiny::tags$dt(shiny::tags$code(id)),
          shiny::tags$dd(style = "margin: 0 0 0.4rem 1rem;", guide[[id]])
        )
      })
    )
  )
}

# --- "How it is built" demo scaffolding (not part of the form API) -----------
# Self-contained: renders this file's own #> STEP / #> NOTE / #> END blocks as
# numbered cards beside the running app. Only the form()/form_server() code
# above is shinyformtools; everything in this block just draws the demo page.
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

# --- App ----------------------------------------------------------------------
# shinymanager::secure_app() owns a full-screen login overlay we cannot split,
# so the "How it is built" / app layout lives INSIDE the secured UI: once logged
# in, the numbered step cards show on the left and the running app on the right.
ui <- shinymanager::secure_app(
  shiny::fluidPage(
    shiny::titlePanel("shinyformtools + shinymanager: a rights table"),
    shiny::p(
      "A small support desk. Permissions come from an editable rights table, not ",
      "per-table credential columns. Log in as admin / editor / viewer ",
      "(password = user name). Admins manage the rights in the Permissions tab; ",
      "everyone else sees only what a rule grants them."
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h4("How it is built"),
        shiny::div(
          style = "height:80vh;overflow:auto;padding-right:0.4rem;",
          how_built("app_shinymanager")
        )
      ),
      shiny::column(
        6,
        shiny::h4("App"),
        shiny::tabsetPanel(
          shiny::tabPanel("Tickets", form_ui("tickets", title = "Tickets", show_user = FALSE, button_options = neutral_buttons)),
          shiny::tabPanel("Agents", form_ui("agents", title = "Agents", show_user = FALSE, button_options = neutral_buttons)),
          shiny::tabPanel("Permissions", shiny::uiOutput("permissions_panel"))
        )
      )
    )
  ),
  # Shown on shinymanager's login screen (above the fields) so the demo logins
  # are discoverable. tags_top / tags_bottom pass through to auth_ui().
  tags_top = shiny::tags$div(
    style = "text-align: left; margin-bottom: 1rem;",
    shiny::tags$strong("Demo logins (user / password):"),
    shiny::tags$ul(
      style = "margin: 0.25rem 0 0; padding-left: 1.2rem;",
      shiny::tags$li("admin / admin", shiny::tags$em("(superuser, manages rights)")),
      shiny::tags$li("editor / editor"),
      shiny::tags$li("viewer / viewer")
    )
  ),
  enable_admin = TRUE
)

#> STEP: Resolve permissions per user and wire the forms
#> NOTE: shinymanager only logs people in. The rights table's records drive
#> NOTE: every form's permissions: rights_permissions(rules, user, form_id)
#> NOTE: resolves the matching rules into the can_* functions form_server()
#> NOTE: expects, per user and per form. Admins are superusers (see everything).
#> NOTE: Because the rules are a reactive sft form, edits apply live and, with
#> NOTE: form_server(hide_forbidden = TRUE), the visible controls follow them.
server <- function(input, output, session) {
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      credentials_db, passphrase = credentials_passphrase
    )
  )

  conn <- db_connect(db_sqlite(db_path))
  session$onSessionEnded(function() db_disconnect(conn))

  current_user <- function() {
    u <- res_auth$user
    if (is.null(u) || length(u) == 0L || is.na(u[[1L]]) || !nzchar(as.character(u[[1L]]))) "unknown" else as.character(u[[1L]])
  }
  is_admin <- function() isTRUE(as.logical(res_auth$admin))

  # The rights table: only admins may edit it. Its records drive the can_*
  # permissions of every other form, reactively.
  rights <- form_server(
    "permissions", rights_form, conn = conn, user = current_user,
    show_audit = FALSE,
    display_transform = pretty_forms,
    table_columns = c("sft_easy_id", "user", "forms",
                      "can_add", "can_edit", "can_delete", "can_view_record"),
    can_add = is_admin, can_edit = is_admin, can_delete = is_admin,
    can_view_record = is_admin, can_view_versions = is_admin,
    can_view_deleted_records = is_admin, can_restore = is_admin,
    persist_column_settings = FALSE
  )

  # Only show the rights manager to admins. The guide above the table explains
  # what each permission column grants; tick one in a rule, then log in as that
  # user to see the matching control appear or disappear.
  output$permissions_panel <- shiny::renderUI({
    if (is_admin()) {
      shiny::tagList(
        shiny::p(
          "Each row grants a set of permissions to a user across the chosen ",
          "forms. Tick a permission column to grant that action; untick to ",
          "revoke. Changes apply live - log in as editor or viewer in another ",
          "session to watch the buttons follow the rules."
        ),
        permission_guide_ui(permission_guide),
        form_ui("permissions", title = "Rights table", show_user = FALSE,
                button_options = neutral_buttons)
      )
    } else {
      shiny::p(
        "Administrators only. Your permissions are set by the rights table; ",
        "you see only the actions a rule grants you."
      )
    }
  })

  # Each data form's permissions are resolved from the rights table for the
  # current user and that form's id. Admins are superusers (see everything).
  for (fid in form_ids) {
    f <- if (fid == "tickets") tickets_form else agents_form
    cols <- if (fid == "tickets") {
      c("sft_easy_id", "subject", "status", "priority", "sft_updated_at")
    } else {
      c("sft_easy_id", "name", "email", "team", "sft_updated_at")
    }
    perms <- rights_permissions(rights$records, user = current_user,
                                form_id = fid, superuser = is_admin)
    do.call(form_server, c(
      list(id = fid, form = f, conn = conn, table_columns = cols), perms
    ))
  }
}
#> END

shinyApp(ui, server)
