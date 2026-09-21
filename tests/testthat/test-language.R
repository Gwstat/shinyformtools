# One language() object carries all user-facing text of a form, and it is
# SCOPED to that form: two sessions in one R process can speak different
# languages, which the process-global use_german() could never do.

sft_test_language_form <- function(db_path = tempfile(fileext = ".sqlite")) {
  form(
    form_id = "lang",
    table_name = "lang",
    db = db_sqlite(db_path),
    fields = list(form_field(id = "name", label = "Name", mandatory = TRUE))
  )
}

test_that("language() validates its vocabularies and names unknown keys", {
  expect_s3_class(language(), "sft_language")
  expect_s3_class(german(), "sft_language")

  expect_error(language(labels = "Speichern"), "labels must be a named list")
  expect_error(language(labels = list("Speichern")), "must be named")
  expect_error(language(labels = list(sav = "OK")), "Unknown labels key: sav")
  expect_error(language(messages = list(uniqe = "x")), "Unknown messages key")
  expect_error(form_ui("x", language = list(labels = list())), "language must be NULL")
})

test_that("language_keys lists every key of the three keyed vocabularies", {
  keys <- language_keys()

  expect_identical(names(keys), c("vocabulary", "key", "default"))
  expect_setequal(unique(keys$vocabulary), c("labels", "messages", "table_labels"))
  expect_false(any(duplicated(keys[, c("vocabulary", "key")])))
  expect_setequal(keys$key[keys$vocabulary == "messages"], names(sft_default_messages()))

  # The German pack translates everything except the one key whose default is
  # "do not show".
  expect_identical(setdiff(names(sft_default_ui_labels()), names(german_labels())), "open_versions")
  expect_length(setdiff(names(sft_default_messages()), names(german_messages())), 0L)
  expect_length(setdiff(names(sft_default_table_labels()), names(german_table_labels())), 0L)
})

test_that("a language is active only inside its scope, and scopes nest", {
  expect_null(sft_active_language())
  expect_identical(sft_ui_labels()$save, "Save")

  de <- german()
  inside <- sft_with_language(de, {
    inner <- sft_with_language(language(labels = list(save = "OK")), sft_ui_labels()$save)
    c(inner = inner, outer = sft_ui_labels()$save)
  })

  expect_identical(inside[["inner"]], "OK")
  expect_identical(inside[["outer"]], "Speichern")
  expect_null(sft_active_language())

  # An early exit restores the scope too.
  try(sft_with_language(de, stop("boom")), silent = TRUE)
  expect_null(sft_active_language())
})

test_that("layering: default < global option < language < explicit argument", {
  withr::local_options(shinyformtools.labels = list(save = "GLOBAL", cancel = "GLOBAL"))

  scoped <- sft_with_language(
    language(labels = list(save = "LANGUAGE")),
    sft_ui_labels(list(cancel = "EXPLICIT"))
  )

  expect_identical(scoped$save, "LANGUAGE")
  expect_identical(scoped$cancel, "EXPLICIT")
  expect_identical(scoped$open_add, sft_default_ui_labels()$open_add)
})

test_that("english() pins a form to English while the process is German", {
  old <- use_german()
  withr::defer(options(old))

  expect_identical(sft_ui_labels()$save, "Speichern")

  pinned <- sft_with_language(english(), list(
    save = sft_ui_labels()$save,
    unique = sft_form_messages()$unique,
    header = sft_table_labels()$audit_changed_at,
    dt = sft_dt_options()$language
  ))

  expect_identical(pinned$save, "Save")
  expect_match(pinned$unique, "already taken")
  expect_identical(pinned$header, "Timestamp")
  expect_null(pinned$dt)
})

test_that("form_ui renders the language, and explicit labels still win", {
  html <- as.character(form_ui("lang", language = german()))
  expect_match(html, "Eintrag hinzuf", fixed = TRUE)

  html <- as.character(form_ui("lang", language = german(), labels = list(open_add = "Neu")))
  expect_match(html, ">Neu<", fixed = TRUE)
  expect_false(grepl("Eintrag hinzuf", html, fixed = TRUE))
})

test_that("the fallback tab name and the changelog follow the language", {
  tabbed <- form(
    form_id = "tabs", table_name = "tabs", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "a", label = "A", tab = 0),
      form_field(id = "b", label = "B", tab = 1)
    )
  )

  english_ui <- as.character(render_form_fields(tabbed))
  german_ui <- sft_with_language(german(), as.character(render_form_fields(tabbed)))

  expect_match(english_ui, "Tab 2", fixed = TRUE)
  expect_match(german_ui, "Reiter 2", fixed = TRUE)

  many <- "[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]"
  expect_match(sft_compact_changed_fields(many), "+2 more", fixed = TRUE)
  expect_match(sft_with_language(german(), sft_compact_changed_fields(many)), "+2 weitere", fixed = TRUE)
})

test_that("two sessions in one process speak different languages", {
  skip_if_not_installed("DT")

  shown <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown <<- c(shown, as.character(ui))
      invisible(NULL)
    },
    .package = "shiny"
  )

  reject_a_save <- function(language) {
    shown <<- character()
    f <- sft_test_language_form()

    shiny::testServer(
      form_server,
      args = list(id = "lang", form = f, language = language),
      {
        session$flushReact()
        session$setInputs(add_name = "")
        session$setInputs(submit_add = 1)
      }
    )

    shown
  }

  german_session <- reject_a_save(german())
  expect_true(any(grepl("Pflichtfelder fehlen: name", german_session, fixed = TRUE)))

  # The very next session, same process, no language: English, nothing leaked.
  english_session <- reject_a_save(NULL)
  expect_true(any(grepl("Mandatory fields missing: name", english_session, fixed = TRUE)))
  expect_false(any(grepl("Pflichtfelder", english_session, fixed = TRUE)))
  expect_null(sft_active_language())
})

test_that("the records and audit tables render their headers in the form's language", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_language_form(db_path)
  conn <- local_test_conn(db_path)
  init_db(f, conn = conn)
  insert_record(f, list(name = "Ada"), conn = conn, user = "alice")

  headers <- function(language) {
    out <- NULL
    shiny::testServer(
      form_server,
      args = list(id = "lang", form = f, conn = conn, language = language),
      {
        session$flushReact()
        out <<- paste(as.character(output$audit), collapse = "")
      }
    )
    out
  }

  expect_match(headers(german()), "Zeitstempel", fixed = TRUE)
  expect_match(headers(NULL), "Timestamp", fixed = TRUE)
  expect_false(grepl("Zeitstempel", headers(NULL), fixed = TRUE))
})

test_that("a reactive language switches a running session", {
  skip_if_not_installed("DT")

  shown <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown <<- c(shown, as.character(ui))
      invisible(NULL)
    },
    .package = "shiny"
  )

  db_path <- tempfile(fileext = ".sqlite")
  f <- sft_test_language_form(db_path)
  conn <- local_test_conn(db_path)
  init_db(f, conn = conn)
  insert_record(f, list(name = "Ada"), conn = conn, user = "alice")

  current <- shiny::reactiveVal(german())

  shiny::testServer(
    form_server,
    args = list(id = "lang", form = f, conn = conn, language = current, labels = list(cancel = "Weg")),
    {
      session$flushReact()

      expect_identical(state$labels$save, "Speichern")
      # An explicit label still wins over the language, in either language.
      expect_identical(state$labels$cancel, "Weg")
      expect_match(paste(as.character(output$audit), collapse = ""), "Zeitstempel", fixed = TRUE)

      session$setInputs(add_name = "")
      session$setInputs(submit_add = 1)
      expect_true(any(grepl("Pflichtfelder fehlen", shown, fixed = TRUE)))

      # Switch while the session runs.
      current(english())
      session$flushReact()

      expect_identical(state$labels$save, "Save")
      expect_identical(state$labels$cancel, "Weg")
      audit <- paste(as.character(output$audit), collapse = "")
      expect_match(audit, "Timestamp", fixed = TRUE)
      expect_false(grepl("Zeitstempel", audit, fixed = TRUE))

      shown <<- character()
      session$setInputs(submit_add = 2)
      expect_true(any(grepl("Mandatory fields missing", shown, fixed = TRUE)))
      expect_false(any(grepl("Pflichtfelder", shown, fixed = TRUE)))
    }
  )

  expect_null(sft_active_language())
})

test_that("a plain function works as a language too, and a bad one is named", {
  html <- as.character(form_ui("lang", language = function() german()))
  expect_match(html, "Eintrag hinzuf", fixed = TRUE)

  expect_identical(sft_with_language(function() german(), sft_ui_labels()$save), "Speichern")
  expect_error(sft_with_language(function() "de", sft_ui_labels()), "language must be NULL")
})
