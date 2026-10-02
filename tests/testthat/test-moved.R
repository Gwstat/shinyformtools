# Grid and basket live in shinygridtools, which depends on this package and
# not the other way round. Here: the old names say where they went, the
# extension points it uses work for any package.

test_that("the old names stop and say where the grid and the basket went", {
  expect_error(grid_input("g"), "`grid_input\\(\\)` moved to the package shinygridtools")
  expect_error(grid_server("g"), "library\\(shinygridtools\\)")
  expect_error(cart_catalog(NULL), "remotes::install_github")
  # A module id comes first, so shiny::testServer() treats it as a module.
  expect_identical(names(formals(grid_server))[1], "id")
  expect_identical(names(formals(grid_ui))[1], "id")
})

test_that("an unknown moved input type says which package registers it", {
  expect_error(sft_unsupported_input_type("cart_input"), "input type 'cart_input' moved")
  expect_error(sft_unsupported_input_type("nope"), "Unsupported input_type: nope")
})

test_that("shinyformtools neither imports, suggests nor loads shinygridtools", {
  desc <- utils::packageDescription("shinyformtools", fields = c("Depends", "Imports", "Suggests", "Remotes"))
  expect_false(any(grepl("shinygridtools", unlist(desc))))
  dir <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(dir), "sources not available")
  code <- unlist(lapply(list.files(dir, full.names = TRUE), readLines, warn = FALSE))
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("(requireNamespace|loadNamespace|getExportedValue|library|require)\\(\\s*\"shinygridtools\"", code)))
  expect_false(any(grepl("shinygridtools::", code, fixed = TRUE)))
})

test_that("form_ui(show_board = TRUE) draws the part an extension registered", {
  had <- exists("board", envir = .sft_ui_parts, inherits = FALSE)
  old <- if (had) get("board", envir = .sft_ui_parts)
  on.exit(if (had) assign("board", old, envir = .sft_ui_parts) else rm("board", envir = .sft_ui_parts), add = TRUE)
  if (had) rm("board", envir = .sft_ui_parts)

  expect_error(form_ui("m", show_board = TRUE), "form_ui\\(show_board = TRUE\\) moved")
  expect_false(grepl("test-board", as.character(form_ui("m"))))

  register_ui_part("board", function(id, labels) {
    shiny::div(id = paste0(id, "-board"), class = "test-board", labels$save)
  })
  html <- as.character(form_ui("m", show_board = TRUE, language = german()))
  expect_match(html, 'id="m-board"', fixed = TRUE)
  expect_match(html, ">Speichern<", fixed = TRUE)
  expect_error(register_ui_part("board", "x"), "fun must be")
})

test_that("texts registered after german() / use_german() still reach them", {
  de <- german()
  en <- english()
  withr::defer({
    for (lang in c("en", "de")) {
      current <- sft_registered_texts(lang)
      current$labels$sft_test_late <- NULL
      current$messages$sft_test_late <- NULL
      assign(lang, current, envir = .sft_registered_texts)
    }
  })
  register_texts("en", labels = list(sft_test_late = "Stock"), messages = list(sft_test_late = "Too few"))
  register_texts("de", labels = list(sft_test_late = "Lager"), messages = list(sft_test_late = "Zu wenig"))

  expect_identical(with_language(de, ui_label("sft_test_late")), "Lager")
  expect_identical(with_language(en, ui_label("sft_test_late")), "Stock")
  f <- form(form_id = "t", table_name = "t", db = db_sqlite(tempfile(fileext = ".sqlite")),
            fields = list(form_field(id = "name", label = "Name")))
  expect_identical(with_language(de, form_message(f, "sft_test_late")), "Zu wenig")

  # A language of the user's own keeps its own words over registered ones.
  own <- de
  own$labels$sft_test_late <- "Depot"
  expect_identical(with_language(own, ui_label("sft_test_late")), "Depot")

  withr::defer(use_english())
  use_german()
  expect_identical(ui_label("sft_test_late"), "Lager")
  use_english()
  expect_identical(ui_label("sft_test_late"), "Stock")
})
