# Bundled example apps are not launched in the test suite, but they must at
# least be discoverable and parse cleanly so a broken example is caught here
# rather than by a user running run_example(). parse() needs no packages.

testthat::test_that("bundled example apps are discoverable and parse", {
  examples <- list_examples()

  testthat::expect_true(all(c(
    "app_shape_map", "app_cascading_inputs", "app_shinymanager",
    "app_backends", "app_mariadb"
  ) %in% examples))

  for (example in examples) {
    path <- example_path(example)
    testthat::expect_true(file.exists(path))
    testthat::expect_no_error(parse(file = path))
  }
})

testthat::test_that("shared demo scaffolding is not listed as an example", {
  testthat::expect_false("_demo_scaffold" %in% list_examples())
  testthat::expect_true(file.exists(example_path("_demo_scaffold")))
})

# The "Complete code" tab promises the whole, runnable app: the example file
# minus the #> DEMO region that pulls the walkthrough in. If that promise breaks,
# users copy code that does not run - which is the whole point of the tab.
testthat::test_that("the complete code of every example is runnable and scaffold-free", {
  scaffold <- new.env(parent = globalenv())
  sys.source(example_path("_demo_scaffold"), envir = scaffold)

  for (example in list_examples()) {
    lines <- readLines(example_path(example), warn = FALSE)
    code <- scaffold$demo_code(lines)

    # It must parse on its own: no half-open call left by the stripping.
    testthat::expect_no_error(
      parse(text = paste(code, collapse = "\n")),
      message = paste("complete code does not parse:", example)
    )

    # It must not leak the walkthrough machinery, and must not depend on it.
    testthat::expect_false(
      any(grepl("demo_page|demo_scaffold|how_built|demo_code|#>", code)),
      label = paste("scaffolding leaked into the complete code of", example)
    )

    # It must still be the whole app: the launch call survives the stripping.
    testthat::expect_true(
      any(grepl("^shinyApp\\(|^shiny::shinyApp\\(", code)),
      label = paste("complete code of", example, "has no shinyApp() call")
    )
  }
})

testthat::test_that("every example marks up its own walkthrough", {
  for (example in list_examples()) {
    lines <- readLines(example_path(example), warn = FALSE)

    # Exactly one demo region, and it sources the shared scaffolding.
    testthat::expect_length(grep("^#>\\s*DEMO\\s*$", lines), 1L)
    testthat::expect_length(grep("^#>\\s*DEMO END\\s*$", lines), 1L)
    testthat::expect_true(
      any(grepl('source\\(example_path\\("_demo_scaffold"\\)', lines)),
      label = paste(example, "does not source the shared scaffolding")
    )

    # And it has steps to show.
    testthat::expect_gt(length(grep("^#>\\s*STEP:", lines)), 0L)
  }
})

testthat::test_that("examples of another package are found by its name", {
  testthat::skip_on_cran()
  # The default stays shinyformtools, so old calls behave as before.
  testthat::expect_identical(list_examples(), list_examples("shinyformtools"))
  testthat::expect_identical(example_path("app_crud_basic"),
                             example_path("app_crud_basic", package = "shinyformtools"))
  # Any installed package with inst/examples works the same way.
  pkg <- file.path(tempfile("lib"), "exmpkg")
  dir.create(file.path(pkg, "inst", "examples"), recursive = TRUE)
  writeLines(c("Package: exmpkg", "Version: 0.0.1", "Title: x", "Description: x.",
               "License: GPL-3", "Maintainer: A B <a@example.org>", "Author: A B"),
             file.path(pkg, "DESCRIPTION"))
  writeLines("shiny::shinyApp(shiny::fluidPage(), function(input, output) NULL)",
             file.path(pkg, "inst", "examples", "app_one.R"))
  writeLines("# helper", file.path(pkg, "inst", "examples", "_helper.R"))
  writeLines("", file.path(pkg, "NAMESPACE"))
  lib <- tempfile("lib")
  dir.create(lib)
  utils::install.packages(pkg, lib = lib, repos = NULL, type = "source", quiet = TRUE)
  withr::with_libpaths(lib, action = "prefix", {
    testthat::expect_identical(list_examples("exmpkg"), "app_one")
    testthat::expect_true(file.exists(example_path("app_one", package = "exmpkg")))
    testthat::expect_error(example_path("app_two", package = "exmpkg"), "Available examples: app_one")
  })
  testthat::expect_error(list_examples("noSuchPackage123"), "Package not found")
  # An installed package without examples says so.
  testthat::expect_identical(list_examples("stats"), character())
  testthat::expect_error(example_path("x", package = "stats"), "stats ships no examples")
})

testthat::test_that("run_example() passes package by name and the rest to runApp()", {
  seen <- NULL
  testthat::local_mocked_bindings(runApp = function(appDir, ...) seen <<- list(appDir = appDir, ...),
                                  .package = "shiny")
  run_example("app_crud_basic", port = 1234)
  testthat::expect_identical(seen$appDir, example_path("app_crud_basic"))
  testthat::expect_identical(seen$port, 1234)
})
