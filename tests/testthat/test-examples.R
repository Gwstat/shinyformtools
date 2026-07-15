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
