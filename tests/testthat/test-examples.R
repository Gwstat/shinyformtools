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
