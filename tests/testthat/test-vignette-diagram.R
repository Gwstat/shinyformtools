# The schema diagram in vignettes/ is hand-drawn, so nothing stops it from
# drifting away from the schema the package actually creates. These tests pin it
# to the definitions: add a system column or a system table without drawing it
# and they fail.
#
# The vignette sources are not shipped to the check directory, so these skip
# during R CMD check and run during devtools::test().

diagram_text <- function() {
  path <- testthat::test_path("..", "..", "vignettes", "db-schema.svg")
  testthat::skip_if_not(file.exists(path), "vignette sources not available")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("the schema diagram draws every system column", {
  svg <- diagram_text()

  for (column in names(sft_system_columns())) {
    expect_true(
      grepl(paste0(">", column, "<"), svg, fixed = TRUE),
      info = paste0("system column not drawn in db-schema.svg: ", column)
    )
  }
})

test_that("the schema diagram draws every system table", {
  svg <- diagram_text()

  tables <- c(
    "sft_forms", "sft_fields", "sft_audit_log",
    "sft_schema_migrations", "sft_user_preferences"
  )

  for (table in tables) {
    expect_true(
      grepl(paste0(">", table, "<"), svg, fixed = TRUE),
      info = paste0("system table not drawn in db-schema.svg: ", table)
    )
  }
})

test_that("the schema diagram claims no foreign keys, and the package declares none", {
  # The diagram tells readers the links are conventions rather than constraints.
  # If that ever stops being true, the diagram becomes actively misleading.
  r_files <- list.files(
    testthat::test_path("..", "..", "R"),
    pattern = "\\.R$", full.names = TRUE
  )
  testthat::skip_if_not(length(r_files) > 0L, "package sources not available")

  sql_text <- paste(
    unlist(lapply(r_files, readLines, warn = FALSE)),
    collapse = "\n"
  )

  expect_false(grepl("FOREIGN KEY", sql_text, ignore.case = TRUE))
  expect_true(grepl("FOREIGN KEY constraints", diagram_text(), fixed = TRUE))
})
