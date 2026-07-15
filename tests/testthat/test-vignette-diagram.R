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
  # Defaults: the columns every form pays for, which the diagram must show.
  svg <- diagram_text()

  for (column in names(sft_system_columns())) {
    expect_true(
      grepl(paste0(">", column, "<"), svg, fixed = TRUE),
      info = paste0("system column not drawn in db-schema.svg: ", column)
    )
  }
})

test_that("the schema diagram draws no column the package does not create", {
  # The mirror of the test above. Without this, a column removed from
  # sft_system_columns() keeps being drawn and the diagram quietly describes a
  # database that no longer exists.
  svg <- diagram_text()

  drawn <- unique(unlist(regmatches(svg, gregexpr(">sft_[a-z_]+<", svg))))
  drawn <- gsub("^>|<$", "", drawn)

  known <- c(
    # Every column the package can create, including the opt-in ones - the
    # diagram is allowed to show sft_uuid / sft_easy_id as optional.
    names(sft_system_columns(uuid = TRUE, easy_id = TRUE)),
    # The system tables are drawn by name too.
    "sft_forms", "sft_fields", "sft_audit_log",
    "sft_schema_migrations", "sft_user_preferences"
  )

  expect_equal(setdiff(drawn, known), character(0))
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
