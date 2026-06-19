test_that("use_german switches UI labels, messages and table labels globally", {
  withr::local_options(list(
    shinyformtools.labels = NULL,
    shinyformtools.messages = NULL,
    shinyformtools.table_labels = NULL,
    shinyformtools.dt_language = NULL
  ))

  # English by default.
  expect_equal(sft_ui_labels()$open_add, "Add entry")
  expect_equal(sft_system_column_labels()[["sft_is_deleted"]], "Deleted")
  expect_equal(sft_action_labels()[["delete"]], "Deleted")

  use_german()

  expect_equal(sft_ui_labels()$open_add, "Eintrag hinzuf\u00fcgen")
  expect_equal(sft_ui_labels()$confirm_delete, "L\u00f6schen")
  expect_equal(
    sft_message(form = NULL, key = "mandatory_missing", values = list(fields = "x")),
    "Pflichtfelder fehlen: x."
  )
  expect_equal(sft_system_column_labels()[["sft_is_deleted"]], "Gel\u00f6scht")
  expect_equal(sft_action_labels()[["delete"]], "Gel\u00f6scht")

  use_english()

  expect_equal(sft_ui_labels()$open_add, "Add entry")
  expect_equal(sft_system_column_labels()[["sft_is_deleted"]], "Deleted")
})

test_that("use_german installs a German DataTables language for the table chrome", {
  withr::local_options(list(
    shinyformtools.labels = NULL,
    shinyformtools.messages = NULL,
    shinyformtools.table_labels = NULL,
    shinyformtools.dt_language = NULL
  ))

  # No language option by default -> DataTables stays English.
  expect_null(sft_dt_options()$language)

  use_german()

  lang <- sft_dt_options()$language
  expect_equal(lang$search, "Suchen:")
  expect_equal(lang$paginate$`next`, "N\u00e4chste")
  expect_true(grepl("Eintr\u00e4ge", lang$lengthMenu))

  # A per-table language still wins over the global option.
  custom <- sft_dt_options(list(language = list(search = "Find:")))
  expect_equal(custom$language$search, "Find:")

  use_english()
  expect_null(sft_dt_options()$language)
})

test_that("the deleted flag renders with the configured Yes/No labels", {
  withr::local_options(list(shinyformtools.table_labels = german_table_labels()))

  data <- data.frame(sft_is_deleted = c(1L, 0L))
  out <- sft_format_display_data(data)

  expect_equal(out$sft_is_deleted, c("Ja", "Nein"))
})

test_that("explicit per-form labels and messages override the global option", {
  withr::local_options(list(
    shinyformtools.labels = german_labels(),
    shinyformtools.messages = german_messages()
  ))

  # Situational rename wins over the global German option.
  expect_equal(sft_ui_labels(list(open_edit = "Editieren"))$open_edit, "Editieren")
  # Other labels still come from the German option.
  expect_equal(sft_ui_labels(list(open_edit = "Editieren"))$open_add, "Eintrag hinzuf\u00fcgen")

  expect_equal(
    sft_message(
      form = NULL, key = "unique", values = list(label = "Email"),
      messages = list(unique = "Custom for {label}")
    ),
    "Custom for Email"
  )
})

test_that("german_labels covers every default UI label key", {
  expect_setequal(names(german_labels()), names(Filter(Negate(is.null), sft_default_ui_labels())))
  expect_setequal(names(german_messages()), names(sft_default_messages()))
  expect_setequal(names(german_table_labels()), names(sft_default_table_labels()))
})
