testthat::test_that("display_transform can add display-only columns", {
  testthat::skip_if_not_installed("dplyr")
  form <- form(
    form_id = "display_test",
    fields = list(
      form_field(id = "street", label = "Straße"),
      form_field(id = "house_no", label = "Hausnummer")
    )
  )

  raw <- data.frame(
    sft_id = c(1L, 2L),
    sft_easy_id = c("D-000001", "D-000002"),
    street = c("Ackerstraße", "Breiter Weg"),
    house_no = c("1", "2"),
    sft_updated_at = c("2026-01-01T10:00:00+0000", "2026-01-02T10:00:00+0000"),
    sft_updated_by = c("tester", "tester"),
    sft_is_deleted = c(0L, 0L),
    stringsAsFactors = FALSE
  )

  display <- sft_apply_display_transform(
    data = raw,
    display_transform = function(data) {
      data |>
        dplyr::mutate(address = paste(street, house_no))
    }
  )

  testthat::expect_equal(display$address, c("Ackerstraße 1", "Breiter Weg 2"))

  choices <- sft_record_column_choices(
    form = form,
    data = display,
    display_column_labels = c(address = "Adresse")
  )

  testthat::expect_true("address" %in% unname(choices))
  testthat::expect_equal(names(choices)[unname(choices) == "address"], "Adresse")

  columns <- sft_resolve_record_columns(
    form = form,
    data = display,
    columns = c("address", "street")
  )

  testthat::expect_equal(columns, c("address", "street"))
})

testthat::test_that("display_transform must preserve sft_id", {
  raw <- data.frame(
    sft_id = 1L,
    name = "Ada",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    sft_apply_display_transform(
      data = raw,
      display_transform = function(data) {
        data.frame(name = data$name, stringsAsFactors = FALSE)
      }
    ),
    "must preserve the sft_id column"
  )
})

testthat::test_that("display selections map back to raw records by sft_id", {
  testthat::skip_if_not_installed("dplyr")
  raw <- data.frame(
    sft_id = c(1L, 2L),
    name = c("Ada", "Grace"),
    stringsAsFactors = FALSE
  )

  display <- raw |>
    dplyr::arrange(dplyr::desc(name)) |>
    dplyr::mutate(label = paste0(name, "!"))

  selected <- sft_selected_record_from_display(
    display_data = display,
    raw_data = raw,
    selected = 1L
  )

  testthat::expect_equal(selected$sft_id, 2L)
  testthat::expect_equal(selected$name, "Grace")
})
