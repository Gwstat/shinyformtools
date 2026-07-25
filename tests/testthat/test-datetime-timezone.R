# Displayed timestamps must show the time the event actually happened, in a
# defined zone. The bug this guards against was found by looking at a running
# app: a record written at 08:18 local time was listed as 06:18, because the
# value was parsed as UTC and then also FORMATTED in UTC, silently shifting
# every timestamp in the records, audit and versions tables by the local offset.

testthat::test_that("a timestamp with an offset is shown in the configured zone", {
  testthat::skip_if_not_installed("withr")

  # What sft_now() writes: an instant, with the writer's offset attached.
  stamp <- "2026-07-25T08:18:03.670+0200"

  withr::with_options(list(shinyformtools.datetime_timezone = "Europe/Berlin"), {
    testthat::expect_identical(
      sft_format_datetime_value(stamp), "25.07.2026 08:18"
    )
  })

  # The same instant, read from elsewhere. These are translations of one moment,
  # not different moments.
  withr::with_options(list(shinyformtools.datetime_timezone = "UTC"), {
    testthat::expect_identical(
      sft_format_datetime_value(stamp), "25.07.2026 06:18"
    )
  })

  withr::with_options(list(shinyformtools.datetime_timezone = "Asia/Tokyo"), {
    testthat::expect_identical(
      sft_format_datetime_value(stamp), "25.07.2026 15:18"
    )
  })
})

testthat::test_that("a timestamp without an offset is never shifted", {
  testthat::skip_if_not_installed("withr")

  # A wall-clock reading whose zone nobody recorded - typically a user's own
  # date or datetime field. Moving it would change the value they typed.
  for (tz in c("UTC", "Europe/Berlin", "Asia/Tokyo", "America/New_York")) {
    withr::with_options(list(shinyformtools.datetime_timezone = tz), {
      testthat::expect_identical(
        sft_format_datetime_value("2026-07-25 08:18:03"), "25.07.2026 08:18"
      )
      testthat::expect_identical(
        sft_format_datetime_value("2026-08-01"), "01.08.2026 00:00"
      )
    })
  }
})

testthat::test_that("offset detection covers the spellings that occur", {
  testthat::expect_true(all(sft_has_utc_offset(c(
    "2026-07-25T08:18:03.670+0200",
    "2026-07-25T08:18:03+02:00",
    "2026-07-25T06:18:03Z",
    "2026-07-25 08:18:03-0500"
  ))))

  testthat::expect_false(any(sft_has_utc_offset(c(
    "2026-07-25 08:18:03",
    "2026-07-25T08:18:03",
    "2026-08-01",
    ""
  ))))
})

testthat::test_that("the display zone falls back to the machine's own", {
  testthat::skip_if_not_installed("withr")

  testthat::expect_identical(sft_datetime_timezone(), "")

  withr::with_options(list(shinyformtools.datetime_timezone = "UTC"), {
    testthat::expect_identical(sft_datetime_timezone(), "UTC")
  })

  # A nonsense option must not take the formatting down with it.
  for (bad in list(NULL, NA_character_, 42L, c("UTC", "GMT"))) {
    withr::with_options(list(shinyformtools.datetime_timezone = bad), {
      testthat::expect_identical(sft_datetime_timezone(), "")
    })
  }
})

testthat::test_that("unparseable and empty values survive untouched", {
  testthat::expect_identical(sft_format_datetime_value("not a date"), "not a date")
  testthat::expect_identical(sft_format_datetime_value(""), "")
  testthat::expect_identical(sft_format_datetime_value(NA_character_), NA_character_)
  testthat::expect_null(sft_format_datetime_value(NULL))

  # Mixed input: each value is judged on its own.
  withr::with_options(list(shinyformtools.datetime_timezone = "UTC"), {
    testthat::expect_identical(
      sft_format_datetime_value(c("2026-07-25T08:18:03+0200", "2026-07-25 08:18:03", "x")),
      c("25.07.2026 06:18", "25.07.2026 08:18", "x")
    )
  })
})
