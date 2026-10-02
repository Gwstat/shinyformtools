slide_form <- function(slide_labels = NULL, field_label = NULL) {
  form(
    form_id = "talks",
    table_name = "talks",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    slide_labels = slide_labels,
    fields = list(
      form_field(id = "name", label = "Name", slide = 0L, slide_label = field_label),
      form_field(id = "title", label = "Title", slide = 1L)
    )
  )
}

render_html <- function(form) {
  as.character(render_form_fields(form, ns = shiny::NS("m")))
}

test_that("slide labels appear as headings above their slide", {
  skip_if_not_installed("shinyglide")

  html <- render_html(slide_form(slide_labels = c("Person", "Talk")))

  expect_identical(lengths(regmatches(html, gregexpr("sft-slide-title", html))), 2L)
  expect_true(grepl('<h4 class="sft-slide-title">Person</h4>', html, fixed = TRUE))
  expect_true(grepl('<h4 class="sft-slide-title">Talk</h4>', html, fixed = TRUE))
})

test_that("a field's slide_label overrides the form's slide_labels", {
  skip_if_not_installed("shinyglide")

  html <- render_html(slide_form(slide_labels = c("Person", "Talk"), field_label = "About you"))

  expect_true(grepl(">About you</h4>", html, fixed = TRUE))
  expect_false(grepl(">Person</h4>", html, fixed = TRUE))
})

test_that("slides without a label get no heading", {
  skip_if_not_installed("shinyglide")

  html <- render_html(slide_form())

  expect_false(grepl("sft-slide-title", html, fixed = TRUE))
  # Only the second slide is named: the first keeps its plain rendering.
  partial <- render_html(slide_form(slide_labels = c("", "Talk")))
  expect_identical(lengths(regmatches(partial, gregexpr("sft-slide-title", partial))), 1L)
})

test_that("the wizard buttons follow the language; English renders as before", {
  skip_if_not_installed("shinyglide")
  f <- slide_form()

  english <- render_html(f)
  expect_identical(english, as.character(render_form_fields(f, ns = shiny::NS("m"))))
  expect_true(grepl("Back", english, fixed = TRUE))
  # Same HTML as shinyglide's own default buttons.
  plain <- sft_with_language(language(), render_html(f))
  expect_identical(plain, english)

  german_html <- sft_with_language(german(), render_html(f))
  expect_true(grepl("Zurück", german_html, fixed = TRUE))
  expect_true(grepl("Weiter", german_html, fixed = TRUE))
  expect_false(grepl(">Back<|Back</", german_html))
})
