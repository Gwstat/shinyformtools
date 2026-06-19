testthat::test_that("sft_render_markdown renders Markdown formatting", {
  testthat::skip_if_not_installed("commonmark")

  out <- sft_render_markdown("- one\n- two\n\n**b** _i_ `code` [ok](https://example.com)")

  testthat::expect_match(out, "<ul>")
  testthat::expect_match(out, "<li>one</li>")
  testthat::expect_match(out, "<strong>b</strong>")
  testthat::expect_match(out, "<em>i</em>")
  testthat::expect_match(out, "<code>code</code>")
  testthat::expect_match(out, '<a href="https://example.com">ok</a>', fixed = TRUE)
})

testthat::test_that("sft_render_markdown neutralises raw HTML (stored XSS)", {
  testthat::skip_if_not_installed("commonmark")

  testthat::expect_match(
    sft_render_markdown("hi <script>alert(1)</script> there"),
    "&lt;script&gt;alert(1)&lt;/script&gt;",
    fixed = TRUE
  )

  out <- sft_render_markdown("before <img src=x onerror=alert(1)> after")
  testthat::expect_match(out, "&lt;img", fixed = TRUE)
  testthat::expect_false(grepl("onerror=alert", out, fixed = TRUE) &&
    !grepl("&lt;img", out, fixed = TRUE))

  testthat::expect_match(
    sft_render_markdown("literal <b>bold</b>"),
    "&lt;b&gt;bold&lt;/b&gt;",
    fixed = TRUE
  )
})

testthat::test_that("sft_render_markdown scrubs unsafe link schemes", {
  testthat::skip_if_not_installed("commonmark")

  for (payload in c(
    "[c](javascript:alert(1))",
    "[c](JaVaScRiPt:alert(1))",
    "[c](data:text/html;base64,PHM+)",
    "[c](vbscript:msgbox(1))"
  )) {
    out <- sft_render_markdown(payload)
    testthat::expect_match(out, 'href="#"', fixed = TRUE)
    testthat::expect_false(grepl("javascript:", out, ignore.case = TRUE))
    testthat::expect_false(grepl("vbscript:", out, ignore.case = TRUE))
    testthat::expect_false(grepl("data:text", out, fixed = TRUE))
  }

  # Safe schemes are preserved.
  testthat::expect_match(
    sft_render_markdown("[m](mailto:a@b.com) [h](http://a.b)"),
    "mailto:a@b.com"
  )
})

testthat::test_that("sft_render_markdown maps NA and empty to empty string", {
  testthat::skip_if_not_installed("commonmark")

  testthat::expect_equal(sft_render_markdown(c(NA, "", "**b**"))[1:2], c("", ""))
})

testthat::test_that("markdown is only allowed on input fields", {
  testthat::expect_error(
    form_field(id = "txt", label = "T", markdown = "yes"),
    "markdown must be TRUE or FALSE"
  )

  testthat::expect_error(
    output_field(id = "out", output_type = "text"),
    NA
  )

  bad <- form_field(id = "out", label = "Out", type = "text_output")
  bad$markdown <- TRUE
  testthat::expect_error(sft_validate_field(bad), "Only input fields can be markdown")
})

testthat::test_that("records_datatable renders the markdown column raw and escapes the rest", {
  testthat::skip_if_not_installed("commonmark")

  form <- form(
    form_id = "markdown_records",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "notes", label = "Notes", markdown = TRUE)
    )
  )

  data <- data.frame(
    sft_easy_id = "M-000001",
    name = "<b>plain</b>",
    notes = "**bold** note",
    stringsAsFactors = FALSE
  )

  table <- records_datatable(
    data = data,
    form = form,
    columns = c("sft_easy_id", "name", "notes")
  )

  # Only the third column (notes) is excluded from escaping.
  testthat::expect_equal(attr(table$x$options, "escapeIdx"), "\"-3\"")

  rendered <- as.character(unlist(table$x$data))
  # The markdown cell holds rendered HTML...
  testthat::expect_true(any(grepl("<strong>bold</strong>", rendered, fixed = TRUE)))
  # ...while the plain field keeps its literal markup (DT escapes it client-side).
  testthat::expect_true(any(grepl("<b>plain</b>", rendered, fixed = TRUE)))
  testthat::expect_false(any(grepl("<strong>plain", rendered, fixed = TRUE)))
})

testthat::test_that("records_datatable escapes everything when no markdown fields", {
  form <- form(
    form_id = "markdown_none",
    fields = list(form_field(id = "name", label = "Name"))
  )

  data <- data.frame(
    sft_easy_id = "N-000001",
    name = "Ada",
    stringsAsFactors = FALSE
  )

  table <- records_datatable(data = data, form = form, columns = c("sft_easy_id", "name"))
  testthat::expect_equal(attr(table$x$options, "escapeIdx"), "true")
})

testthat::test_that("versions datatable renders markdown snapshots raw", {
  testthat::skip_if_not_installed("commonmark")

  form <- form(
    form_id = "markdown_versions",
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "notes", label = "Notes", markdown = TRUE)
    )
  )

  snapshot <- jsonlite::toJSON(
    list(name = "<b>plain</b>", notes = "**bold** note"),
    auto_unbox = TRUE
  )

  data <- data.frame(
    changed_by = "u",
    changed_at = "2026-01-01 00:00:00",
    version_no = 1L,
    action = "insert",
    reason = "",
    new_data_json = as.character(snapshot),
    stringsAsFactors = FALSE
  )

  table <- sft_versions_datatable(data = data, form = form)

  # meta columns: changed_by, changed_at, version_no, action, reason (5),
  # then name (6), notes (7). Only notes is excluded.
  testthat::expect_equal(attr(table$x$options, "escapeIdx"), "\"-7\"")

  rendered <- as.character(unlist(table$x$data))
  testthat::expect_true(any(grepl("<strong>bold</strong>", rendered, fixed = TRUE)))
  testthat::expect_true(any(grepl("<b>plain</b>", rendered, fixed = TRUE)))
})
