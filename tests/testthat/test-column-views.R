testthat::test_that("shared column views are available across users", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "column_views",
    table_name = "column_views",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "email", label = "E-Mail"),
      form_field(id = "phone", label = "Telefon")
    )
  )

  init_db(form, conn = conn)

  sft_set_shared_column_view(
    conn = conn,
    form = form,
    view_name = "Kontakt",
    columns = c("sft_easy_id", "name", "phone")
  )

  testthat::expect_true("Kontakt" %in% sft_available_shared_column_view_names(conn, form))
  testthat::expect_equal(
    sft_get_shared_column_view(conn, form, "Kontakt"),
    c("sft_easy_id", "name", "phone")
  )

  testthat::expect_equal(
    sft_resolve_saved_column_view(
      conn = conn,
      form = form,
      user = "viewer",
      table_views = NULL,
      view_name = "Kontakt"
    ),
    c("sft_easy_id", "name", "phone")
  )
})

testthat::test_that("sft_column_view_names lists Standard and shared views", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "view_names",
    table_name = "view_names",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "email", label = "E-Mail"),
      form_field(id = "phone", label = "Telefon")
    )
  )

  init_db(form, conn = conn)

  # Without persistence, "Standard" is always offered.
  testthat::expect_true(
    "Standard" %in% sft_column_view_names(
      table_views = NULL,
      persist_column_settings = FALSE,
      conn = conn,
      form = form
    )
  )

  sft_set_shared_column_view(
    conn = conn,
    form = form,
    view_name = "Kontakt",
    columns = c("sft_easy_id", "name", "phone")
  )

  # With persistence on, shared saved views appear in the list.
  testthat::expect_true(
    "Kontakt" %in% sft_column_view_names(
      table_views = NULL,
      persist_column_settings = TRUE,
      conn = conn,
      form = form
    )
  )
})

testthat::test_that("sft_resolve_column_view_columns honours saved views and falls back", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  form <- form(
    form_id = "resolve_columns",
    table_name = "resolve_columns",
    db_path = db_path,
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "email", label = "E-Mail"),
      form_field(id = "phone", label = "Telefon")
    )
  )

  init_db(form, conn = conn)
  insert_record(
    form,
    list(name = "A", email = "a@example.com", phone = "1"),
    conn = conn
  )
  data <- fetch_records(form, conn = conn)

  sft_set_shared_column_view(
    conn = conn,
    form = form,
    view_name = "Kontakt",
    columns = c("name", "phone")
  )

  # A saved view drives the resolved selection: selected columns are kept,
  # unselected-but-allowed columns (email) are dropped.
  resolved <- sft_resolve_column_view_columns(
    view_name = "Kontakt",
    conn = conn,
    form = form,
    user = "viewer",
    table_views = NULL,
    persist_column_settings = TRUE,
    data = data,
    show_system_columns = FALSE
  )

  testthat::expect_true(all(c("name", "phone") %in% resolved))
  testthat::expect_false("email" %in% resolved)

  # An unknown view with no saved columns falls back to the default column set.
  fallback <- sft_resolve_column_view_columns(
    view_name = "DoesNotExist",
    conn = conn,
    form = form,
    user = "viewer",
    table_views = NULL,
    persist_column_settings = FALSE,
    data = data,
    show_system_columns = FALSE
  )

  testthat::expect_gt(length(fallback), 0L)
})
