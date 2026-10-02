# The extension API: hooks of register_input(), registered texts, db helpers.
# The grid and the basket of shinygridtools are built on it.

test_that("register_input() hooks run: validate on save, server inside form_server", {
  seen <- new.env()
  register_input(
    "sft_test_hooked", fun = shiny::textInput,
    validate = function(form, fields, record, conn, current_id) {
      value <- record_value(record, fields[[1]])
      if (identical(value, "bad")) list(validation_issue(fields[[1]]$id, "no bad values", source = "hooked"))
    },
    server = function(input, output, session, state, fields) {
      seen$fields <- vapply(fields, function(f) f$id, character(1))
      seen$user <- state$current_user()
    }
  )
  on.exit(rm("sft_test_hooked", envir = shinyformtools:::.sft_input_registry), add = TRUE)

  f <- form(form_id = "hooked", table_name = "hooked", db = db_sqlite(tempfile(fileext = ".sqlite")),
            fields = list(form_field(id = "name", label = "Name"),
                          form_field(id = "tag", label = "Tag", input_type = "sft_test_hooked")))
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  err <- tryCatch(insert_record(f, list(name = "a", tag = "bad"), conn = conn), error = function(e) e)
  expect_s3_class(err, "sft_validation_error")
  expect_identical(err$issues$source, "hooked")
  insert_record(f, list(name = "a", tag = "good"), conn = conn)
  expect_error(update_record(f, list(tag = "bad"), record_id = 1L, conn = conn), class = "sft_validation_error")

  shiny::testServer(form_server, args = list(id = "m", form = f, conn = conn, user = "bob",
                                             columns = list(persist = FALSE)), {
    session$flushReact()
  })
  expect_identical(seen$fields, "tag")
  expect_identical(seen$user, "bob")
})

test_that("register_texts() adds label and message keys to english and german", {
  register_texts("en", labels = list(sft_test_hello = "Hello {name}"), messages = list(sft_test_msg = "Nope {x}"))
  register_texts("de", labels = list(sft_test_hello = "Hallo {name}"), messages = list(sft_test_msg = "Nein {x}"))
  expect_identical(ui_label("sft_test_hello", list(name = "Ada")), "Hello Ada")
  expect_identical(with_language(german(), ui_label("sft_test_hello", list(name = "Ada"))), "Hallo Ada")
  f <- form(form_id = "t", table_name = "t", db = db_sqlite(tempfile(fileext = ".sqlite")),
            fields = list(form_field(id = "name", label = "Name")))
  expect_identical(form_message(f, "sft_test_msg", list(x = 1)), "Nope 1")
  expect_identical(with_language(german(), form_message(f, "sft_test_msg", list(x = 1))), "Nein 1")
  # A language() may now set the registered key.
  expect_no_error(language(labels = list(sft_test_hello = "Hi")))
})

test_that("the db helpers find records by key and report the backend", {
  f <- form(form_id = "k", table_name = "k", db = db_sqlite(tempfile(fileext = ".sqlite")),
            fields = list(form_field(id = "g", label = "G"), form_field(id = "n", label = "N", input_type = "numericInput")))
  conn <- local_test_conn(f$db)
  init_db(f, conn = conn)
  insert_record(f, list(g = "a", n = 1), conn = conn)
  insert_record(f, list(g = "b", n = 2), conn = conn)
  expect_identical(find_records_by_key(f, list(g = "a"), conn)$n, 1)
  expect_identical(db_backend(conn), "sqlite")
  expect_true(table_exists(conn, "k"))
  stamp <- records_stamp(f, list(g = "a"), conn)
  update_record(f, list(n = 5), record_id = 1L, conn = conn)
  expect_false(identical(records_stamp(f, list(g = "a"), conn), stamp))
  expect_identical(fetch_record(f, 2L, conn)$g, "b")
  ran <- FALSE
  with_transaction(conn, { after_commit(conn, function() ran <<- TRUE); expect_false(ran); expect_true(in_transaction(conn)) })
  expect_true(ran)
})
