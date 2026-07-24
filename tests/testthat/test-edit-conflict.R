# Edit-conflict detection (optimistic locking) and the module conflict view.

sft_test_conflict_form <- function(db_path) {
  form(
    form_id = "contacts",
    table_name = "contacts",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(id = "age", label = "Age", input_type = "numericInput")
    )
  )
}

test_that("sft_conflicting_columns compares stored input values type-tolerantly", {
  contacts <- sft_test_conflict_form(tempfile(fileext = ".sqlite"))

  expected <- list(name = "Ada", age = 30)

  # Identical values (even with a numeric/text type mismatch) are no conflict.
  expect_identical(
    sft_conflicting_columns(contacts, expected, list(name = "Ada", age = "30")),
    character()
  )

  # A genuinely different value names its column.
  expect_identical(
    sft_conflicting_columns(contacts, expected, list(name = "Bob", age = 30)),
    "name"
  )

  expect_identical(
    sft_conflicting_columns(contacts, expected, list(name = "Bob", age = 31)),
    c("name", "age")
  )
})

test_that("update_record with expected_record rejects a stale edit and writes nothing", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_conflict_form(db_path)
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(contacts, conn = conn, user = "alice")
  added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
  record_id <- added$sft_id[1]
  baseline <- added

  # Another user saves while the edit dialog is (conceptually) open.
  update_record(
    contacts,
    list(name = "Bob"),
    record_id = record_id,
    conn = conn,
    user = "bob"
  )

  err <- tryCatch(
    update_record(
      contacts,
      list(name = "Alice-Edit", age = 31),
      record_id = record_id,
      conn = conn,
      user = "alice",
      expected_record = baseline
    ),
    sft_edit_conflict = function(cond) cond
  )

  expect_true(inherits(err, "sft_edit_conflict"))
  expect_identical(err$columns, "name")
  expect_identical(err$current_record$name[1], "Bob")

  # Nothing was written: the stale edit did not overwrite the newer value.
  stored <- fetch_records(contacts, conn = conn)
  stored <- stored[stored$sft_id == record_id, , drop = FALSE]
  expect_identical(stored$name[1], "Bob")
  expect_equal(as.numeric(stored$age[1]), 30)

  # With a fresh baseline the same update succeeds.
  update_record(
    contacts,
    list(name = "Alice-Edit", age = 31),
    record_id = record_id,
    conn = conn,
    user = "alice",
    expected_record = stored
  )

  stored <- fetch_records(contacts, conn = conn)
  expect_identical(stored$name[stored$sft_id == record_id][1], "Alice-Edit")

  # Without expected_record the previous behaviour is unchanged (no check).
  update_record(
    contacts,
    list(name = "Overwrite"),
    record_id = record_id,
    conn = conn,
    user = "carol"
  )
  stored <- fetch_records(contacts, conn = conn)
  expect_identical(stored$name[stored$sft_id == record_id][1], "Overwrite")
})

test_that("a save that changed no values does not count as a conflict", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_conflict_form(db_path)
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(contacts, conn = conn, user = "alice")
  added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
  record_id <- added$sft_id[1]

  # Another user saves the identical values: sft_updated_at moves on, the
  # values do not. The check is value-based, so this is not a conflict.
  update_record(
    contacts,
    list(name = "Ada", age = 30),
    record_id = record_id,
    conn = conn,
    user = "bob"
  )

  expect_silent(
    update_record(
      contacts,
      list(name = "Ada-Edit"),
      record_id = record_id,
      conn = conn,
      user = "alice",
      expected_record = added
    )
  )

  stored <- fetch_records(contacts, conn = conn)
  expect_identical(stored$name[stored$sft_id == record_id][1], "Ada-Edit")
})

test_that("sft_conflict_changes_meta attributes columns to the last writer", {
  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_conflict_form(db_path)
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(contacts, conn = conn, user = "alice")
  added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
  record_id <- added$sft_id[1]

  Sys.sleep(0.05)
  update_record(contacts, list(name = "Bob-Name"), record_id = record_id,
                conn = conn, user = "bob")
  Sys.sleep(0.05)
  update_record(contacts, list(age = 44), record_id = record_id,
                conn = conn, user = "carol")

  meta <- sft_conflict_changes_meta(
    conn = conn,
    form = contacts,
    record_id = record_id,
    since = added$sft_updated_at[1],
    fallback_user = NULL
  )

  expect_identical(unname(meta$by_column["name"]), "bob")
  expect_identical(unname(meta$by_column["age"]), "carol")
  expect_setequal(meta$users, c("bob", "carol"))

  # Unreadable audit information falls back to the supplied user.
  dead_conn <- db_connect(db_path)
  db_disconnect(dead_conn)
  fallback <- sft_conflict_changes_meta(
    conn = dead_conn,
    form = contacts,
    record_id = record_id,
    since = added$sft_updated_at[1],
    fallback_user = "bob"
  )
  expect_identical(fallback$users, "bob")
  expect_length(fallback$by_column, 0L)
})

test_that("the edit dialog switches to the conflict view and resolves via keep", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_conflict_form(db_path)
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(contacts, conn = conn, user = "alice")
  added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
  record_id <- added$sft_id[1]

  shiny::testServer(
    form_server,
    args = list(
      id = "contacts",
      form = contacts,
      conn = conn,
      user = function() "alice"
    ),
    {
      session$flushReact()

      row <- fetch_records(contacts, conn = conn)
      row <- row[row$sft_id == record_id, , drop = FALSE]
      current_edit_row(row)
      edit_conflict_baseline(row)

      # Another user saves while the dialog is open.
      update_record(contacts, list(name = "Bob"), record_id = record_id,
                    conn = conn, user = "bob")

      session$setInputs(edit_name = "Alice-Edit", edit_age = 30)
      session$setInputs(submit_edit = 1)

      # The save was rejected: conflict state set, nothing written.
      expect_false(is.null(edit_conflict()))
      expect_identical(edit_conflict()$columns, "name")

      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Bob")

      # The conflict view renders with the field, both values and the writer.
      html <- as.character(output$sft_edit_conflict_ui$html)
      expect_true(grepl("sft-edit-conflict", html, fixed = TRUE))
      expect_true(grepl("Ada", html, fixed = TRUE))
      expect_true(grepl("Bob", html, fixed = TRUE))
      expect_true(grepl("sft_conflict_accept", html, fixed = TRUE))
      # It also hides the regular form body while it is shown.
      expect_true(grepl("sft_edit_form_main", html, fixed = TRUE))
      expect_true(grepl("display: none", html, fixed = TRUE))

      # "Keep my entries": baseline moves to the current row, view closes,
      # and the next save deliberately writes the user's values.
      session$setInputs(sft_conflict_keep = 1)
      expect_null(edit_conflict())
      expect_identical(edit_conflict_baseline()$name[1], "Bob")

      session$setInputs(submit_edit = 2)
      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Alice-Edit")
    }
  )
})

test_that("conflict_check = FALSE restores last-write-wins", {
  skip_if_not_installed("DT")

  db_path <- tempfile(fileext = ".sqlite")
  contacts <- sft_test_conflict_form(db_path)
  conn <- db_connect(db_path)
  on.exit(db_disconnect(conn), add = TRUE)

  init_db(contacts, conn = conn, user = "alice")
  added <- insert_record(
    contacts,
    list(name = "Ada", age = 30),
    conn = conn,
    user = "alice"
  )
  record_id <- added$sft_id[1]

  shiny::testServer(
    form_server,
    args = list(
      id = "contacts",
      form = contacts,
      conn = conn,
      user = function() "alice",
      conflict_check = FALSE
    ),
    {
      session$flushReact()

      row <- fetch_records(contacts, conn = conn)
      row <- row[row$sft_id == record_id, , drop = FALSE]
      current_edit_row(row)
      edit_conflict_baseline(row)

      update_record(contacts, list(name = "Bob"), record_id = record_id,
                    conn = conn, user = "bob")

      session$setInputs(edit_name = "Alice-Edit", edit_age = 30)
      session$setInputs(submit_edit = 1)

      # No conflict view: the stale edit overwrites (previous behaviour).
      expect_null(edit_conflict())
      stored <- fetch_records(contacts, conn = conn)
      expect_identical(stored$name[stored$sft_id == record_id][1], "Alice-Edit")
    }
  )
})
