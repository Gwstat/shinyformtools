# shinyformtools 0.10.1

* Examples and documentation use neutral data. `app_matrix_input` enters the
  units a store sold per product and quarter, in English; the
  `upsert_records()` example counts stock per warehouse and product. No
  change in behaviour.

# shinyformtools 0.10.0

shinyformtools no longer depends on shinygridtools in any form (no Suggests,
no Remotes, nothing loads it). shinygridtools depends on shinyformtools and
plugs into it through the extension API.

* Breaking for apps that use the grid or the basket: they attach
  shinygridtools with `library(shinygridtools)`. Before (0.9.0) a form with
  a `grid_input` or `cart_input` field loaded it on its own, and the old
  function names forwarded to it. Now the old names (`grid_input()`,
  `grid_server()`, `cart_input()`, `cart_catalog()`, ...) stop with a
  message saying where they went, and an unknown `grid_input` /
  `cart_input` type says which package registers it. Stored values are
  unchanged.
* New: `register_ui_part()`, so an extension provides a part of
  `form_ui()`. `form_ui(show_board = TRUE)` draws the part `"board"`, which
  shinygridtools registers.
* Fixed: a language taken with `german()`, `english()` or `use_german()`
  before an extension registered its texts (`register_texts()`) showed
  those texts in English. Such texts now reach it whenever they are
  registered; the object's own entries still win.

# shinyformtools 0.9.0

The grid and the basket moved to the package shinygridtools
(https://github.com/Gwstat/shinygridtools), built on the extension API of
0.8.0. Install it next to this package; apps keep working for this release.

* `grid_input()`, `update_grid_input()`, `grid_ui()`, `grid_server()`,
  `cart_input()`, `update_cart_input()` and `cart_catalog()` forward to
  shinygridtools with a deprecation warning once per session
  (`?moved_to_shinygridtools`). Attach shinygridtools with
  `library(shinygridtools)` to use them directly. Without shinygridtools
  installed they stop with a message saying where they went.
* A form whose field has `input_type = "grid_input"` or `"cart_input"`
  loads shinygridtools, which registers the type; values are stored and
  shown as before. `form_ui(show_board = TRUE)` draws shinygridtools'
  board, `language()` accepts its label keys.
* The `grid_*`, `cart_*` and `board_*` labels and the `cart_*` messages come
  from shinygridtools now: `english()`, `german()`, `german_labels()` and
  `language_keys()` list them only once it is loaded.
* Names the browser sees changed with the move (`sgt-grid-*`, `sgt-cart-*`,
  `sgt-board-*` classes instead of `sft-*`); custom CSS against the old
  classes needs the new names.
* The examples `app_grid_entry` and `app_cart_input` moved along
  (`shinygridtools::run_example()`).

# shinyformtools 0.8.1

* Fixed: `grid_server(permissions = list(editable_fields = ...))` failed with
  "$ operator is invalid for atomic vectors"; the grid handed
  `module_editable_fields()` the entry instead of the bundle.

# shinyformtools 0.8.0

An extension API, so that modules over many records (the grid, the basket
and its board) can live in a package of their own (`shinygridtools`, in
preparation) without touching internals. Nothing changes for apps: the grid
and the basket stay in this package for now and behave as before; they are
the first users of the API, and a test keeps them on it.

* New in `register_input()`: `validate` (a check on every save, with all
  fields of the type at once, returning `validation_issue()`s), `server`
  (a module part run inside `form_server()`, see `?module_state`), and
  `prepare_args`, `blank`, `clear`, `empty`, `sep`, which built-in types
  always had.
* New exported helpers, each a thin wrapper over what the package itself
  uses: `form_fields()`, `find_field()`, `key_fields()`, `record_value()`,
  `field_db_value()`, `resolve_editable()` (`?form_helpers`);
  `module_connection()`, `after_commit()`, `after_transaction()`,
  `in_transaction()`, `prepare_write()`, `find_records_by_key()`,
  `fetch_record()`, `records_stamp()`, `table_exists()`, `db_backend()`
  (`?db_helpers`); `validation_issue()`, `form_message()`
  (`?validation_helpers`); `with_language()`, `ui_labels()`, `ui_label()`,
  `register_texts()` (`?language_helpers`); `module_permission()`,
  `module_editable_fields()`, `apply_edit_rules()` (`?module_helpers`).
* `register_texts()` adds label and message keys of an extension to
  `english()` and `german()`; `language()` accepts them.

# shinyformtools 0.7.1

Fixes from a fifth review (code review since 0.5.2, basket and board under
attack on three backends, compatibility with 0.5.2, documentation).

* Fixed: numbers written into text fields lost digits past 15 since 0.5.4
  (1234567890123456 became 1.23456789012346e+15), so two account numbers
  could hit the same upsert key and one record overwrote the other. Whole
  numbers are stored as plain digits, small decimals as 0.0001, others with
  as many digits as they need. Upsert keys also find the spellings earlier
  versions stored (7.0, 1.0e-05, 1e15, DuckDB's true / false).
* Fixed: text in a number field is refused on update and on upsert updates
  as on insert; it was decoded to NA first and then stored (SQLite) or hit a
  raw driver error. Only decimal notation counts as a number ("0x1A" is
  refused).
* Fixed (basket): the stock of a catalog is shared by every basket field of
  every form that names it; before, two forms or two fields of one record
  could each give out the whole stock. Restoring an older version of a live
  record checks the stock. Counting is done in one pass (5000 records: 5 s
  to well under 1 s). Odd values from scripts or crafted clients give
  messages instead of raw errors; a missing catalog table names the form to
  initialise; duplicate catalog ids count once. A basket field asks for its
  palette itself, so it is filled in the inline layout too.
* Fixed (board): a move needs a board on the page and the rights to see the
  table and the record; it recomputes derived fields and leaves a basket
  that `dynamic_visibility()` hides alone.
* Fixed (MariaDB): two writers waiting for each other's basket lock and row
  lock are resolved by a retry after 3 s instead of failing after 10 s.
* Examples: in `app_language` the live form now refreshes the other two as
  well. README lists the basket field and `app_cart_input`.

# shinyformtools 0.7.0

* New: `form_ui(show_board = TRUE)` for a form with a `cart_input` field.
  Every record shows as a drop target on the left, the stock on the right
  with what is still free. One drag moves one piece: from the stock onto a
  record, from one record to another, or back onto the stock. Each move is
  saved at once in one transaction, with the same stock check and rights as
  the dialog; a search box filters the records, and moves made in other
  sessions show within a few seconds. Lines kept on site show hatched and
  stay where they are. Off by default; `app_cart_input` turns it on.
* Fixed: a basket field given `NA` from a script (to empty it) failed the
  stock check.

# shinyformtools 0.6.0

* New: a basket field, `input_type = "cart_input"` (also `cart_input()` /
  `update_cart_input()` for plain Shiny apps). Items are dragged from a
  palette into the basket or clicked, `+` and `-` set the count, and each
  line can be marked as kept on site. The palette is fixed
  (`args = list(items = )`) or read from another form (`cart_catalog()`),
  with name, icon or emoji and stock. `form_server()` shows how many of each
  item are still free (stock minus what the other live records hold; lines
  kept on site do not count), and a save or restore that would take more is
  refused with a validation error, also when two sessions take the last one
  at the same moment. Stored as JSON, shown in the table as
  "2 × Scope, 1 × Compass". Example: `app_cart_input`.
* Fixed: with `form_server(language = )` a records table refresh named the
  columns in the default language, so the table showed no rows after the
  first save when system columns were visible (since the language object
  was introduced). Apps that set the language globally with `use_german()`
  were not affected.

# shinyformtools 0.5.4

* New: `form_server()` returns `saved`, which moves only when the module
  itself wrote. `changed` also moves on every refresh, so two forms listening
  to each other's `changed` refreshed each other forever; `saved` is meant
  for that. `app_language` uses it.
* Changed (grid): a row the form's rules refuse stays in the grid with its
  message, and the other rows of the save are stored. Before, one refused
  row dropped the whole save, and after a group switch the valid rows were
  gone without a word.
* Changed: a number a script writes into a text field is stored as its
  digits on every backend (`7`, `100000`); SQLite and DuckDB stored `7.0`,
  MariaDB `7`. Upsert keys still find rows stored as `7.0`.
* Changed: with `german()` the slide wizard shows "Zurück" / "Weiter".
  English forms render as before.
* Docs: `fetch_records(include_deleted =)` values, restoring a delete
  version, the xlsx cell limit. Several help pages that 0.5.3 corrected in
  the source were regenerated.

# shinyformtools 0.5.3

Fixes and speedups only; apps that worked before render and store the same.
Found by a second multi-agent review (database layer on three backends,
migrations and upgrades from 0.3.1 and 0.4.0, module and permissions, grid in
a browser, rendering and injection, documentation, resources).

* Fixed: a write whose validation raised a warning (a `warning_if()` rule,
  `on_edit_missing_required = "warn"`), called inside `tryCatch(warning = )`,
  left its transaction open; the record was not stored and every later write
  on the session's connection failed. Warnings now reach the caller after
  the commit, with the same class and message.
* Fixed: a permission function that fails no longer ends the Shiny session;
  it denies and shows the error.
* Fixed: with `permissions = list(can_view_table = FALSE)` a client could
  still open, delete or read records by sending a row index.
* Fixed: a records table that could not be built at session start
  (database refused) stayed empty until a reload; it is built on the next
  refresh.
* Fixed: with `table = list(refresh_delay = )` a row index refers to the rows
  the browser shows, not to newer data.
* Fixed: `dynamic_visibility()` and `dynamic_value()` handlers re-evaluated
  on save received only `context$form`; they now get the context
  `?hook_context` describes, as in the dialog.
* Fixed: text in a number field from a script ("abc", "1,5") is refused with
  a validation error on every backend; SQLite stored it and read it back as
  0. Blank text is stored empty.
* Fixed: a range slider given `NA` stored the text `["NA","NA"]`; a time
  given to a date field stored the UTC date; a time stored as "HH:MM" was
  reset to 00:00:00 by the next edit; a rule reading `values$tag` saw the
  field `tags` when `tag` was empty.
* Fixed: two forms on one table dropped each other's unique index on every
  call. Documented that a table belongs to one form.
* Fixed: `plan_migration()` / `apply_migration()` refused a pre-0.4.1
  database with a range slider that `init_db()` accepts.
* Fixed: two processes setting up an empty database at the same time made
  one of them fail with a misleading message.
* Fixed: field columns that differ only in case are refused by `form()`; a
  field renamed only in case explains itself instead of failing every call.
* Fixed: DuckDB audit rows of an insert listed `sft_id` as changed.
* Fixed (grid): after a refused save the grid sent its value again, which
  doubled the notice and named the user's own rows as changed by someone
  else; a 0 typed where no record is vanished at the next poll.
* Fixed (examples): `app_language` refreshed its forms forever after the
  first add; `app_custom_input`'s multi-select picker allowed one genre and
  dropped the others on edit; texts of `app_field_control`,
  `app_shinymanager`, `app_table_style` corrected.
* Faster: writes quote column names in one call (an insert on a 30-field
  form 16 -> 10 ms); a closed edit dialog's version table is no longer
  rebuilt after the save.
* `upsert_records()` returns invisibly, as documented. `example_path()`
  lists the examples for a mistyped name.
* Docs: forms share one connection per session and database (since 0.5.0);
  `permissions = perms` for the adapters; `editable` is not enforced on
  script inserts; a later `db_default` change does not reach existing
  tables; MariaDB unique values ignore trailing spaces; going back to a
  version before 0.4.1; `default_view` needs `persist`.

# shinyformtools 0.5.2

* New: `form_server(table = list(refresh_delay = ))` in milliseconds,
  default `0` (unchanged). Above 0 the records table is rebuilt at most once
  in that time: the first refresh shows at once, further ones are collected
  into one at the end of the window. Meant for a table next to a grid whose
  autosave refreshes it; `1000` is used in `app_grid_entry`. Only the
  display is throttled; conflict checks and the selected record stay
  current.
* Docs: `?connections` explains why an app update must not leave an old
  version running on the same database (restart Shiny Server), and
  `form_field(db_default =)` says the add dialog does not preselect it
  (use `args`).

# shinyformtools 0.5.1

Fixes and speedups only; existing apps render and store the same as with
0.5.0. Found by a multi-agent review (code review, cross-backend fuzzing,
feature combinations, performance under 1 to 40 sessions).

* Fixed: on DuckDB, adding a new field with `unique = TRUE` to an existing
  form broke every call on it ("Referenced column not found").
* Fixed: a nested `with_transaction()` (or package write inside one) that
  failed while the caller caught the error kept its partial writes; the
  whole block now rolls back and raises that error.
* Fixed: a grid saved inside `with_transaction()` (e.g. `confirm()` in a
  "done" handler) took its values as stored even when the block rolled back
  or was retried, so they were never written. The grid now takes them as
  stored, reports `saved` and bumps `changed` only after the commit.
* Fixed: pasting a single spreadsheet column with empty cells into a grid
  kept the old values in those cells.
* Fixed: an emptied grid row whose record held only an unticked checkbox was
  kept as an empty record instead of being deleted.
* Fixed: a poll answer that arrived after a group switch caused a false
  "someone else changed" message in the new group.
* Fixed: with `schema_policy = "manual"`, old 4-byte DuckDB number columns
  and numeric range-slider columns stopped every call after the update to
  0.4; they work as before until `init_db()` widens them.
* Fixed: SQLite "database is locked" after the full busy timeout was retried
  five times, blocking the R process (and every session in it) for 25 s.
* Fixed: a `numericInput` field without `args$value` could not open the add
  dialog ('argument "value" is missing').
* Fixed: an empty date field wrote a Shiny warning to the log on every
  render.
* Fixed: `upsert_records()` with a value for a locked field wrote an audit
  row without changes on every call.
* Fixed: `integer64` values were stored as their raw bits on DuckDB.
* Fixed: checkbox values 2, -1, 0.5 or "1.0" were stored as 0.
* Fixed: with `table = list(checkbox_labels = TRUE)`, the records table went
  back to 0 / 1 after its first refresh.
* Faster: edit dialogs no longer convert the record once per field (300
  fields: 0.81 s to 0.33 s); timestamp formatting in the table ~15 % faster;
  a records table that is hidden (another tab) is not rebuilt on every
  refresh, only once when it is shown again.

# shinyformtools 0.5.0

* Fixed: what a user typed into a grid and then left for another group
  within half a second (the autosave's wait) was lost: the new grid's first
  value replaced it. The replaced grid now sends it at once, and it is saved
  to the group it was typed in. Found by a simulation of twelve counters
  switching stations quickly: 21 of 21 quick switches had lost the station's
  entries, now none.
* New: `grid_server(col_key =, value =)` for data stored long, one record per
  cell (a species' count in one distance band): the columns come from the values of
  `col_key` (given in `cols` like `rows`), each cell is its own record with
  its number in `value`, and an emptied cell's record is soft-deleted.
  Rights, conflict check, polling, `saved` and `confirm()` work per cell.
* Fixed: grid rows given as a named vector (`c("Species A" = "A")`) showed the
  values instead of the names as labels.
* New: `grid_server()` returns `saved`, which fires after every successful
  save, also one that wrote nothing (group, rows written, whether the grid
  is empty, whether it was confirmed), and `confirm(empty = FALSE)` for a
  "done" or "nothing found here" button. An app can now tell a group that
  was visited and found empty from one nobody opened; before, an empty grid
  wrote nothing and reported nothing.
* New: `with_transaction(conn, { ... })` runs several writes as one: the
  package's write functions inside join the transaction, as do your own DBI
  statements on `conn`, so either everything is stored or nothing. Collisions
  retry the whole block. Before, `insert_record()` and friends always opened
  their own transaction and could not be combined with other writes.
* Changed: all `form_server()` and `grid_server()` modules of one Shiny
  session that use the same database share one connection, opened by the
  first and closed when the session ends. A page with several forms cost one
  connection per form and user before, which ran a MariaDB server into its
  connection limit. `options(shinyformtools.share_connections = FALSE)`
  restores one per module.

# shinyformtools 0.4.2

* Fixed: `collect_input_values()` returned empty inputs as `NULL` entries
  since 0.4.0. Apps that call it and compare its result saw fields they did
  not before. It leaves them out again by default, as in 0.3.1; the new
  `keep_empty = TRUE` keeps them, and the module's edit dialog uses that, so
  clearing a multi-select or date there still stores it as empty.

# shinyformtools 0.4.1

* Changed: a locked field filled by `dynamic_value()` is recomputed on edit
  only when the edit changes one of its `depends_on` fields, in the dialog
  and on save. An edit of anything else keeps the stored value: past values
  are not rewritten because a formula or the data it reads changed since.
  (0.4.0 recomputed it on every edit.)
* Fixed: a slider with a range could not be saved on DuckDB or MariaDB
  (the column was a number, the value a JSON array). The column is text
  there now; existing columns are changed on first access, without loss.
* Fixed: on DuckDB, switching a field to `unique = TRUE` failed as soon as
  the table had deleted records or empty values in that field.
* Fixed: saving the edit dialog of a record with an empty checkbox or slider
  stored the unchecked box or the slider's default, although nobody set it.
  A field stored empty that comes back exactly as the dialog showed it is
  not written.

# shinyformtools 0.4.0

* New: grid entry over several records. `grid_ui()` / `grid_server(id, form,
  rows, key, group, ...)` show one record per row of a grid (a group's
  species, an order's items) with the form's numeric fields as columns, and
  save the rows the user changed through `upsert_records()` in one
  transaction, half a second after the last change or on a Save button.
  Emptied rows are soft-deleted, a `hint` matrix shows reference values
  greyed, the returned `changed` refreshes other modules. A row someone else
  changed in the meantime is not overwritten: the save stops, the user is
  told which row, and the grid shows the current values; the check works per
  cell and can be switched off (`conflict_check = FALSE`, the last save of a
  cell wins; emptying a row stays checked). Every `poll` seconds (default 3)
  the grid shows what others saved, in cells the user has not changed and
  is not typing in. `permissions` works
  as in `form_server()` (`can_add`, `can_edit`, `can_delete`,
  `editable_fields`), and fields the user may not edit show as locked
  columns. Numbers pasted from a spreadsheet may carry thousands separators:
  in a whole-number column "1.234" and "1,234" are 1234; in a decimal column
  the browser's language decides which of "." and "," is the decimal mark;
  "1.234,5" and "1,234.5" are 1234.5 everywhere. Example `app_grid_entry`.
* New: `grid_input()`, a table of number cells as a Shiny input (Enter and
  arrow keys walk the cells, a block pasted from a spreadsheet lands from the
  focused cell, row / column / total sums follow the entries, a `hint` matrix
  shows greyed reference values); `update_grid_input()` sets values and hints
  from the server. Also a built-in `input_type = "grid_input"` for
  `form_field()`: stored as one JSON text, shown as column totals.
* New: `upsert_records(form, records, key, ...)` writes several records in
  one transaction, matched to the stored rows by key fields: insert what is
  new, update what changed, leave the rest alone, all through the usual
  validation and audit log. `empty` soft-deletes rows that count as empty
  (a grid of counts stores only what was entered), `scope` soft-deletes stored
  rows of a group that the call did not write, and `expected` stops the call
  with `sft_edit_conflict` when a row is no longer stored the way the caller
  saw it. On MariaDB, calls for the same table wait for each other through a
  named lock, so two users who save the same new group at once cannot both
  insert it. First building block of grid entry over several records.
* New example `app_matrix_input`: a cross table (product x quarter) as a single
  field via `register_input()` and `shinyMatrix::matrixInput`, stored as JSON,
  shown as totals, validated cell by cell.

* Fixed: the edit-conflict check compared numbers to 7 significant digits,
  so a concurrent change from 100000.01 to 100000.02 went unnoticed and was
  overwritten. It compares 15 digits now.
* Fixed: clearing a checkbox group, a multi-select or a date in the edit
  dialog kept the stored value while the record was reported as updated,
  and a cleared mandatory multi-select passed validation.
* Fixed: audit snapshots rounded numbers to 4 decimals, so restoring wrote
  3.1416 back for 3.14159265; stored multi-value numbers (a slider range)
  were rounded the same way. Full precision now.
* Fixed: DuckDB stored numeric fields as 4-byte floats (123456789 became
  123456792). They are DOUBLE now; existing columns are widened
  automatically on the next access, without loss.
* Fixed: restoring a record went back past `attach_shapes()` and cleared its
  geometry.
* Fixed: switching a field with two or more empty values to `unique = TRUE`
  failed and took every later call on the form down with it.
* Fixed: a validation message that sounds like a database conflict ("must be
  unique") was retried and reported as "someone else was saving". SQLite's
  "database is locked" is retried instead of shown raw.
* Fixed: `shinymanager_permissions()`, `rights_permissions()` and
  `permissions_form()` did not know `can_export`, so every user could
  export. They do now; an absent column or rule keeps export allowed as
  before, and the rights form gets a checked box.
* Fixed: restoring an older version of a record needed only `can_restore`
  (default `TRUE`). It needs `can_edit` as well now and is refused when the
  version differs in a field the user may not edit.
* Fixed: `update_record()` dropped a field whose `editable` is a function
  even for a user it allows; it is decided for `user` now.
* Fixed: a locked field filled by a `dynamic_value()` binding (a total from
  price and quantity) was recomputed when a record was added but not when it
  was edited, so it kept its value from the insert while its inputs changed.
  It is recomputed on edit now, from the stored record overlaid with what the
  user submitted: a field the user may not edit or does not see counts with
  its stored value, never as empty.
* Fixed: a record's empty values opened in the edit dialog with the inputs'
  own defaults (today for a date, the first radio choice, a number's
  `value`), and saving stored them in fields the user never touched. Empty
  values render as empty inputs now.
* Fixed: a client could set `include_deleted` without
  `can_view_deleted_records` and see deleted records in the table and the
  export.
* Fixed: restoring versions from before a field became unique stored empty
  values as '' and collided in the unique index.
* Fixed: on edit, a derived field whose `editable` is a function was
  recomputed over a value a permitted user had set, and visibility rules
  judged only the submitted values, so a field shown because of a locked
  field lost what the user typed into it.
* Fixed: widening DuckDB number columns failed on tables with a unique
  index, and every call then ran the schema migration again.
* Faster: the records table formats per column and each distinct value
  once. A refresh of 5000 records went from 370 ms to 33 ms; it runs on
  every save of a module wired through `refresh_triggers` and blocks every
  session of the app while it runs.

# shinyformtools 0.3.1

* New, off by default: `form_server(table = list(checkbox_labels = TRUE))`
  shows checkbox fields in the records table as the language's yes / no
  labels ("Yes" / "No", "Ja" / "Nein") instead of 0 / 1. The export keeps
  writing `TRUE` / `FALSE`.

* Housekeeping, no change in behaviour: the weekly integration workflow now
  runs the live MariaDB tests (it started a server but set the wrong variable
  names); every tracked file is stored with LF line endings; five internal
  helpers that nothing called were removed.
* Fixed: numeric fields reached the records table as text, so DT sorted them
  as text ("100" before "35") and drew a text box instead of a range slider
  for the column filter. They stay numeric now.
* Fixed: a records table with a column filter (`table = list(filter = )`)
  loads DT's older selectize, which then broke every select input in the add
  and edit dialog under Shiny 1.13 (the dialog stayed unbound and could not
  save). `form_ui()` and `form_buttons()` now put Shiny's own selectize on the
  page first.
* Fixed: a dynamic choice list that becomes empty (`dynamic_choices()` with a
  server-side selectize) raised a script error in the browser.
* Slide headings: `form(slide_labels = )` and `form_field(slide_label = )` were
  stored but never shown. A multi-slide form now renders the label as a heading
  above its slide. Slides without a label get no heading, so existing slide
  forms render as before. New example `app_presentation_german`.

# shinyformtools 0.3.0

* New: export to CSV and Excel. `export_records(form, "people.xlsx")` writes a
  form's records from a script; `form_ui(show_export = TRUE)` adds download
  buttons to the module (off by default). The file has field labels as
  headers, multi-value fields as readable text instead of JSON arrays, numbers
  as numbers, checkboxes as `TRUE` / `FALSE` and timestamps in local time. CSV
  files carry a UTF-8 byte order mark so that Excel reads umlauts correctly;
  `"csv2"` is the semicolon / decimal-comma dialect for European Excel. The
  module's download holds what the table holds: the visible columns and, when
  the user has searched the table, the matching rows. New permission
  `can_export` (default `TRUE`; it also requires `can_view_table`).

# shinyformtools 0.2.2

* Fixed: the audit log listed EVERY submitted field of an update as changed.
  The edit form submits all fields on every save, so each update claimed to
  have changed all of them, the changelog showed them all, and the conflict
  view ("last changed by") credited the last saver with columns somebody else
  had changed. Updates and restores now log only the fields whose stored value
  really differs. Entries written before this fix keep their long lists.
* `changed_fields_json` is always a JSON array. A single changed field used to
  be written as a bare JSON string. Both shapes are still read.
* New help pages: `?connections` (who opens and closes a connection, with and
  without `conn`, in scripts and in the module) and `?hook_context` (what the
  `context` argument of `display_transform`, `modal_header`, `table$format` and
  input-binding handlers holds).

# shinyformtools 0.2.1

* Fixed: a checkbox field written from a script with `1`, `1L`, `"1"` or
  `"TRUE"` was stored as 0 ("no"). Only `TRUE` counted. Storing and reading now
  share one notion of "true". Forms filled in through the app were never
  affected, because a checkbox input delivers `TRUE` / `FALSE`.
* `fetch_records(include_deleted = "only")` returns just the soft-deleted
  records, filtered by the database. The deleted-records dialog uses it instead
  of fetching the whole table.
* Internal: three long database functions were split into named steps. No
  change in behaviour, no schema change, nothing re-migrates.

# shinyformtools 0.2.0

## Argument clean-up

* `form_server()` now takes four option bundles instead of ~30 flat arguments:
  `permissions = list(can_add = , ..., hide_forbidden = , editable_fields = )`,
  `table = list(options = , class = , filter = , format = , audit_options = ,
  version_options = , deleted_records_options = , datetime_format = )`,
  `columns = list(visible = , views = , default_view = , persist = ,
  show_system = , labels = )` and `highlight = list(fields = , tab = , color = ,
  show_changed = , changed_color = )`. The adapters `shinymanager_permissions()`
  and `rights_permissions()` return a `permissions` list directly, so
  `form_server(id, form, permissions = rights_permissions(...))` replaces the
  `do.call()` idiom. Every old flat argument still works and warns once per
  session; a misspelt bundle key is an error.
* `form_server(show_audit = )` is gone: the audit output is always registered
  and Shiny computes it only when `form_ui(show_audit = TRUE)` placed it.
* `form_server(form_layout = )` is optional: the server reads the layout that
  `form_ui()` rendered from a hidden input, so the two no longer have to match
  by hand.
* `inspect_schema()`, `plan_migration()` and `apply_migration()` take
  `(form, conn)` like every other function. The old `(conn, form)` order is
  recognised and warns.
* `validate_record(current_id = )` is `record_id = `; `shinymanager_users(db = )`
  is `credentials = `; `IBANInput()` / `updateIBANInput()` are `ibanInput()` /
  `updateIbanInput()`. Old names still work and warn.
* `form_ui(show_versions = )` and `form_ui(show_include_deleted = )` are
  deprecated (use `show_deleted_records`).
* `form_server()` returns `connection()`, a function yielding the module's
  current connection, next to the start-up `conn`.

## Languages

* New `language()` object: UI labels, validation messages, table labels and the
  'DataTables' chrome of a form in one argument, `form_ui(language = )` /
  `form_server(language = )`. It is scoped to that form, so one app can serve
  forms or users in different languages, which the process-wide `use_german()`
  cannot. `german()` and `english()` are ready-made; `language_keys()` lists
  every key with its English default. Example: `app_language`.
* `form_server(language = )` also accepts a function or reactive returning a
  language, so a user can switch language while the app runs.
* Text that was hard-coded English now follows the language: the fallback names
  of unnamed tabs and wizard slides, the changelog box (title, empty text,
  "+N more") and two uniqueness-rule messages.
* Three label keys that nothing read any more are gone from the defaults:
  `apply_columns`, `apply_column_selection`, `reset_columns`.

## Validation

* A rejected save now marks its fields: the add/edit form glows the fields the
  failed checks name (missing mandatory fields, taken unique values, the
  `fields` of a failed rule) until the next successful save.
  `form_server(highlight = list(invalid = FALSE))` turns it off.
* New `validation_issues()`: the non-throwing counterpart of
  `validate_record()`, returning one row per issue with `severity`, `source`,
  `message` and `fields`.
* The error `validate_record()` (and therefore `insert_record()` /
  `update_record()`) raises is of class `sft_validation_error` and carries
  `issues` and `fields`. Its message is unchanged.

## Input types

* `register_input()` gained `db_type`: the default column type of fields using
  that input, so a numeric widget no longer needs `db_type = "REAL"` on every
  `form_field()`. A `db_type` on the field still wins.
* A two-handle `sliderTextInput` is restored to its range in the edit dialog
  and shown as "from - to" in the tables (it came back as a raw JSON string),
  and `sliderTextInput` now supports `dynamic_choices()`.
* `dynamic_value()` works on choice inputs (`selectInput`, `selectizeInput`,
  `radioButtons`, `checkboxGroupInput`, `multiInput`, `sliderTextInput`); it
  used to stop with "not supported yet". Returning an empty value clears the
  selection.

## Performance

* New option `shinyformtools.schema_probe_ttl` (seconds, default 0 = off):
  remember a passed schema check per database and form definition. Meant for a
  remote database, where the check costs about 12 round trips per call; with
  `30`, a read drops from 15 round trips to 3 and an update from 19 to 7. A
  schema change made by another process is then noticed up to that many seconds
  late.

## Bug fixes

* A checkbox field stored `0` for every value that was not literally `TRUE`:
  a script inserting `1`, `"1"` or `"TRUE"` - an import from a table, say -
  silently saved "no". Storing and reading back now share one notion of true
  (`TRUE`, a non-zero number, `"1"`, `"true"`, `"yes"`). The checkbox in the app
  always delivered `TRUE` / `FALSE` and was never affected.
* `fetch_records(include_deleted = "only")` returns just the soft-deleted
  records, filtered by the database; the deleted-records dialog uses it instead
  of fetching the whole table.

* A write that loses its race five times in a row now fails with a readable
  message ("someone else was saving the same data at the same moment ... please
  save again") and the class `sft_write_conflict`, instead of the database's
  raw text. The original message is kept in parentheses.
* The schema check that precedes every database call no longer lists the
  tables twice; on MariaDB that is three round trips less per operation.

* A write that loses a race now waits a short, random moment before it
  retries. Retries used to fire at once, so writers that had collided collided
  again; measured with 8 processes updating one record, failed updates fell
  from 45% to 22%. No write was ever lost either way - a failed update raises
  an error.

* A database that refuses connections (MariaDB at `max_connections`) no longer
  takes user sessions down. Measured with a live server: a save in a session
  whose connection had been dropped lost the input and left the save button
  dead, a refresh killed the session through the table-sync observer, and a new
  session died at start-up with the driver's message. Now the save reports the
  error and keeps the dialog, the next save works once capacity is back, and a
  session that cannot connect stays up, says so, and connects on the next
  action. The README gained a "Connections" section on sharing one connection
  between forms.

* After a server-dropped connection was healed, the audit table, the
  deleted-records dialog, restore, saved column views and the conflict
  attribution kept using the dead handle. Every part of the module now reads
  the connection through one accessor.
* Validation warnings (`warning_if()`, `on_edit_missing_required = "warn"`)
  are shown to the user as warning notifications; the save still completes.
  They used to reach only the R console.
* A stored time value nothing could parse was replaced by the current time in
  the edit conflict view; the input now keeps its state.
* `form(schema_policy = "manual")` had no effect. It now stops CRUD calls on a
  missing or outdated schema with a hint to run `init_db()`.
* On update and restore, validation ran on the stored representation of a
  record: a unique multi-value field (checkbox group, multi-select) never hit
  the friendly "already taken" message but the raw index error, and a
  validation rule saw a JSON string where it sees a vector on insert. Stored
  rows are now decoded back to input values before validation.

# shinyformtools 0.1.0

First public release.

* Declarative core: describe a form once with `form()` / `form_field()`; the
  database schema, CRUD, rendering, and the Shiny module are derived from that
  description.
* Database-backed CRUD with soft delete only, a full audit log, and record
  restore from any past version.
* Additive schema migrations reconciled to the form definition, with
  database-level uniqueness via composite unique indexes.
* Configurable, user-saveable table views; dynamic and cross-referencing inputs
  (`dynamic_choices()`, `dynamic_value()`, `reference_choices()`) and display-only
  derived columns via `display_transform`.
* Server-side validation (`validation_rule()`, `required_if()`, `forbid_if()`,
  `warning_if()`) that cannot be bypassed from the client.
* Permissions: per-action `can_*` controls on `form_server()`, a rights-table
  model (`permissions_form()` / `rights_permissions()`), and a `shinymanager`
  adapter.
* Shape fields: attach a fixed, non-editable geometry to each record with
  `shape_field()` / `attach_shapes()`, stored backend-neutrally as text.
* Three backends behind one interface: 'SQLite', 'MariaDB', and 'DuckDB'.
* Bundled, runnable example apps (`list_examples()` / `run_example()`).
