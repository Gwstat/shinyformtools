# Extracted from test-db-unique-index.R:275

# prequel ----------------------------------------------------------------------
sft_test_unique_multi_form <- function(db_path) {
  form(
    form_id = "unique_multi",
    table_name = "unique_multi",
    db = db_sqlite(db_path),
    fields = list(
      form_field(id = "name", label = "Name"),
      form_field(
        id = "tags", label = "Tags", input_type = "checkboxGroupInput",
        args = list(choices = c("a", "b", "c")), unique = TRUE
      )
    ),
    validation_rules = list(
      validation_rule(
        id = "max_two_tags",
        validate = function(values) length(values$tags %||% character()) <= 2L,
        message = "At most two tags."
      )
    )
  )
}

# test -------------------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")
conn <- local_test_conn(db_path)
f <- sft_test_unique_multi_form(db_path)
init_db(f, conn = conn)
rec <- insert_record(f, list(name = "A", tags = c("a", "b")), conn = conn)
testthat::expect_error(
    update_record(f, record_id = rec$sft_id[1], values = list(tags = c("a", "b", "c")), conn = conn),
    "At most two tags"
  )
