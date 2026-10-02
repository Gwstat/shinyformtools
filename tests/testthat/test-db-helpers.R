
test_that("column lists quote every column as quoting them one by one did", {
  cols <- c("name", "sft_id", "order", "Mixed Case", "a\"b", "x`y")
  one_by_one <- function(conn) {
    vapply(cols, function(column) as.character(DBI::dbQuoteIdentifier(conn, column)), character(1))
  }
  conns <- list(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
  if (requireNamespace("duckdb", quietly = TRUE)) {
    conns <- c(conns, list(DBI::dbConnect(duckdb::duckdb())))
  }
  on.exit(for (conn in conns) DBI::dbDisconnect(conn))
  for (conn in conns) {
    expect_identical(sft_sql_quoted_columns(conn, cols), paste(one_by_one(conn), collapse = ", "))
    expect_identical(sft_sql_assignments(conn, cols, sep = " AND "),
                     paste(paste0(one_by_one(conn), " = ?"), collapse = " AND "))
  }
})
