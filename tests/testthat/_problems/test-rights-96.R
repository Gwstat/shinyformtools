# Extracted from test-rights.R:96

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
perms <- rights_permissions(data.frame(), user = "u", form_id = "x")
testthat::expect_true(all(names(perms) %in% names(formals(form_server))))
