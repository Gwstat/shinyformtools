# Extracted from test-permissions.R:22

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "shinyformtools", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
auth <- list(user = "editor", can_add = "TRUE")
perms <- shinymanager_permissions(auth)
server_args <- names(formals(form_server))
testthat::expect_true(all(names(perms) %in% server_args))
