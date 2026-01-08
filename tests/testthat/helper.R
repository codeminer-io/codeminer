skip_if_not_long_tests <- function() {
  if (Sys.getenv("RUN_LONG_TESTS") != "true") {
    testthat::skip("Long tests not enabled. Set RUN_LONG_TESTS=true to run.")
  }
}
