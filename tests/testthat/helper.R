skip_if_not_long_tests <- function() {
  if (Sys.getenv("RUN_LONG_TESTS") != "true") {
    testthat::skip("Long tests not enabled. Set RUN_LONG_TESTS=true to run.")
  }
}

# Count how many times an internal codeminer function is called while evaluating
# `expr`. Used to assert that the metadata resolvers are invoked only once per
# query (see #163).
count_codeminer_calls <- function(fn_name, expr) {
  n <- 0L
  trace(
    fn_name,
    tracer = function() n <<- n + 1L,
    where = asNamespace("codeminer"),
    print = FALSE
  )
  on.exit(untrace(fn_name, where = asNamespace("codeminer")), add = TRUE)
  suppressWarnings(suppressMessages(force(expr)))
  n
}
