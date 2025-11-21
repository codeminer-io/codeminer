test_that("codeminer_abort creates a CodeMiner error condition", {
  expect_error(
    codeminer_abort(c(x = "Test error.")),
    class = "codeminer_error"
  )
})

test_that("codeminer_abort interpolates variables in vector and matches cli_abort output", {
  invalid_code_type <- "foo"

  original_cli_error_message <- c(
    x = "Code type {.arg {invalid_code_type}} not found.",
    i = "Use `add_lookup_table()`."
  )

  e <- tryCatch(
    codeminer_abort(
      original_cli_error_message,
      class = "codeminer_arg_validation_error"
    ),
    error = function(e) e
  )

  # check that the captured interpolated error message vector matches the
  # original error message vector (also interpolated)
  expect_identical(
    e$cli_error_message,
    vapply(
      original_cli_error_message,
      cli::format_inline,
      character(1),
      .envir = rlang::current_env()
    )
  )

  # check with `conditionMessage()`` that the captured error message matches the
  # original error message
  f <- tryCatch(cli::cli_abort(e$cli_error_message), error = function(e) e)

  expect_identical(conditionMessage(f), conditionMessage(e))
})

test_that("codeminer_abort allows additional custom classes to be prepended", {
  e <- tryCatch(
    codeminer_abort(
      c(x = "Test error."),
      class = c("custom_class1", "custom_class2")
    ),
    error = function(e) e
  )
  expect_true(all(c("custom_class1", "custom_class2") %in% class(e)))
  expect_true("codeminer_error" %in% class(e))
})
