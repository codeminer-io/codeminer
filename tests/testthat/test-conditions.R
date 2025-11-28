test_that("codeminer_abort creates a CodeMiner error condition", {
  expect_error(
    codeminer_abort(c(x = "Test error.")),
    class = "codeminer_error"
  )
})

test_that("codeminer_abort interpolates variables in vector and matches cli_abort output", {
  invalid_code_type <- "foo"

  original_cli_message <- c(
    x = "Code type {.arg {invalid_code_type}} not found.",
    i = "Use `add_lookup_table()`."
  )

  e <- tryCatch(
    codeminer_abort(
      original_cli_message,
      class = "codeminer_arg_validation_error"
    ),
    error = function(e) e
  )

  # check that the captured interpolated error message vector matches the
  # original error message vector (also interpolated)
  expect_identical(
    e$cli_message,
    codeminer_interpolate_message(original_cli_message)
  )

  # check with `conditionMessage()` that the captured error message matches the
  # original error message
  f <- tryCatch(cli::cli_abort(e$cli_message), error = function(e) e)

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

test_that("codeminer_warn interpolates variables, sets class, and stores cli_message", {
  var <- "bar"
  original_cli_message <- c(
    x = "Warning for {.val {var}}.",
    i = "Check your input."
  )
  w <- tryCatch(
    codeminer_warn(
      original_cli_message,
      class = "codeminer_test_warning"
    ),
    warning = function(w) w
  )
  expect_identical(
    w$cli_message,
    codeminer_interpolate_message(original_cli_message)
  )
  expect_true("codeminer_test_warning" %in% class(w))
  expect_true("codeminer_warning" %in% class(w))

  x <- tryCatch(cli::cli_warn(w$cli_message), warning = function(cnd) cnd)
  expect_identical(conditionMessage(w), conditionMessage(x))
})

test_that("codeminer_inform interpolates variables, sets class, and stores cli_message", {
  info <- "baz"
  original_cli_message <- c(
    x = "Inform: {.val {info}}!",
    i = "Just FYI."
  )
  m <- tryCatch(
    codeminer_inform(
      original_cli_message,
      class = "codeminer_test_message"
    ),
    message = function(m) m
  )
  expect_identical(
    m$cli_message,
    codeminer_interpolate_message(original_cli_message)
  )
  expect_true("codeminer_test_message" %in% class(m))
  expect_true("codeminer_message" %in% class(m))

  n <- tryCatch(cli::cli_inform(m$cli_message), message = function(cnd) cnd)
  expect_identical(conditionMessage(m), conditionMessage(n))
})
