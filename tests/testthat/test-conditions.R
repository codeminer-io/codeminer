test_that("codeminer_abort creates a CodeMiner error condition", {
  expect_error(
    codeminer_abort(c(x = "Test error.")),
    class = "codeminer_error"
  )
})

test_that("codeminer_abort stores the original cli message vector in cli_error_message", {
	e <- tryCatch(
		codeminer_abort(c(x = "Test error.", i = "Extra info.")),
		error = function(e) e
	)
	expect_true("cli_error_message" %in% names(e))
	expect_equal(e$cli_error_message, c(x = "Test error.", i = "Extra info."))
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
