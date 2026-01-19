test_that("split_double_pipe works with simple strings", {
  expect_equal(split_double_pipe("E10"), "E10")
  expect_equal(split_double_pipe("E10 || E11"), c("E10", "E11"))
  expect_equal(split_double_pipe("E10 || E11 || E12"), c("E10", "E11", "E12"))
})

test_that("split_double_pipe trims whitespace", {
  expect_equal(split_double_pipe("  E10  ||  E11  "), c("E10", "E11"))
})

test_that("parse_codes works with simple strings", {
  result <- parse_codes("E10")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, "E10")
  expect_equal(result$description, NA_character_)
})

test_that("parse_codes works with double-pipe strings", {
  result <- parse_codes("E10 || E11")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, c("E10", "E11"))
})

test_that("parse_codes works with comment syntax", {
  result <- parse_codes("E10 << Type 1 diabetes >>")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, "E10")
  expect_equal(result$description, "Type 1 diabetes")
})


test_that("parse_codes works with both double-pipe and comments", {
  result <- parse_codes("E10 << Type 1 >> || E11 << Type 2 >>")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, c("E10", "E11"))
  expect_equal(result$description, c("Type 1", "Type 2"))
})
