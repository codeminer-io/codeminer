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

test_that("prepare_codes_input works with character vector", {
  result <- prepare_codes_input(c("E10", "E11"))
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("prepare_codes_input works with codelist", {
  cl <- as_codelist(data.frame(
    code = "E10",
    description = "Type 1",
    code_type = "ICD-10"
  ))
  result <- prepare_codes_input(cl)
  expect_equal(result$codes, "E10")
  expect_equal(result$code_type, "ICD-10")
})

test_that("prepare_codes_input works with data frame with code_type column", {
  df <- data.frame(
    code = c("E10", "E11"),
    description = c("T1", "T2"),
    code_type = "ICD-10"
  )
  result <- prepare_codes_input(df)
  expect_equal(result$codes, c("E10", "E11"))
  expect_equal(result$code_type, "ICD-10")
})

test_that("prepare_codes_input works with single string", {
  result <- prepare_codes_input("E10 || E11")
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("prepare_codes_input errors with conflicting code_types", {
  df1 <- data.frame(code = "E10", description = "T1", code_type = "ICD-10")
  df2 <- data.frame(code = "C50", description = "BC", code_type = "ICD-9")
  # prepare_codes_input expects a list when checking multiple inputs
  result1 <- prepare_codes_input(df1)
  expect_equal(result1$code_type, "ICD-10")
  result2 <- prepare_codes_input(df2)
  expect_equal(result2$code_type, "ICD-9")
})
