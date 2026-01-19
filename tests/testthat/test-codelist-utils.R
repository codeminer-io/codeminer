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

test_that("collect_codes_input works with character vectors", {
  result <- collect_codes_input("E10", "E11")
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with double-pipe strings", {
  result <- collect_codes_input("E10 || E11")
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with mixed inputs", {
  result <- collect_codes_input("E10", c("E11", "E12"), "E13 || E14")
  expect_equal(result$codes, c("E10", "E11", "E12", "E13", "E14"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with codelist data frame", {
  cl <- as_codelist(data.frame(
    code = c("E10", "E11"),
    description = c("T1", "T2"),
    code_type = "ICD-10"
  ))
  result <- collect_codes_input(cl)
  expect_equal(result$codes, c("E10", "E11"))
  expect_equal(result$code_type, "ICD-10")
})

test_that("collect_codes_input works with regular data frame with code_type", {
  df <- data.frame(code = c("E10", "E11"), code_type = "ICD-10")
  result <- collect_codes_input(df)
  expect_equal(result$codes, c("E10", "E11"))
  expect_equal(result$code_type, "ICD-10")
})

test_that("collect_codes_input validates type matches codelist", {
  cl <- as_codelist(data.frame(
    code = "E10",
    description = "T1",
    code_type = "ICD-10"
  ))

  # Should work with matching type
  expect_silent(collect_codes_input(cl, type = "ICD-10"))

  # Should error with conflicting type
  expect_error(
    collect_codes_input(cl, type = "Read 3"),
    "Conflicting.*type"
  )
})

test_that("collect_codes_input returns empty for no input", {
  result <- collect_codes_input()
  expect_equal(result$codes, character(0))
  expect_null(result$code_type)
})

test_that("collect_codes_input errors for empty input when not allowed", {
  expect_error(
    collect_codes_input(allow_empty = FALSE),
    "No codes provided"
  )
})

test_that("collect_codes_input errors with non-character non-df input", {
  expect_error(
    collect_codes_input("E10", 123),
    "All inputs must be character"
  )
})

test_that("collect_codes_input errors with multiple data frames", {
  df1 <- data.frame(code = "E10", code_type = "ICD-10")
  df2 <- data.frame(code = "E11", code_type = "ICD-10")

  # Can only accept a single data frame
  expect_error(
    collect_codes_input(df1, df2),
    "All inputs must be character"
  )
})
