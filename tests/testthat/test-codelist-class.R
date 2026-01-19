test_that("as_codelist creates valid codelist", {
  df <- data.frame(code = "E10", description = "Type 1", code_type = "ICD-10")
  cl <- as_codelist(df)
  expect_s3_class(cl, "codeminer_codelist")
  expect_equal(cl$code, "E10")
  expect_equal(cl$description, "Type 1")
  expect_equal(cl$code_type, "ICD-10")
})

test_that("validate_codeminer_codelist accepts valid codelist", {
  cl <- as_codelist(data.frame(
    code = "E10",
    description = "Type 1",
    code_type = "ICD-10"
  ))
  expect_silent(validate_codeminer_codelist(cl))
})

test_that("validate_codeminer_codelist errors without code column", {
  df <- data.frame(description = "Type 1", code_type = "ICD-10")
  class(df) <- c("codeminer_codelist", "data.frame")
  expect_error(validate_codeminer_codelist(df), "Missing.*code")
})

test_that("validate_codeminer_codelist errors without description column", {
  df <- data.frame(code = "E10", code_type = "ICD-10")
  class(df) <- c("codeminer_codelist", "data.frame")
  expect_error(
    validate_codeminer_codelist(df),
    "Missing.*description"
  )
})

test_that("validate_codeminer_codelist errors without code_type column", {
  df <- data.frame(code = "E10", description = "Type 1")
  class(df) <- c("codeminer_codelist", "data.frame")
  expect_error(validate_codeminer_codelist(df), "Missing.*code_type")
})

test_that("validate_codeminer_codelist errors with non-character code column", {
  df <- data.frame(code = 123, description = "Type 1", code_type = "ICD-10")
  class(df) <- c("codeminer_codelist", "data.frame")
  expect_error(validate_codeminer_codelist(df), "code.*numeric")
})

test_that("validate_codeminer_codelist errors with multiple code_types", {
  df <- data.frame(
    code = c("E10", "C50"),
    description = c("Type 1", "Breast cancer"),
    code_type = c("ICD-10", "ICD-9")
  )
  class(df) <- c("codeminer_codelist", "data.frame")
  expect_error(validate_codeminer_codelist(df), "Found 2 unique values")
})

test_that("print.codeminer_codelist works", {
  cl <- as_codelist(data.frame(
    code = "E10",
    description = "Type 1",
    code_type = "ICD-10"
  ))
  expect_output(print(cl), "ICD-10")
})
