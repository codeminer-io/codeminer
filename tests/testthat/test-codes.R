## Set up dummy database
create_dummy_database()

test_that("CODES() returns the expected data format", {
  test_codes <- c("A028", "U838", "E12", "E106", "O109")
  test_type <- "icd10"

  result <- CODES(test_codes, code_type = test_type, version = "v0")

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("code", "description", "code_type"))
  expect_equal(nrow(result), length(test_codes))
  expect_identical(unique(result$code_type), test_type)
})

test_that("CODES works with the codeminer.code_type option", {
  test_codes <- c("A39", "E149", "M142", "E146", "E141")
  test_type <- "icd10"

  result <- withr::with_options(
    list(codeminer.code_type = test_type),
    CODES(test_codes)
  )
  expect_equal(nrow(result), length(test_codes))
  expect_identical(unique(result$code_type), test_type)
})

test_that("CODES allows querying all codes", {
  test_type <- "icd10"
  expected_rows <- 199

  result <- CODES("all", code_type = test_type)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), expected_rows)
})

test_that("CODES respects versions", {
  test_type <- "icd10"
  test_version <- "v2"

  # add test table as new version of icd10
  test_table <- data.frame(
    code = c("a", "b", "c"),
    description = c("letter a", "letter b", "letter c")
  )
  add_lookup_table(
    test_table,
    lookup_metadata(test_type, version = test_version)
  )

  result <- CODES("all", code_type = test_type, version = test_version)
  expect_identical(result$code, test_table$code)
  expect_identical(result$description, test_table$description)
  expect_identical(unique(result$code_type), test_type)
})

test_that("CODES warns about missing codes", {
  test_codes <- c("foo", "bar")
  expect_warning(
    CODES(test_codes, "icd10"),
    "The following codes were not found in the lookup table: `foo` and `bar`",
    fixed = TRUE
  )
})

test_that("CODES fails for wrong argument types", {
  expect_error(CODES(1:3, "icd10"), "`codes` must be a character vector")
  expect_error(
    CODES(c("a", "b"), code_type = TRUE),
    "`code_type` must be of type character"
  )
  expect_error(
    CODES("all", code_type = c("icd10", "icd11", "icd12")),
    "`code_type` must have length 1"
  )
})

test_that("CODES fails for missing code_type", {
  expect_error(
    CODES("all", code_type = "idontexist"),
    "Code type 'idontexist' not found"
  )
})

test_that("CODES fails for wrong version", {
  expect_error(
    CODES("all", code_type = "icd10", version = "nope"),
    "No metadata found for 'icd10' version 'nope'"
  )
})

test_that("CODES can return multiple descriptions for the same code", {
  test_code <- "X40J4"
  result <- CODES(test_code, code_type = "read3")
  expect_equal(nrow(result), 5)
  expect_identical(unique(result$code), test_code)
})
