## Set up dummy database
create_dummy_database()

test_that("CODES() returns the expected data format", {
  test_codes <- c("A028", "U838", "E12", "E106", "O109")
  test_type <- "icd10"

  result <- CODES(test_codes, code_type = test_type, version = "v0")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
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

test_that("CODES handles versions correctly", {
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

  v2_result <- CODES("all", code_type = test_type, version = test_version)
  expect_identical(v2_result$code, test_table$code)
  expect_identical(v2_result$description, test_table$description)
  expect_identical(unique(v2_result$code_type), test_type)

  latest_result <- CODES("all", code_type = test_type, version = "latest")
  expect_identical(latest_result, v2_result)
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
  expect_error(
    CODES("all", code_type = c("icd10", "icd11", "icd12")),
    "`code_type` must be a string"
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
  result <- CODES(
    test_code,
    code_type = "read3",
    preferred_description_only = FALSE
  )
  expect_equal(nrow(result), 5)
  expect_identical(unique(result$code), test_code)
})

test_that("CODES can return only the preferred description", {
  test_code <- "X40J4"
  result <- CODES(
    test_code,
    code_type = "read3",
    preferred_description_only = TRUE
  )
  expect_equal(nrow(result), 1)
  expect_identical(unique(result$code), test_code)
})

test_that("get_latest_version handles edge cases", {
  # Non-numeric versions should follow alphabetic ordering
  test_versions <- c("aaa", "zzz", "ccc")
  expect_identical(get_latest_version(test_versions), "zzz")

  # But if there's a numeric component, should use that
  test_versions <- c("v1", "v20", "v5")
  expect_identical(get_latest_version(test_versions), "v20")
})

test_that("CODES_LIKE can handle regular expressions", {
  test_pattern <- "^A00"
  result <- CODES_LIKE(
    test_pattern,
    code_type = "icd10",
    version = "v0"
  )
  expect_equal(nrow(result), 4)
  expect_true(all(stringr::str_detect(result$code, test_pattern)))
})
