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
