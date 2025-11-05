## Set up dummy database
create_dummy_database()

test_that("MAP() returns the expected data format", {
  test_codes <- c("C10..", "XE0Uc", "C10..", "C10..", "XE0Uc")
  test_from <- "read3"
  test_to <- "icd10"

  result <- MAP(test_codes, from = test_from, to = test_to, version = "v0")

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("code", "description", "code_type"))
  expect_true(nrow(result) >= length(test_codes))
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP fails for wrong argument types", {
  expect_error(
    MAP("all", from = c("icd10", "icd11", "icd12"), to = "read3"),
    "`from` must have length 1"
  )
  expect_error(
    MAP("all", from = "read3", to = c("icd10", "icd11", "icd12")),
    "`to` must have length 1"
  )
})

test_that("MAP fails for missing mapping table", {
  expected_msg <- "No mapping table found"
  expect_error(
    MAP("all", from = "idontexist", to = "icd10"),
    expected_msg
  )
  expect_error(
    MAP("all", from = "read3", to = "idontexist"),
    expected_msg
  )
  expect_error(
    MAP("all", from = "read3", to = "icd10", version = "nope"),
    expected_msg
  )
})
