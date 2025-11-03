## Set up dummy database
create_dummy_database()

test_that("`CODES()` returns the expected number of results", {
  res <- CODES(codes = c("C10E.", "C108."), code_type = "read2")
  expect_equal(nrow(res), 7)

  res <- CODES(codes = c("C10E.", "C108."), code_type = "read2")
  expect_equal(nrow(res), 2)
})

test_that("`CODES()` returns the expected columns", {
  result <- CODES(c("E10", "E100"), code_type = "icd10")

  expect_equal(names(result), c("code", "description", "code_type"))

  expect_identical(
    result$description,
    c(
      "Type 1 diabetes mellitus",
      "Type 1 diabetes mellitus With coma"
    )
  )
})
