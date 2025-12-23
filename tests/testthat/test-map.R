## Set up dummy database
create_dummy_database()

test_that("MAP() returns the expected data format", {
  test_codes <- c("C10..", "XE0Uc", "C10..", "C10..", "XE0Uc")
  test_from <- "read3"
  test_to <- "icd10"

  result <- MAP(test_codes, from = test_from, to = test_to, map_version = "v0")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_true(nrow(result) >= length(test_codes))
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP fails for wrong argument types", {
  expect_error(
    MAP("foo", from = c("icd10", "icd11", "icd12"), to = "read3"),
    "`from` must have length 1"
  )
  expect_error(
    MAP("foo", from = "read3", to = c("icd10", "icd11", "icd12")),
    "`to` must have length 1"
  )
})

test_that("MAP fails for missing mapping table", {
  expected_msg <- "No mapping table found"
  expect_error(
    MAP("foo", from = "idontexist", to = "icd10"),
    expected_msg
  )
  expect_error(
    MAP("foo", from = "read3", to = "idontexist"),
    expected_msg
  )
  expect_error(
    MAP("foo", from = "read3", to = "icd10", map_version = "nope"),
    expected_msg
  )
})

test_that("`MAP()` warns about missing codes in the coding system being mapped from", {
  expect_warning(
    MAP(
      codes = c("foo", "bar"),
      from = "read3",
      to = "icd10"
    ),
    "The following codes were not found in the mapping table",
  )
})

test_that("MAP() swaps `to` and `from` if necessary and warns", {
  # We only have read3 -> icd10 mapping in the database,
  # icd10 -> read3 is still expected to work, because MAP() swaps the direction if necessary
  test_codes <- c("E129", "E109", "E14", "L721", "I13") # icd10 codes
  test_from <- "icd10"
  test_to <- "read3"

  expect_warning(
    result <- MAP(test_codes, test_from, test_to),
    "No explicit mapping table found"
  )
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP('all') returns the mapping table", {
  result <- MAP("all", from = "read3", to = "icd10", map_version = "v0")
  expect_identical(result, dummy_read3_icd10_mapping())
})

test_that("MAP handles versions correctly", {
  test_from <- "read3"
  test_to <- "icd10"
  test_version <- "v2"

  # add test tables as new version of icd10
  test_lookup_table <- data.frame(
    code = c("A", "B", "C"),
    description = c("letter A", "letter B", "letter C")
  )
  add_lookup_table(test_lookup_table, lookup_metadata(test_to, test_version))
  test_mapping_table <- data.frame(
    from = c("a", "b", "c"),
    to = c("A", "B", "C")
  )
  add_mapping_table(
    test_mapping_table,
    mapping_metadata(test_from, test_to, test_version)
  )

  v2_result <- MAP(
    "all",
    from = test_from,
    to = test_to,
    map_version = test_version
  )
  expect_identical(v2_result$to, test_lookup_table$code)

  latest_result <- MAP(
    "all",
    from = test_from,
    to = test_to,
    map_version = "latest"
  )
  expect_identical(latest_result, v2_result)
})
