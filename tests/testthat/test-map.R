## Set up dummy database
suppressMessages(create_dummy_database(.local = TRUE))

test_that("MAP() returns the expected data format", {
  test_codes <- c("C10..", "XE0Uc", "C10..", "C10..", "XE0Uc")
  test_from <- "Read 3"
  test_to <- "ICD-10"

  result <- MAP(
    test_codes,
    from = test_from,
    to = test_to,
    map_version = "UKB v4"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP fails for wrong argument types", {
  expect_error(
    MAP("foo", from = c("ICD-10", "icd11", "icd12"), to = "Read 3"),
    "`from` must be a string"
  )
  expect_error(
    MAP("foo", from = "Read 3", to = c("ICD-10", "icd11", "icd12")),
    "`to` must be a string"
  )
})

test_that("MAP fails for missing mapping table", {
  expect_error(
    MAP("foo", from = "idontexist", to = "ICD-10"),
    "not found in mapping metadata"
  )
  expect_error(
    MAP("foo", from = "Read 3", to = "idontexist"),
    "not found in mapping metadata"
  )
  expect_error(
    MAP("foo", from = "Read 3", to = "ICD-10", map_version = "nope"),
    "No mapping metadata found"
  )
})

test_that("`MAP()` warns about missing codes in the coding system being mapped from", {
  expect_warning(
    MAP(
      c("foo", "bar"),
      from = "Read 3",
      to = "ICD-10"
    ),
    "The following codes were not found in the mapping table",
  )
})

test_that("MAP() swaps `to` and `from` if necessary and warns", {
  # We only have Read 3 -> ICD-10 mapping in the database,
  # ICD-10 -> Read 3 is still expected to work, because MAP() swaps the direction if necessary
  test_codes <- c("E129", "E109", "E14", "L721", "I13") # ICD-10 codes
  test_from <- "ICD-10"
  test_to <- "Read 3"

  expect_warning(
    result <- MAP(test_codes, from = test_from, to = test_to),
    "No explicit mapping table found"
  )
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP('all') returns the mapping table", {
  result <- MAP(
    "all",
    from = "Read 3",
    to = "ICD-10",
    map_version = "UKB v4"
  ) |>
    nrow()

  expected <- get_mapping_table("Read 3", "ICD-10", map_version = "UKB v4") |>
    dplyr::collect() |>
    nrow()

  expect_identical(result, expected)
})

test_that("MAP handles versions correctly", {
  test_from <- "Read 3"
  test_to <- "ICD-10"
  test_version <- "UKB v5"

  # add test tables as new version of ICD-10
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
  # Clear cached "latest" so it re-resolves to the newly added versions
  codeminer_clear_versions(
    lookup = test_to,
    mapping = paste(test_from, ">", test_to)
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
