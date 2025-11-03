test_that("`CODES()` returns the expected number of results", {
  expect_equal(
    nrow(
      CODES(
        codes = c("C10E.", "C108."),
        code_type = "read2",
        all_lkps_maps = all_lkps_maps,
        preferred_description_only = FALSE
      )
    ),
    expected = 7
  )

  expect_equal(
    nrow(
      CODES(
        codes = c("C10E.", "C108."),
        code_type = "read2",
        all_lkps_maps = all_lkps_maps,
        preferred_description_only = TRUE
      )
    ),
    expected = 2
  )
})

test_that("`CODES()` returns the expected columns when `standardise_output` is `TRUE`", {
  result <- CODES(
    codes = c("E10", "E100"),
    code_type = "icd10",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE
  )

  expect_equal(names(result), c("code", "description", "code_type"))

  expect_equal(
    result$description,
    c(
      "Type 1 diabetes mellitus",
      "Type 1 diabetes mellitus With coma"
    )
  )
})

test_that("`CODES()` returns unrecognised codes only when requested", {
  result <- CODES(
    codes = c("E10", "E100", "UNRECOGNISED"),
    code_type = "icd10",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    .return_unrecognised_codes = TRUE
  )

  expect_equal(
    result,
    "UNRECOGNISED"
  )
})
