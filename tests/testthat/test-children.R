# TODO: as part of https://github.com/codeminer-io/codeminer/issues/44
testthat::skip("CHILDREN not implemented yet")
test_that("`CHILDREN()` returns error for unrecognised codes", {
  expect_error(
    CHILDREN(
      codes = c("C10"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    regexp = "not found for 'read2' in table 'read_v2_lkp"
  )
})

testthat::skip("CHILDREN not implemented yet")
test_that("`CHILDREN()` works as expected for read2", {
  expect_equal(
    CHILDREN(
      codes = c("C10.."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    c("C10..", "C108.", "C10E.")
  )
})

testthat::skip("CHILDREN not implemented yet")
test_that("`CHILDREN()` raises error for unsupported code types e.g. read3", {
  expect_error(
    CHILDREN(
      codes = "C10..",
      code_type = "read3",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    "Currently codeminer is unable to retrieve child codes for read3"
  )
})
