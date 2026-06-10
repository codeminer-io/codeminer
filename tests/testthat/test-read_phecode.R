test_that("read_phecode() lookup metadata uses canonical code_type", {
  result <- read_phecode(lkp_path = dummy_phecode_lkp_path())
  expect_equal(result$phecode_lkp$lookup$metadata$code_type, "Phecode")
})

test_that("read_phecode() icd10_phecode mapping metadata has from_col / to_col matching real columns", {
  result <- read_phecode(icd10_map_path = dummy_icd10_phecode_map_path())
  meta <- result$icd10_phecode$mapping$metadata
  expect_equal(meta$from_code_type, "ICD-10")
  expect_equal(meta$to_code_type, "Phecode")
  expect_equal(meta$from_col, "ICD10")
  expect_equal(meta$to_col, "PHECODE")
  expect_true(all(
    c(meta$from_col, meta$to_col) %in%
      names(result$icd10_phecode$mapping$table)
  ))
})
