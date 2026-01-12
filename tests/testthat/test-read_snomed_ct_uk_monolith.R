# Helper tests -----------------------------------------------------------

test_that("find_snomed_file selects correct file", {
  tmp <- withr::local_tempdir()
  fs::file_create(file.path(tmp, "sct2_Concept_Snapshot_2024.txt"))
  fs::file_create(file.path(tmp, "sct2_Other_File.txt"))

  result <- find_snomed_file(tmp, "^sct2_Concept_")
  expect_match(result, "sct2_Concept_Snapshot_2024.txt")
})

test_that("find_snomed_file errors on missing file", {
  tmp <- withr::local_tempdir()
  expect_error(find_snomed_file(tmp, "MissingFile"), "Could not find file")
})

# Main function tests -----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() returns all tables by default", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(dummy_snomed_ct_uk_monolith_path())
  )

  expect_equal(
    names(result),
    c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4")
  )
})

test_that("read_snomed_ct_uk_monolith() returns correct structure", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup"
    )
  )

  # Check nested structure
  expect_true("lookup" %in% names(result$sct_lookup))
  expect_true("table" %in% names(result$sct_lookup$lookup))
  expect_true("metadata" %in% names(result$sct_lookup$lookup))

  # Check table is data frame
  expect_s3_class(result$sct_lookup$lookup$table, "data.frame")

  # Check metadata has expected fields
  expect_equal(result$sct_lookup$lookup$metadata$code_type, "sct")
})

test_that("read_snomed_ct_uk_monolith() tables argument works", {
  # Request only lookup and ICD-10
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = c("sct_lookup", "sct_icd10")
    )
  )

  expect_equal(names(result), c("sct_lookup", "sct_icd10"))
  expect_false("sct_relationship" %in% names(result))
  expect_false("sct_opcs4" %in% names(result))
})

test_that("read_snomed_ct_uk_monolith() filters mappings correctly", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = c("sct_icd10", "sct_opcs4")
    )
  )

  # Check ICD-10 table has correct refset and no blocks
  expect_true(all(
    result$sct_icd10$mapping$table$refsetId == "999002271000000101"
  ))
  expect_false(any(grepl("#", result$sct_icd10$mapping$table$mapTarget)))

  # Check OPCS-4 table has correct refset and no blocks
  expect_true(all(
    result$sct_opcs4$mapping$table$refsetId == "999002321000000109"
  ))
  expect_false(any(grepl("#", result$sct_opcs4$mapping$table$mapTarget)))
})

# Error handling tests ----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() errors on non-existent path", {
  expect_error(
    read_snomed_ct_uk_monolith("/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("read_snomed_ct_uk_monolith() errors on missing subdirectories", {
  tmp <- withr::local_tempdir()

  # Create Snapshot dir but missing Terminology
  fs::dir_create(file.path(tmp, "Snapshot"))

  expect_error(
    suppressMessages(read_snomed_ct_uk_monolith(tmp)),
    "required subdirectories are missing"
  )
})

# Zip file input tests ----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() accepts zip file input", {
  # Get the package zip file directly
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(
    read_snomed_ct_uk_monolith(zip_path, tables = "sct_lookup")
  )

  expect_true("sct_lookup" %in% names(result))
  expect_s3_class(result$sct_lookup$lookup$table, "data.frame")
  expect_gt(nrow(result$sct_lookup$lookup$table), 0)
})

test_that("read_snomed_ct_uk_monolith() derives version from zip filename", {
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(
    read_snomed_ct_uk_monolith(zip_path, tables = "sct_lookup")
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_version,
    "snomed_gps.zip"
  )
})

test_that("read_snomed_ct_uk_monolith() extracts all tables from zip", {
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(read_snomed_ct_uk_monolith(zip_path))

  expect_equal(
    names(result),
    c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4")
  )

  # Check all tables have data

  expect_gt(nrow(result$sct_lookup$lookup$table), 0)
  expect_gt(nrow(result$sct_relationship$relationship$table), 0)
  expect_gt(nrow(result$sct_icd10$mapping$table), 0)
  expect_gt(nrow(result$sct_opcs4$mapping$table), 0)
})

# Parameter variation tests -----------------------------------------------

test_that("read_snomed_ct_uk_monolith() uses custom version parameter", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup",
      version = "CUSTOM_v1.0"
    )
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_version,
    "CUSTOM_v1.0"
  )
})

test_that("read_snomed_ct_uk_monolith() uses custom source parameter", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup",
      source = "https://custom.source.org/"
    )
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_source,
    "https://custom.source.org/"
  )
})

test_that("read_snomed_ct_uk_monolith() uses custom refset IDs", {
  # This won't find any matches with dummy data, but tests parameter passing
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_icd10",
      .icd10_refset_id = "999999999999999999"
    )
  )

  # Should return empty table since refset ID won't match
  expect_equal(nrow(result$sct_icd10$mapping$table), 0)
})
