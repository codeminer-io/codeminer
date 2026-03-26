test_that("read_icd10_trud() errors on nonexistent path", {
  expect_error(read_icd10_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_icd10_trud() errors when codes file not found", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_icd10_trud(tmp)),
    "Could not find ICD-10 codes file"
  )
})

test_that("read_icd10_trud() returns correct structure", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))

  expect_named(result, "icd10_lkp")
  expect_true("lookup" %in% names(result$icd10_lkp))
  expect_true("table" %in% names(result$icd10_lkp$lookup))
  expect_true("metadata" %in% names(result$icd10_lkp$lookup))
  expect_s3_class(result$icd10_lkp$lookup$table, "data.frame")
  expect_equal(result$icd10_lkp$lookup$metadata$code_type, "ICD-10")
})

test_that("read_icd10_trud() reads all rows", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))
  expect_equal(nrow(result$icd10_lkp$lookup$table), 3L)
})

test_that("read_icd10_trud() augments DESCRIPTION with MODIFIER_4", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))
  tbl <- result$icd10_lkp$lookup$table

  e11 <- tbl[tbl$CODE == "E11", ]
  expect_match(e11$DESCRIPTION, "With renal complications")
})

test_that("read_icd10_trud() augments DESCRIPTION with MODIFIER_5", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))
  tbl <- result$icd10_lkp$lookup$table

  e12 <- tbl[tbl$CODE == "E12", ]
  expect_match(e12$DESCRIPTION, "With coma")
})

test_that("read_icd10_trud() does not modify DESCRIPTION when no modifier", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))
  tbl <- result$icd10_lkp$lookup$table

  e10 <- tbl[tbl$CODE == "E10", ]
  expect_equal(e10$DESCRIPTION, "Type 1 diabetes mellitus")
})

test_that("read_icd10_trud() uses custom version", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir, version = "MY_VERSION"))
  expect_equal(result$icd10_lkp$lookup$metadata$lookup_version, "MY_VERSION")
})

test_that("read_icd10_trud() derives version from directory name", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(dir))
  expect_equal(result$icd10_lkp$lookup$metadata$lookup_version, basename(dir))
})

test_that("read_icd10_trud() uses custom source", {
  dir <- create_dummy_icd10_dir()
  result <- suppressMessages(read_icd10_trud(
    dir,
    source = "https://custom.source/"
  ))
  expect_equal(
    result$icd10_lkp$lookup$metadata$lookup_source,
    "https://custom.source/"
  )
})

test_that("read_icd10_trud() accepts zip file input", {
  tmp <- withr::local_tempdir()
  release_name <- "ICD10_Dummy_Release"
  release_dir <- file.path(tmp, release_name)
  dir.create(file.path(release_dir, "Content"), recursive = TRUE)

  writeLines(
    c(
      paste(
        c(
          "CODE",
          "ALT_CODE",
          "USAGE",
          "USAGE_UK",
          "DESCRIPTION",
          "MODIFIER_4",
          "MODIFIER_5",
          "QUALIFIERS",
          "GENDER_MASK",
          "MIN_AGE",
          "MAX_AGE",
          "TREE_DESCRIPTION"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "E10",
          "E10",
          "M",
          "N",
          "Type 1 diabetes",
          "",
          "",
          "",
          "0",
          "0",
          "999",
          "Endo"
        ),
        collapse = "\t"
      )
    ),
    file.path(
      release_dir,
      "Content",
      "ICD10_Edition5_CodesAndTitlesAndMetadata_GB.txt"
    )
  )

  zip_path <- file.path(tmp, paste0(release_name, ".zip"))
  withr::with_dir(
    tmp,
    utils::zip(zipfile = paste0(release_name, ".zip"), files = release_name)
  )

  result <- suppressMessages(read_icd10_trud(zip_path))

  expect_named(result, "icd10_lkp")
  expect_gt(nrow(result$icd10_lkp$lookup$table), 0)
  # version derived from zip filename
  expect_equal(
    result$icd10_lkp$lookup$metadata$lookup_version,
    paste0(release_name, ".zip")
  )
})
