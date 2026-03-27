test_that("read_read2_trud() errors on nonexistent path", {
  expect_error(read_read2_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_read2_trud() validates tables argument", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_read2_trud(tmp, tables = "bad_table"),
    class = "rlang_error"
  )
})

test_that("read_read2_trud() errors when Mapping Tables directory missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_read2_trud(tmp)),
    "Mapping Tables"
  )
})

test_that("read_read2_trud() errors when lookup file missing", {
  tmp <- withr::local_tempdir()
  # Create the directory structure but omit the lookup file
  assured_dir <- file.path(
    tmp,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )
  not_assured_dir <- file.path(
    tmp,
    "Mapping Tables",
    "Updated",
    "Not Clinically Assured"
  )
  dir.create(assured_dir, recursive = TRUE)
  dir.create(not_assured_dir, recursive = TRUE)

  expect_error(
    suppressMessages(read_read2_trud(tmp, tables = "read2_lkp")),
    "rctermsctmap"
  )
})

test_that("read_read2_trud() returns all tables by default", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir))

  expect_true(all(
    c("read2_lkp", "read2_relationship", "read2_ctv3", "ctv3_read2") %in%
      names(result)
  ))
})

test_that("read_read2_trud() read2_relationship has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(
    dir,
    tables = "read2_relationship"
  ))

  expect_true("read2_relationship" %in% names(result))
  rel <- result$read2_relationship$relationship
  expect_s3_class(rel$table, "data.frame")
  expect_equal(rel$metadata$code_type, "read2")
  expect_equal(rel$metadata$child_parent_relationship_code, "is a")
})

test_that("read_read2_trud() read2_lkp has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))

  expect_true("lookup" %in% names(result$read2_lkp))
  expect_s3_class(result$read2_lkp$lookup$table, "data.frame")
  expect_equal(result$read2_lkp$lookup$metadata$code_type, "read2")
  expect_equal(result$read2_lkp$lookup$metadata$lookup_code_col, "ReadCode")
  expect_equal(result$read2_lkp$lookup$metadata$lookup_description_col, "Term")
})

test_that("read_read2_trud() read2_lkp returns all rows including multi-description codes", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))
  tbl <- result$read2_lkp$lookup$table

  # 1234 appears twice (two descriptions), 5678 appears once
  expect_equal(nrow(tbl), 3L)
  expect_equal(sum(tbl$ReadCode == "1234"), 2L)
})

test_that("read_read2_trud() read2_ctv3 has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_ctv3"))

  expect_true("mapping" %in% names(result$read2_ctv3))
  expect_s3_class(result$read2_ctv3$mapping$table, "data.frame")
  expect_equal(result$read2_ctv3$mapping$metadata$from_code_type, "read2")
  expect_equal(result$read2_ctv3$mapping$metadata$to_code_type, "read3")
})

test_that("read_read2_trud() ctv3_read2 has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "ctv3_read2"))

  expect_true("mapping" %in% names(result$ctv3_read2))
  expect_equal(result$ctv3_read2$mapping$metadata$from_code_type, "read3")
  expect_equal(result$ctv3_read2$mapping$metadata$to_code_type, "read2")
})

test_that("read_read2_trud() tables argument selects subset", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(
    dir,
    tables = c("read2_lkp", "read2_ctv3")
  ))

  expect_named(result, c("read2_lkp", "read2_ctv3"))
  expect_false("ctv3_read2" %in% names(result))
})

test_that("read_read2_trud() uses custom version", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, version = "NDM_v2020"))

  expect_equal(result$read2_lkp$lookup$metadata$lookup_version, "NDM_v2020")
  expect_equal(result$read2_ctv3$mapping$metadata$map_version, "NDM_v2020")
  expect_equal(result$ctv3_read2$mapping$metadata$map_version, "NDM_v2020")
})

test_that("read_read2_trud() errors when read2_ctv3 file missing", {
  tmp <- withr::local_tempdir()
  assured_dir <- file.path(
    tmp,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )
  not_assured_dir <- file.path(
    tmp,
    "Mapping Tables",
    "Updated",
    "Not Clinically Assured"
  )
  dir.create(assured_dir, recursive = TRUE)
  dir.create(not_assured_dir, recursive = TRUE)

  # Only create the lookup file, not the mapping files
  writeLines(
    c(
      paste(c("ReadCode", "Term", "ConceptId", "MapId"), collapse = "\t"),
      paste(c("1234", "Test", "12345", "1"), collapse = "\t")
    ),
    file.path(not_assured_dir, "rctermsctmap_uk_20200401000001.txt")
  )

  expect_error(
    suppressMessages(read_read2_trud(tmp, tables = "read2_ctv3")),
    "rctctv3map"
  )
})

test_that("read_read2_trud() accepts zip file input", {
  tmp <- withr::local_tempdir()
  release_name <- "Read2_Dummy_Release"
  release_dir <- file.path(tmp, release_name)

  assured_dir <- file.path(
    release_dir,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )
  not_assured_dir <- file.path(
    release_dir,
    "Mapping Tables",
    "Updated",
    "Not Clinically Assured"
  )
  dir.create(assured_dir, recursive = TRUE)
  dir.create(not_assured_dir, recursive = TRUE)

  writeLines(
    c(
      paste(c("ReadCode", "Term", "ConceptId", "MapId"), collapse = "\t"),
      paste(c("1234", "Diabetes", "12345678", "1"), collapse = "\t")
    ),
    file.path(not_assured_dir, "rctermsctmap_uk_20200401000001.txt")
  )
  writeLines(
    c(
      paste(c("V2_CONCEPTID", "CTV3_CONCEPTID"), collapse = "\t"),
      paste(c("1234", "X40J5"), collapse = "\t")
    ),
    file.path(assured_dir, "rctctv3map_uk_20200401000001.txt")
  )
  writeLines(
    c(
      paste(c("CTV3_CONCEPTID", "V2_CONCEPTID"), collapse = "\t"),
      paste(c("X40J5", "1234"), collapse = "\t")
    ),
    file.path(assured_dir, "ctv3rctmap_uk_20200401000002.txt")
  )

  zip_path <- file.path(tmp, paste0(release_name, ".zip"))
  withr::with_dir(
    tmp,
    utils::zip(zipfile = paste0(release_name, ".zip"), files = release_name)
  )

  result <- suppressMessages(read_read2_trud(zip_path))

  expect_named(
    result,
    c("read2_lkp", "read2_relationship", "read2_ctv3", "ctv3_read2")
  )
  expect_gt(nrow(result$read2_lkp$lookup$table), 0)
})
