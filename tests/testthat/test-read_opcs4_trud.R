test_that("read_opcs4_trud() errors on nonexistent path", {
  expect_error(read_opcs4_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_opcs4_trud() errors when codes file not found", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_opcs4_trud(tmp)),
    "Could not find OPCS-4 codes file"
  )
})

test_that("read_opcs4_trud() returns correct structure", {
  dir <- create_dummy_opcs4_dir()
  result <- suppressMessages(read_opcs4_trud(dir))

  expect_named(result, "opcs4_lkp")
  expect_true("lookup" %in% names(result$opcs4_lkp))
  expect_true("table" %in% names(result$opcs4_lkp$lookup))
  expect_true("metadata" %in% names(result$opcs4_lkp$lookup))
  expect_s3_class(result$opcs4_lkp$lookup$table, "data.frame")
  expect_equal(result$opcs4_lkp$lookup$metadata$code_type, "OPCS4")
})

test_that("read_opcs4_trud() reads the correct columns", {
  dir <- create_dummy_opcs4_dir()
  result <- suppressMessages(read_opcs4_trud(dir))
  tbl <- result$opcs4_lkp$lookup$table

  expect_true("opcs4_code" %in% names(tbl))
  expect_true("description" %in% names(tbl))
  expect_equal(nrow(tbl), 3L)
  expect_equal(tbl$opcs4_code[[1]], "A01")
  expect_equal(tbl$description[[1]], "Excision of gallbladder")
})

test_that("read_opcs4_trud() uses custom version", {
  dir <- create_dummy_opcs4_dir()
  result <- suppressMessages(read_opcs4_trud(dir, version = "OPCS4_v11"))
  expect_equal(result$opcs4_lkp$lookup$metadata$lookup_version, "OPCS4_v11")
})

test_that("read_opcs4_trud() derives version from directory name", {
  dir <- create_dummy_opcs4_dir()
  result <- suppressMessages(read_opcs4_trud(dir))
  expect_equal(result$opcs4_lkp$lookup$metadata$lookup_version, basename(dir))
})

test_that("read_opcs4_trud() uses custom source", {
  dir <- create_dummy_opcs4_dir()
  result <- suppressMessages(read_opcs4_trud(
    dir,
    source = "https://custom.source/"
  ))
  expect_equal(
    result$opcs4_lkp$lookup$metadata$lookup_source,
    "https://custom.source/"
  )
})

test_that("read_opcs4_trud() accepts zip file input", {
  tmp <- withr::local_tempdir()
  release_name <- "OPCS4_Dummy_Release"
  release_dir <- file.path(tmp, release_name)
  dir.create(release_dir)

  writeLines(
    c("A01\tExcision of organ", "A02\tIncision of organ"),
    file.path(release_dir, "OPCS411 CodesAndTitles.txt")
  )

  zip_path <- file.path(tmp, paste0(release_name, ".zip"))
  withr::with_dir(
    tmp,
    utils::zip(zipfile = paste0(release_name, ".zip"), files = release_name)
  )

  result <- suppressMessages(read_opcs4_trud(zip_path))

  expect_named(result, "opcs4_lkp")
  expect_gt(nrow(result$opcs4_lkp$lookup$table), 0)
})
