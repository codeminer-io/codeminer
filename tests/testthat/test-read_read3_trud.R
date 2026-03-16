test_that("read_read3_trud() errors on nonexistent path", {
  expect_error(read_read3_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_read3_trud() validates tables argument", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "V3"))
  expect_error(
    suppressMessages(read_read3_trud(tmp, tables = "bad_table")),
    class = "rlang_error"
  )
})

test_that("read_read3_trud() errors when V3 directory missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_read3_trud(tmp)),
    "V3"
  )
})

test_that("read_read3_trud() returns both tables by default", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir))

  expect_named(result, c("read3_lkp", "read3_relationship"))
})

test_that("read_read3_trud() read3_lkp has correct structure", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))

  expect_true("lookup" %in% names(result$read3_lkp))
  expect_s3_class(result$read3_lkp$lookup$table, "data.frame")
  expect_equal(result$read3_lkp$lookup$metadata$code_type, "read3")
  expect_equal(result$read3_lkp$lookup$metadata$lookup_code_col, "code")
  expect_equal(result$read3_lkp$lookup$metadata$lookup_description_col, "term")
})

test_that("read_read3_trud() excludes retired codes from lookup", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # X40J7 is retired (status = "R") and should be excluded
  expect_false("X40J7" %in% tbl$code)
})

test_that("read_read3_trud() includes only preferred + clinical descriptions", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # X40J5 has preferred (P) + clinical (C) → "Disorder of organ"
  # Synonym (S) and obsolete (O) entries should not appear
  expect_equal(nrow(tbl[tbl$code == "X40J5", ]), 1L)
  expect_equal(tbl$term[tbl$code == "X40J5"], "Disorder of organ")
})

test_that("read_read3_trud() read3_relationship has correct structure", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_relationship"))

  expect_true("relationship" %in% names(result$read3_relationship))
  expect_s3_class(result$read3_relationship$relationship$table, "data.frame")
  expect_equal(result$read3_relationship$relationship$metadata$code_type, "read3")
  expect_equal(
    result$read3_relationship$relationship$metadata$child_parent_relationship_code,
    "01"
  )
})

test_that("read_read3_trud() relationship table has expected rows", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_relationship"))
  tbl <- result$read3_relationship$relationship$table

  expect_equal(nrow(tbl), 2L)
  expect_true("child_code" %in% names(tbl))
  expect_true("parent_code" %in% names(tbl))
})

test_that("read_read3_trud() tables argument selects subset", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  expect_named(result, "read3_lkp")
  expect_false("read3_relationship" %in% names(result))
})

test_that("read_read3_trud() uses custom version", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, version = "CTV3_v25"))
  expect_equal(result$read3_lkp$lookup$metadata$lookup_version, "CTV3_v25")
  expect_equal(result$read3_relationship$relationship$metadata$relationship_version, "CTV3_v25")
})

test_that("read_read3_trud() accepts zip file input", {
  tmp <- withr::local_tempdir()
  release_name <- "Read3_Dummy_Release"
  release_dir <- file.path(tmp, release_name)
  v3_dir <- file.path(release_dir, "V3")
  dir.create(v3_dir, recursive = TRUE)

  writeLines(c("X40J5|C|P|"), file.path(v3_dir, "Concept.v3"))
  writeLines(c("X40J5|D001|P"), file.path(v3_dir, "Descrip.v3"))
  writeLines(c("D001|C|Test disorder||"), file.path(v3_dir, "Terms.v3"))
  writeLines(c("X40J5|X40J6|01"), file.path(v3_dir, "V3hier.v3"))

  zip_path <- file.path(tmp, paste0(release_name, ".zip"))
  withr::with_dir(tmp, utils::zip(zipfile = paste0(release_name, ".zip"), files = release_name))

  result <- suppressMessages(read_read3_trud(zip_path))

  expect_named(result, c("read3_lkp", "read3_relationship"))
  expect_gt(nrow(result$read3_lkp$lookup$table), 0)
})
