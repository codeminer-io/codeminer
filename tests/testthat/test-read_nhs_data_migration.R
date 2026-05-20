test_that("read_nhs_data_migration() errors on nonexistent path", {
  expect_error(
    read_nhs_data_migration("/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("read_nhs_data_migration() validates tables argument", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_nhs_data_migration(tmp, tables = "bad_table"),
    class = "rlang_error"
  )
})

test_that("read_nhs_data_migration() errors when Clinically Assured dir missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_nhs_data_migration(tmp)),
    "Clinically Assured"
  )
})

test_that("read_nhs_data_migration() returns all four tables by default", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(dir))

  expect_named(
    result,
    c("ctv3sctmap2", "rcsctmap2", "read2_ctv3", "ctv3_read2")
  )
})

test_that("read_nhs_data_migration() ctv3sctmap2 has correct metadata", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(
    dir,
    tables = "ctv3sctmap2"
  ))

  meta <- result$ctv3sctmap2$mapping$metadata
  expect_equal(meta$from_code_type, "Read v3")
  expect_equal(meta$to_code_type, "sct")
})

test_that("read_nhs_data_migration() rcsctmap2 has correct metadata", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(
    dir,
    tables = "rcsctmap2"
  ))

  meta <- result$rcsctmap2$mapping$metadata
  expect_equal(meta$from_code_type, "Read v2")
  expect_equal(meta$to_code_type, "sct")
})

test_that("read_nhs_data_migration() read2_ctv3 has correct metadata", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(
    dir,
    tables = "read2_ctv3"
  ))

  meta <- result$read2_ctv3$mapping$metadata
  expect_equal(meta$from_code_type, "Read v2")
  expect_equal(meta$to_code_type, "Read v3")
  expect_equal(meta$from_col, "V2_CONCEPTID")
  expect_equal(meta$to_col, "CTV3_CONCEPTID")
})

test_that("read_nhs_data_migration() ctv3_read2 has correct metadata", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(
    dir,
    tables = "ctv3_read2"
  ))

  meta <- result$ctv3_read2$mapping$metadata
  expect_equal(meta$from_code_type, "Read v3")
  expect_equal(meta$to_code_type, "Read v2")
  expect_equal(meta$from_col, "CTV3_CONCEPTID")
  expect_equal(meta$to_col, "V2_CONCEPTID")
})

test_that("read_nhs_data_migration() errors when a file is missing", {
  tmp <- withr::local_tempdir()
  assured_dir <- file.path(
    tmp,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )
  dir.create(assured_dir, recursive = TRUE)

  expect_error(
    suppressMessages(read_nhs_data_migration(tmp, tables = "read2_ctv3")),
    "rctctv3map"
  )
})

test_that("read_nhs_data_migration() uses custom version", {
  dir <- create_dummy_nhs_data_migration_dir()
  result <- suppressMessages(read_nhs_data_migration(
    dir,
    version = "NDM_v2020"
  ))

  expect_equal(result$ctv3sctmap2$mapping$metadata$map_version, "NDM_v2020")
  expect_equal(result$read2_ctv3$mapping$metadata$map_version, "NDM_v2020")
})
