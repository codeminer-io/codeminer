test_that("icd10cm_releases() is a well-formed, ascending release table", {
  releases <- icd10cm_releases()

  expect_named(releases, c("version", "file", "url"))
  expect_true(all(grepl("^[0-9]{8}$", releases$version)))
  expect_equal(anyDuplicated(releases$version), 0L)
  expect_equal(releases$version, sort(releases$version))
  expect_true(all(grepl("^https://ftp\\.cdc\\.gov/", releases$url)))
  expect_true(all(fs::path_ext(releases$file) %in% c("zip", "txt")))
  # Spaces in NCHS file names must be URL-encoded
  expect_false(any(grepl(" ", releases$url)))
})

test_that("get_icd10cm() validates release argument", {
  expect_error(
    get_icd10cm(release = c("latest", "other")),
    "must be a non-empty character string"
  )
  expect_error(
    get_icd10cm(release = ""),
    "must be a non-empty character string"
  )
  expect_error(
    get_icd10cm(release = 20261001),
    "must be a non-empty character string"
  )
})

test_that("get_icd10cm() validates dir_path exists", {
  expect_error(
    get_icd10cm(dir_path = "/nonexistent/path"),
    "Directory does not exist"
  )
})

test_that("get_icd10cm() errors on an unknown release, listing the known ones", {
  local_mocked_bindings(
    download.file = function(...) stop("Should not download"),
    .package = "utils"
  )
  dir <- withr::local_tempdir()

  expect_error(
    get_icd10cm(dir_path = dir, release = "19990101"),
    "Unknown ICD-10-CM release.*20261001"
  )
})

test_that("get_icd10cm() skips the download when the file already exists", {
  local_mocked_bindings(
    download.file = function(...) stop("Should not download"),
    .package = "utils"
  )
  dir <- withr::local_tempdir()
  existing <- file.path(dir, "icd10cm_order_20261001.zip")
  writeLines("dummy", existing)

  expect_message(
    path <- get_icd10cm(dir_path = dir),
    "already exists"
  )
  expect_equal(path, existing)
})

test_that("get_icd10cm() downloads the selected release to a dated file name", {
  downloaded_urls <- character()
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      downloaded_urls <<- c(downloaded_urls, url)
      writeLines("dummy", destfile)
      0L
    },
    .package = "utils"
  )
  dir <- withr::local_tempdir()

  # "latest" is the last row of the release table (a zip)
  latest <- get_icd10cm(dir_path = dir, quiet = TRUE)
  expect_equal(basename(latest), "icd10cm_order_20261001.zip")
  expect_true(file.exists(latest))

  # A release published as a loose .txt keeps that extension
  txt <- get_icd10cm(dir_path = dir, release = "20220401", quiet = TRUE)
  expect_equal(basename(txt), "icd10cm_order_20220401.txt")

  expect_equal(
    downloaded_urls,
    icd10cm_releases()$url[
      match(c("20261001", "20220401"), icd10cm_releases()$version)
    ]
  )
})

test_that("get_icd10cm() re-downloads when overwrite = TRUE", {
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fresh", destfile)
      0L
    },
    .package = "utils"
  )
  dir <- withr::local_tempdir()
  existing <- file.path(dir, "icd10cm_order_20261001.zip")
  writeLines("stale", existing)

  path <- get_icd10cm(dir_path = dir, overwrite = TRUE, quiet = TRUE)
  expect_equal(readLines(path), "fresh")
})

# -------------------------------------------------------------------------
# Integration test (long-running)
# -------------------------------------------------------------------------

test_that("actually downloads and reads the latest ICD-10-CM release (integration)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_long_tests()

  dir <- withr::local_tempdir()
  path <- get_icd10cm(dir_path = dir, quiet = TRUE)
  expect_true(file.exists(path))

  result <- suppressMessages(read_icd10cm(path))
  lkp <- result$icd10cm_lkp$lookup$table
  rel <- result$icd10cm_relationship$relationship$table

  expect_gt(nrow(lkp), 90000)
  expect_false(anyNA(lkp$category))
  # Every code below the 3-character category level has a parent
  non_category <- lkp$icd10cm_code[nchar(lkp$icd10cm_code) > 3]
  expect_true(all(non_category %in% rel$from))
})
