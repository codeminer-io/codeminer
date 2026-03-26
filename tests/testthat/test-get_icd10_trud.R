test_that("get_icd10_trud() validates release argument", {
  expect_error(
    get_icd10_trud(release = c("latest", "other")),
    "must be a non-empty character string"
  )
  expect_error(
    get_icd10_trud(release = ""),
    "must be a non-empty character string"
  )
  expect_error(
    get_icd10_trud(release = 123),
    "must be a non-empty character string"
  )
})

test_that("get_icd10_trud() validates dir_path exists", {
  expect_error(
    get_icd10_trud(dir_path = "/nonexistent/path"),
    "Directory does not exist"
  )
})

test_that("get_icd10_trud() skips download if zip exists", {
  temp_dir <- withr::local_tempdir()
  dummy_zip <- file.path(temp_dir, "ICD10_Edition5_20160401_2.zip")
  file.create(dummy_zip)

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("ICD10_Edition5_20160401_2.zip")
    },
    .package = "trud"
  )

  result <- get_icd10_trud(dir_path = temp_dir, overwrite = FALSE, quiet = TRUE)

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_identical(result, dummy_zip)
})

test_that("get_icd10_trud() downloads when file absent", {
  temp_dir <- withr::local_tempdir()

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("ICD10_Edition5_20160401_2.zip")
    },
    download_item = function(item, directory, release, overwrite) {
      zip <- file.path(directory, "ICD10_Edition5_20160401_2.zip")
      file.create(zip)
      invisible(zip)
    },
    .package = "trud"
  )

  result <- get_icd10_trud(dir_path = temp_dir, quiet = TRUE)

  expect_type(result, "character")
  expect_true(file.exists(result))
})

test_that("get_icd10_trud() re-downloads when overwrite = TRUE", {
  temp_dir <- withr::local_tempdir()
  dummy_zip <- file.path(temp_dir, "ICD10_Edition5_20160401_2.zip")
  file.create(dummy_zip)

  download_called <- FALSE

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("ICD10_Edition5_20160401_2.zip")
    },
    download_item = function(item, directory, release, overwrite) {
      download_called <<- TRUE
      invisible(file.path(directory, "ICD10_Edition5_20160401_2.zip"))
    },
    .package = "trud"
  )

  get_icd10_trud(dir_path = temp_dir, overwrite = TRUE, quiet = TRUE)

  expect_true(download_called)
})

test_that("get_icd10_trud() errors when specific release not found", {
  temp_dir <- withr::local_tempdir()

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("ICD10_Edition5_20160401_2.zip")
    },
    .package = "trud"
  )

  expect_error(
    get_icd10_trud(dir_path = temp_dir, release = "nonexistent", quiet = TRUE),
    "not found"
  )
})

test_that("get_icd10_trud() passes specific release to download_item", {
  temp_dir <- withr::local_tempdir()
  called_with_release <- NULL

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("ICD10_Edition5_20160401_2.zip")
    },
    download_item = function(item, directory, release, overwrite) {
      called_with_release <<- release
      zip <- file.path(directory, "ICD10_Edition5_20160401_2.zip")
      file.create(zip)
      invisible(zip)
    },
    .package = "trud"
  )

  get_icd10_trud(dir_path = temp_dir, release = "release_1", quiet = TRUE)

  expect_equal(called_with_release, "release_1")
})
