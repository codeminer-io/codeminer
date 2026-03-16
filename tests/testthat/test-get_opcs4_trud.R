test_that("get_opcs4_trud() validates release argument", {
  expect_error(
    get_opcs4_trud(release = c("latest", "other")),
    "must be a non-empty character string"
  )
  expect_error(
    get_opcs4_trud(release = ""),
    "must be a non-empty character string"
  )
  expect_error(
    get_opcs4_trud(release = 123),
    "must be a non-empty character string"
  )
})

test_that("get_opcs4_trud() validates dir_path exists", {
  expect_error(
    get_opcs4_trud(dir_path = "/nonexistent/path"),
    "Directory does not exist"
  )
})

test_that("get_opcs4_trud() skips download if zip exists", {
  temp_dir <- withr::local_tempdir()
  dummy_zip <- file.path(temp_dir, "OPCS-4.11 Data files txt.zip")
  file.create(dummy_zip)

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("OPCS-4.11 Data files txt.zip")
    },
    .package = "trud"
  )

  result <- get_opcs4_trud(dir_path = temp_dir, overwrite = FALSE, quiet = TRUE)

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_identical(result, dummy_zip)
})

test_that("get_opcs4_trud() downloads when file absent", {
  temp_dir <- withr::local_tempdir()

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("OPCS-4.11 Data files txt.zip")
    },
    download_item = function(item, directory, release, overwrite) {
      zip <- file.path(directory, "OPCS-4.11 Data files txt.zip")
      file.create(zip)
      invisible(zip)
    },
    .package = "trud"
  )

  result <- get_opcs4_trud(dir_path = temp_dir, quiet = TRUE)

  expect_type(result, "character")
  expect_true(file.exists(result))
})

test_that("get_opcs4_trud() re-downloads when overwrite = TRUE", {
  temp_dir <- withr::local_tempdir()
  dummy_zip <- file.path(temp_dir, "OPCS-4.11 Data files txt.zip")
  file.create(dummy_zip)

  download_called <- FALSE

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("OPCS-4.11 Data files txt.zip")
    },
    download_item = function(item, directory, release, overwrite) {
      download_called <<- TRUE
      invisible(file.path(directory, "OPCS-4.11 Data files txt.zip"))
    },
    .package = "trud"
  )

  get_opcs4_trud(dir_path = temp_dir, overwrite = TRUE, quiet = TRUE)

  expect_true(download_called)
})

test_that("get_opcs4_trud() errors when specific release not found", {
  temp_dir <- withr::local_tempdir()

  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      mock_trud_metadata("OPCS-4.11 Data files txt.zip")
    },
    .package = "trud"
  )

  expect_error(
    get_opcs4_trud(dir_path = temp_dir, release = "nonexistent", quiet = TRUE),
    "not found"
  )
})
