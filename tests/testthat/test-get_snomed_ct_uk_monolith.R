test_that("get_snomed_ct_uk_monolith() validates release argument", {
  expect_error(
    get_snomed_ct_uk_monolith(release = c("latest", "other")),
    "must be a non-empty character string"
  )

  expect_error(
    get_snomed_ct_uk_monolith(release = ""),
    "must be a non-empty character string"
  )

  expect_error(
    get_snomed_ct_uk_monolith(release = 123),
    "must be a non-empty character string"
  )
})

test_that("get_snomed_ct_uk_monolith() validates dir_path exists", {
  expect_error(
    get_snomed_ct_uk_monolith(dir_path = "/nonexistent/path"),
    "Directory does not exist"
  )
})

test_that("get_snomed_ct_uk_monolith() skips download if zip exists", {
  temp_dir <- withr::local_tempdir()

  # Create a dummy zip file
  dummy_zip <- file.path(temp_dir, "SnomedCT_GPS_PRODUCTION.zip")
  file.create(dummy_zip)

  # Mock trud::get_item_metadata to return metadata matching our dummy
  local_mocked_bindings(
    get_item_metadata = function(item, release_scope) {
      list(
        releases = list(
          list(
            id = "dummy_release",
            archiveFileName = "SnomedCT_GPS_PRODUCTION.zip"
          )
        )
      )
    },
    .package = "trud"
  )

  # Should return existing zip path without calling download
  result <- get_snomed_ct_uk_monolith(
    dir_path = temp_dir,
    overwrite = FALSE,
    quiet = TRUE
  )

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_identical(result, dummy_zip)
})
