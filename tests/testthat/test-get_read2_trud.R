test_that("get_read2_trud() delegates to get_nhs_data_migration()", {
  called_with <- list()

  local_mocked_bindings(
    get_nhs_data_migration = function(dir_path, release, overwrite, quiet) {
      called_with <<- list(
        dir_path = dir_path,
        release = release,
        overwrite = overwrite,
        quiet = quiet
      )
      invisible("dummy_path.zip")
    },
    .package = "codeminer"
  )

  result <- get_read2_trud(
    dir_path = tempdir(),
    release = "latest",
    overwrite = FALSE,
    quiet = TRUE
  )

  expect_identical(result, "dummy_path.zip")
  expect_equal(called_with$dir_path, tempdir())
  expect_equal(called_with$release, "latest")
  expect_false(called_with$overwrite)
  expect_true(called_with$quiet)
})

test_that("get_read2_trud() passes overwrite = TRUE to get_nhs_data_migration()", {
  called_overwrite <- NULL

  local_mocked_bindings(
    get_nhs_data_migration = function(dir_path, release, overwrite, quiet) {
      called_overwrite <<- overwrite
      invisible("dummy.zip")
    },
    .package = "codeminer"
  )

  get_read2_trud(overwrite = TRUE, quiet = TRUE)
  expect_true(called_overwrite)
})
