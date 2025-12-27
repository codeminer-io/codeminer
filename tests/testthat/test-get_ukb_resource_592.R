test_that("get_ukb_resource_592 logic works", {
  # 1. Test Input Validation
  expect_error(
    get_ukb_resource_592(dir_path = "invalid_dir"),
    "Directory does not exist"
  )

  # 2. Test Early Exit (overwrite = FALSE)
  # We mock file.exists to simulate the file already being there
  withr::with_tempdir({
    temp_dir <- getwd()
    target_file <- file.path(temp_dir, "all_lkps_maps_v4.xlsx")
    writeLines("dummy", target_file) # Create a fake file

    # If it tries to download, it will hit this error
    local_mocked_bindings(
      download.file = function(...) stop("Should not download"),
      .package = "utils"
    )

    expect_message(
      res <- get_ukb_resource_592(dir_path = temp_dir, overwrite = FALSE),
      "already exists"
    )
    expect_equal(res, target_file)
  })
})

# -------------------------------------------------------------------------
# Integration test (long-running)
# -------------------------------------------------------------------------

test_that("actually downloads UKB resource 592 (integration)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_long_tests()

  temp_dir <- withr::local_tempdir()

  result <- get_ukb_resource_592(dir_path = temp_dir)

  expect_true(file.exists(result))
  expect_match(result, "all_lkps_maps_v4\\.xlsx$")
  expect_equal(dirname(result), temp_dir)
  expect_gt(file.size(result), 1000)
})
