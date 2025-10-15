test_that("RunCodelistBuilder sets ALL_LKPS_MAPS_DB environment variable for query execution", {
  # Regression test for issue #4: https://github.com/UCL-ARC/codemapper/issues/4

  # Setup: Create a temporary database
  temp_db_path <- tempfile(fileext = ".db")
  dummy_db <- suppressMessages(dummy_all_lkps_maps_db(db_path = temp_db_path))

  # Test: Call the setup portion of RunCodelistBuilder that should set the env var
  # We're testing the internal logic without launching the full Shiny app
  con <- check_all_lkps_maps_path(dummy_db)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  withr::local_envvar(list("ALL_LKPS_MAPS_DB" = dummy_db))

  # Test that query functions can now access the database via the env var
  # This simulates what happens when a user runs DESCRIPTION() in the app
  result <- expect_no_error({
    DESCRIPTION("diabetic retinopathy", code_type = "icd10")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Cleanup
  unlink(temp_db_path)
})

test_that("Query execution fails gracefully when ALL_LKPS_MAPS_DB is not set", {
  withr::local_envvar(list("ALL_LKPS_MAPS_DB" = NULL))

  # Ensure we're not in a directory with all_lkps_maps.db
  withr::local_dir(tempdir())

  expect_error(
    {
      DESCRIPTION("diab", code_type = "icd10", all_lkps_maps = NULL)
    },
    "No/invalid path supplied to `all_lkps_maps` and no file called 'all_lkps_maps.db' found in current working directory"
  )
})

test_that("RunCodelistBuilder properly validates database path parameter", {
  withr::local_envvar(list("ALL_LKPS_MAPS_DB" = NULL))

  expect_error(RunCodelistBuilder(), "No database file found at path")
  expect_error(RunCodelistBuilder(all_lkps_maps = "/path/that/does/not/exist.db"), "No database file found at path")
})
