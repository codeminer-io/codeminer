withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("get_snomed_available_items() fetch item number 1799 and its name", {
  available_items <- get_snomed_available_items()

  # Test number of rows and columns
  expect_equal(nrow(available_items), 74)
  expect_equal(ncol(available_items), 2)

  # Test item number `1799`` and name `SNOMED UK Monolith Edition, RF2: Snapshot`
  expect_equal(available_items$item_number[71], 1799)
  expect_equal(
    available_items$item_name[71],
    "SNOMED CT UK Monolith Edition, RF2: Snapshot"
  )
})

test_that("download_locally_latestversion_of_snomed_item() download the latest version of item 1799", {
  # Skip if no TRUD_API_KEY is available
  skip_if(Sys.getenv("TRUD_API_KEY") == "", "No TRUD API key available")

  results <- download_locally_latestversion_of_snomed_item(1799)
  expect_equal(results$release_id, "uk_sct2mo_41.1.0_20251022000001Z.zip")
})
