withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("download_latestversion_of_snomed_item() downloads the latest version of item 1799", {
  # Skip if no TRUD_API_KEY is available
  skip_if(Sys.getenv("TRUD_API_KEY") == "", "No TRUD API key available")

  results <- download_latestversion_of_snomed_item(1799)
  expect_equal(results$release_id, "uk_sct2mo_41.1.0_20251022000001Z.zip")
})
