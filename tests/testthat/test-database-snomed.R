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

test_that("read_snomed_ct_uk_monolith() returns appropriate tables", {
  # Define path to the test data
  path <- file.path(
    getwd(),
    "snomed_item_1799_uk_sct2mo_41.1.0_20251022000001Z.zip",
    "SnomedCT_MonolithRF2_PRODUCTION_20251022T120000Z"
  )

  # Skip test for CI as path is for local test
  skip_if(!dir.exists(path), paste("Test data is for local test:", path))

  # Run the function
  snomedct <- read_snomed_ct_uk_monolith(path)

  expect_equal(
    names(snomedct$sct_description),
    c(
      "id_description",
      "effectiveTime_description",
      "active_description",
      "moduleId_description",
      "conceptId",
      "languageCode_description",
      "typeId_description",
      "term_description",
      "caseSignificanceId_description",
      "effectiveTime_concept",
      "active_concept",
      "moduleId_concept",
      "definitionStatusId_concept"
    )
  )

  expect_equal(
    names(snomedct$sct_relationship),
    c(
      "id",
      "effectiveTime",
      "active",
      "moduleId",
      "sourceId",
      "destinationId",
      "relationshipGroup",
      "typeId",
      "characteristicTypeId",
      "modifierId"
    )
  )

  expect_equal(
    names(snomedct$sct_icd10),
    c(
      "id",
      "effectiveTime",
      "active",
      "moduleId",
      "refsetId",
      "referencedComponentId",
      "mapGroup",
      "mapPriority",
      "mapRule",
      "mapAdvice",
      "mapTarget",
      "correlationId",
      "mapBlock"
    )
  )
})
