withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("download_latestversion_of_snomed_item() correctly downloads the latest version of SNOMED item 1799", {
  # Unit tests are intended for local execution.
  # See the function's documentation for prerequisites.
  #
  # Skip if no TRUD_API_KEY is available
  skip_if(Sys.getenv("TRUD_API_KEY") == "", "No TRUD API key available")

  results <- download_latestversion_of_snomed_item(1799)

  trud_metadata <- trud::get_item_metadata(1799, release_scope = "all")
  latest_release_id <- trud_metadata$releases[[1]]$id

  # Compare the downloaded release ID to the latest one in metadata
  expect_equal(
    results$release_id,
    latest_release_id,
    label = "Downloaded release ID",
    expected.label = "Latest TRUD release ID"
  )
})

test_that("read_snomed_ct_uk_monolith() returns appropriate tables", {
  # Unit tests are intended for local execution.
  # See the function's documentation for prerequisites.

  trud_metadata <- trud::get_item_metadata(1799, release_scope = "all")
  expected_zip_name <- trud_metadata$releases[[1]]$archiveFileName
  expected_base_name <- tools::file_path_sans_ext(expected_zip_name)

  # Construct dynamic path with trud_metadata
  expected_path <- file.path(
    getwd(),
    paste0("snomed_item_1799_", expected_zip_name),
    paste0(
      "SnomedCT_MonolithRF2_PRODUCTION_",
      substr(
        expected_base_name,
        nchar(expected_base_name) - 14,
        nchar(expected_base_name) - 7
      ),
      "T120000Z"
    )
  )

  # Skip test for CI as path is for local test
  skip_if(
    !dir.exists(expected_path),
    paste("Test data is for local test:", expected_path)
  )

  # Run the function
  snomedct <- read_snomed_ct_uk_monolith(expected_path)

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
