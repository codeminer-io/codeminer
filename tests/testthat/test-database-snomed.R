withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

# Skip the whole file/test if TRUD_API_KEY is not set
if (Sys.getenv("TRUD_API_KEY") == "") {
  skip("Skipping NHS TRUD tests because TRUD_API_KEY is not set")
}

test_that("download_latestversion_of_snomed_item() correctly downloads the latest version of SNOMED item 1799", {
  # Unit tests are intended for local execution.
  # See the function's documentation for prerequisites.

  # Set the index for the TRUD metadata release
  # Use 1 for the latest version; increasing values retrieve older releases
  release_index <- 1

  # Download and extract the specified SNOMED CT UK TRUD release
  results <- download_latestversion_of_snomed_item(
    1799,
    release_index
  )

  trud_metadata <- trud::get_item_metadata(1799, release_scope = "all")
  latest_release_id <- trud_metadata$releases[[
    release_index
  ]]$id

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

  # Set the index for the TRUD metadata release
  # Use 1 for the latest version; increasing values retrieve older releases
  release_index <- 1

  trud_metadata <- trud::get_item_metadata(1799, release_scope = "all")
  expected_zip_name <- trud_metadata$releases[[
    release_index
  ]]$archiveFileName
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
    names(snomedct$sct_lookup),
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
})

test_that("add_snomed_database() maps snomed database with codeminer ", {
  # Unit tests are intended for local execution.
  # See the function's documentation for prerequisites.

  databases <- add_snomed_database(
    path_destination = getwd(),
    release_index = 1
  )

  #TODO databases$lookup_metadata$lookup_table_name
  expect_equal(
    databases$mapping_metadata$mapping_table_name,
    "ICD-10_SNOMED-CT_Release_41.1.0"
  )
})
