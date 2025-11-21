create_dummy_database()

withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("read_snomed_ct_uk_monolith() returns appropriate tables", {
  # Set the index for the TRUD metadata release
  # Use 1 for the latest version; increasing values retrieve older releases

  #TODO
  # build_snomed_metadata <- build_snomed_metadata(
  #   # path_destination = path_destination,
  #   path_destination = getwd(),
  #   release_index = 1,
  #   item_id = 1799
  # )
  # expected_path <- build_snomed_metadata$expected_path
  # # Skip test for CI as path is for local test
  # skip_if(
  #   !dir.exists(expected_path),
  #   paste("Test data is for local test:", expected_path)
  # )

  snomedct <- read_snomed_ct_uk_monolith(dummy_snomed_data_path())

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

  expect_equal(
    names(snomedct$sct_icd10_mapping),
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

# # Skip the whole file/tests if TRUD_API_KEY is not set
# # Meaning that unit tests are intended for local execution.

# if (Sys.getenv("TRUD_API_KEY") == "") {
#   skip("Skipping NHS TRUD tests because TRUD_API_KEY is not set")
# }

# test_that("download_snomed_item() correctly downloads the latest version of SNOMED item 1799", {
#   # Set the index for the TRUD metadata release
#   # Use 1 for the latest version; increasing values retrieve older releases
#   release_index <- 1

#   # Download and extract the specified SNOMED CT UK TRUD release
#   results <- download_snomed_item(
#     path_destination = getwd(),
#     item_number = 1799,
#     release_index = release_index
#   )

#   trud_metadata <- trud::get_item_metadata(1799, release_scope = "all")
#   latest_release_id <- trud_metadata$releases[[
#     release_index
#   ]]$id

#   # Compare the downloaded release ID to the latest one in metadata
#   expect_equal(
#     results$release_id,
#     latest_release_id,
#     label = "Downloaded release ID",
#     expected.label = "Latest TRUD release ID"
#   )
# })

# test_that("add_snomed_tables() are added successfully", {
#   expect_invisible(
#     success <- add_snomed_tables(
#       path = getwd(),
#       release_index = 1
#     )
#   )
#   expect_true(success)

#   result <- CODES("all", code_type = "SNOMED-CT")
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) > 0)
# })
