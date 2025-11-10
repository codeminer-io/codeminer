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

  snomed_monolith_terminology <- snomedct$snomed_monolith_terminology
  snomed_monolith_refset_Map <- snomedct$snomed_monolith_refset$Map

  expected_terminology_names <- c(
    "sct2_Concept_MONOSnapshot_GB_20251022.txt",
    "sct2_Description_MONOSnapshot-en_GB_20251022.txt",
    "sct2_Identifier_MONOSnapshot_GB_20251022.txt",
    "sct2_Relationship_MONOSnapshot_GB_20251022.txt",
    "sct2_RelationshipConcreteValues_MONOSnapshot_GB_20251022.txt",
    "sct2_sRefset_OWLExpressionMONOSnapshot_GB_20251022.txt",
    "sct2_StatedRelationship_MONOSnapshot_GB_20251022.txt",
    "sct2_TextDefinition_MONOSnapshot-en_GB_20251022.txt"
  )

  expected_refset_map_names <- c(
    "der2_iisssccRefset_ExtendedMapMONOSnapshot_GB_20251022.txt",
    "der2_iisssciRefset_ExtendedMapMONOSnapshot_GB_20251022.txt",
    "der2_iissscRefset_ComplexMapMONOSnapshot_GB_20251022.txt",
    "der2_sRefset_SimpleMapMONOSnapshot_GB_20251022.txt"
  )

  # Check that all expected names are present (order does not matter)
  expect_setequal(
    names(snomed_monolith_terminology),
    expected_terminology_names
  )
  expect_setequal(
    names(snomed_monolith_refset_Map),
    expected_refset_map_names
  )
})
