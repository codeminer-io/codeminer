create_dummy_database()

test_that("get_metadata_for_relationship() works correctly", {
  con <- connect_to_db()

  # Test valid code_type and version
  meta <- get_metadata_for_relationship(con, "icd10", "v0")
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 1)
  expect_true(all(
    c("code_type", "relationship_version", "relationship_table_name") %in%
      names(meta)
  ))

  # Test latest version
  meta_latest <- get_metadata_for_relationship(con, "icd10", "latest")
  expect_s3_class(meta_latest, "data.frame")
  expect_equal(nrow(meta_latest), 1)
})
