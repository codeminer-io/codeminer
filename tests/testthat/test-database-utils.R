local_build_temp_database()
con <- connect_to_db(read_only = FALSE)

test_that("table_exists() returns TRUE for existing table", {
  expect_true(table_exists(con, "_lookup_metadata"))
  expect_true(table_exists(con, "_mapping_metadata"))
  expect_true(table_exists(con, "_relationship_metadata"))
})

test_that("table_exists() returns FALSE for non-existing table", {
  expect_false(table_exists(con, "nonexistent_table"))
  expect_false(table_exists(con, "foo_bar_baz"))
})

test_that("check_database() succeeds with valid database", {
  expect_invisible(check_database(con))
  expect_true(check_database(con))
})

test_that("check_database() fails when metadata tables are missing", {
  local_temp_database()
  con_empty <- connect_to_db(read_only = FALSE)

  expect_error(
    check_database(con_empty),
    "The lookup metadata table does not exist in the database"
  )

  # Create only lookup metadata table
  create_lookup_metadata_table(con_empty)
  expect_error(
    check_database(con_empty),
    "The mapping metadata table does not exist in the database"
  )

  # Create only mapping metadata table
  create_mapping_metadata_table(con_empty)
  expect_error(
    check_database(con_empty),
    "The relationship metadata table does not exist in the database"
  )
})

test_that("add_metadata_table() can take multiple entries", {
  test_metadata <- lookup_metadata(c("foo", "bar", "baz"))

  expect_no_error(add_metadata_table(con, test_metadata, type = "lookup"))
  expect_equal(nrow(get_lookup_metadata(con)), 3)
})

test_that("add_metadata_table() fails for invalid type", {
  test_metadata <- lookup_metadata(c("foo", "bar", "baz"))

  expect_error(
    add_metadata_table(con, test_metadata, type = "invalid"),
    '`type` must be one of "lookup", "mapping", or "relationship"'
  )
})

test_that("add_metadata_table() works with mapping type", {
  test_metadata <- mapping_metadata("code_a", "code_b", map_version = "v1")

  result <- add_metadata_table(con, test_metadata, type = "mapping")
  expect_type(result, "integer")
  expect_true(result > 0)

  mapping_meta <- get_mapping_metadata(con)
  expect_true("code_a_code_b_v1" %in% mapping_meta$mapping_table_name)
})

test_that("add_metadata_table() works with relationship type", {
  test_metadata <- relationship_metadata("test", relationship_version = "v1")

  result <- add_metadata_table(con, test_metadata, type = "relationship")
  expect_type(result, "integer")
  expect_true(result > 0)

  rel_meta <- get_relationship_metadata(con)
  expect_true("test_relationship_v1" %in% rel_meta$relationship_table_name)
})

test_that("add_metadata_table() prevents duplicate entries", {
  test_metadata <- lookup_metadata("duplicate_test", lookup_version = "v1")

  # First addition should succeed
  result1 <- add_metadata_table(con, test_metadata, type = "lookup")
  expect_true(result1 > 0)

  # Second addition with same lookup_table_name should fail
  result2 <- add_metadata_table(con, test_metadata, type = "lookup")
  expect_false(result2)

  # Verify only one entry exists
  lookup_meta <- get_lookup_metadata(con)
  duplicate_count <- sum(lookup_meta$lookup_table_name == "duplicate_test_v1")
  expect_equal(duplicate_count, 1)
})

test_that("add_metadata_table() fails when missing required id column", {
  # Create metadata missing the lookup_table_name field
  bad_metadata <- data.frame(
    code_type = "test",
    lookup_version = "v1"
  )

  expect_error(
    add_metadata_table(con, bad_metadata, type = "lookup"),
    "Missing field lookup_table_name in metadata"
  )
})

test_that("add_metadata_table() handles single entry correctly", {
  test_metadata <- lookup_metadata("single_entry", lookup_version = "v0")

  initial_count <- nrow(get_lookup_metadata(con))
  result <- add_metadata_table(con, test_metadata, type = "lookup")

  expect_type(result, "integer")
  expect_equal(result, 1)
  expect_equal(nrow(get_lookup_metadata(con)), initial_count + 1)
})

test_that("get_lookup_metadata() returns a data frame", {
  result <- get_lookup_metadata(con)

  expect_s3_class(result, "data.frame")
  expect_true("lookup_table_name" %in% names(result))
})

test_that("get_lookup_metadata() returns all added entries", {
  # Add multiple entries
  test_metadata <- lookup_metadata(
    c("test1", "test2", "test3"),
    lookup_version = "v99"
  )
  add_metadata_table(con, test_metadata, type = "lookup")

  result <- get_lookup_metadata(con)
  expect_true(all(
    c("test1_v99", "test2_v99", "test3_v99") %in% result$lookup_table_name
  ))
})

test_that("get_lookup_metadata() works without explicit connection", {
  # Should use default connection from connect_to_db()
  expect_no_error(result <- get_lookup_metadata())
  expect_s3_class(result, "data.frame")
})

test_that("get_mapping_metadata() returns a data frame", {
  result <- get_mapping_metadata(con)

  expect_s3_class(result, "data.frame")
  expect_true("mapping_table_name" %in% names(result))
})

test_that("get_mapping_metadata() returns all added entries", {
  # Add multiple mapping entries
  test_metadata1 <- mapping_metadata("type1", "type2", map_version = "v1")
  test_metadata2 <- mapping_metadata("type3", "type4", map_version = "v2")

  add_metadata_table(con, test_metadata1, type = "mapping")
  add_metadata_table(con, test_metadata2, type = "mapping")

  result <- get_mapping_metadata(con)
  expect_true("type1_type2_v1" %in% result$mapping_table_name)
  expect_true("type3_type4_v2" %in% result$mapping_table_name)
})

test_that("get_mapping_metadata() works without explicit connection", {
  # Should use default connection from connect_to_db()
  expect_no_error(result <- get_mapping_metadata())
  expect_s3_class(result, "data.frame")
})

test_that("get_relationship_metadata() returns a data frame", {
  result <- get_relationship_metadata(con)

  expect_s3_class(result, "data.frame")
  expect_true("relationship_table_name" %in% names(result))
})

test_that("get_relationship_metadata() returns all added entries", {
  # Add multiple relationship entries
  test_metadata1 <- relationship_metadata(
    "rel_type1",
    relationship_version = "v1"
  )
  test_metadata2 <- relationship_metadata(
    "rel_type2",
    relationship_version = "v2"
  )

  add_metadata_table(con, test_metadata1, type = "relationship")
  add_metadata_table(con, test_metadata2, type = "relationship")

  result <- get_relationship_metadata(con)
  expect_true("rel_type1_relationship_v1" %in% result$relationship_table_name)
  expect_true("rel_type2_relationship_v2" %in% result$relationship_table_name)
})

test_that("get_relationship_metadata() works without explicit connection", {
  # Should use default connection from connect_to_db()
  expect_no_error(result <- get_relationship_metadata())
  expect_s3_class(result, "data.frame")
})

test_that("read_table_from_db() fails for non-existent table", {
  expect_error(
    read_table_from_db(con, "nonexistent_table"),
    "does not exist"
  )
})

test_that("get_codeminer_metadata() returns all metadata as a list by default", {
  result <- get_codeminer_metadata()

  expect_type(result, "list")
  expect_named(result, c("lookup", "mapping", "relationship"))
  expect_s3_class(result$lookup, "data.frame")
  expect_s3_class(result$mapping, "data.frame")
  expect_s3_class(result$relationship, "data.frame")
})

test_that("get_codeminer_metadata() returns a data frame for single type", {
  result_lookup <- get_codeminer_metadata("lookup")
  result_mapping <- get_codeminer_metadata("mapping")
  result_relationship <- get_codeminer_metadata("relationship")

  expect_s3_class(result_lookup, "data.frame")
  expect_s3_class(result_mapping, "data.frame")
  expect_s3_class(result_relationship, "data.frame")

  expect_true("lookup_table_name" %in% names(result_lookup))
  expect_true("mapping_table_name" %in% names(result_mapping))
  expect_true("relationship_table_name" %in% names(result_relationship))
})

test_that("get_codeminer_metadata() returns a list for multiple types", {
  result <- get_codeminer_metadata(c("lookup", "mapping"))

  expect_type(result, "list")
  expect_named(result, c("lookup", "mapping"))
  expect_s3_class(result$lookup, "data.frame")
  expect_s3_class(result$mapping, "data.frame")
})

test_that("get_codeminer_metadata() fails for invalid type", {
  expect_error(
    get_codeminer_metadata("invalid"),
    '`type` must be one of "lookup", "mapping", or "relationship"'
  )
})
