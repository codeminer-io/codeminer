# remove_lookup_table() ------------------------------------------------------

test_that("remove_lookup_table() removes data table and metadata", {
  local_build_temp_database()

  test_table <- data.frame(
    code = c("A", "B"),
    description = c("A desc", "B desc")
  )
  add_lookup_table(
    test_table,
    lookup_metadata("rm_test", lookup_version = "v1")
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "rm_test_v1"))
  expect_true("rm_test_v1" %in% get_lookup_metadata(con)$lookup_table_name)

  remove_lookup_table("rm_test", "v1")

  # Re-read from DB (cache was refreshed by connect_to_db's re-ATTACH)
  con <- connect_to_db()
  expect_false(table_exists(con, "rm_test_v1"))
  expect_false("rm_test_v1" %in% get_lookup_metadata(con)$lookup_table_name)
})

test_that("remove_lookup_table() errors for non-existent table", {
  local_build_temp_database()

  expect_error(
    remove_lookup_table("nonexistent", "v1"),
    "No lookup table found"
  )
})

test_that("remove_lookup_table() allows re-adding after removal", {
  local_build_temp_database()

  test_table <- data.frame(code = "X", description = "X desc")
  add_lookup_table(
    test_table,
    lookup_metadata("readd_test", lookup_version = "v1")
  )
  remove_lookup_table("readd_test", "v1")

  # Re-add the same table
  expect_no_error(
    add_lookup_table(
      test_table,
      lookup_metadata("readd_test", lookup_version = "v1")
    )
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "readd_test_v1"))
})

# remove_mapping_table() -----------------------------------------------------

test_that("remove_mapping_table() removes data table and metadata", {
  local_build_temp_database()

  test_table <- data.frame(from = c("A", "B"), to = c("1", "2"))
  add_mapping_table(
    test_table,
    mapping_metadata("rm_from", "rm_to", map_version = "v1")
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "rm_from_rm_to_v1"))
  expect_true(
    "rm_from_rm_to_v1" %in% get_mapping_metadata(con)$mapping_table_name
  )

  remove_mapping_table("rm_from", "rm_to", "v1")

  con <- connect_to_db()
  expect_false(table_exists(con, "rm_from_rm_to_v1"))
  expect_false(
    "rm_from_rm_to_v1" %in% get_mapping_metadata(con)$mapping_table_name
  )
})

test_that("remove_mapping_table() errors for non-existent table", {
  local_build_temp_database()

  expect_error(
    remove_mapping_table("nonexistent", "also_no", "v1"),
    "No mapping table found"
  )
})

test_that("remove_mapping_table() allows re-adding after removal", {
  local_build_temp_database()

  test_table <- data.frame(from = "X", to = "1")
  add_mapping_table(
    test_table,
    mapping_metadata("readd_from", "readd_to", map_version = "v1")
  )
  remove_mapping_table("readd_from", "readd_to", "v1")

  expect_no_error(
    add_mapping_table(
      test_table,
      mapping_metadata("readd_from", "readd_to", map_version = "v1")
    )
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "readd_from_readd_to_v1"))
})

# remove_relationship_table() ------------------------------------------------

test_that("remove_relationship_table() removes data table and metadata", {
  local_build_temp_database()

  test_table <- data.frame(from = c("A", "B"), to = c("C", "C"), type = "is a")
  add_relationship_table(
    test_table,
    relationship_metadata("rm_rel", relationship_version = "v1")
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "rm_rel_relationship_v1"))
  expect_true(
    "rm_rel_relationship_v1" %in%
      get_relationship_metadata(con)$relationship_table_name
  )

  remove_relationship_table("rm_rel", "v1")

  con <- connect_to_db()
  expect_false(table_exists(con, "rm_rel_relationship_v1"))
  expect_false(
    "rm_rel_relationship_v1" %in%
      get_relationship_metadata(con)$relationship_table_name
  )
})

test_that("remove_relationship_table() errors for non-existent table", {
  local_build_temp_database()

  expect_error(
    remove_relationship_table("nonexistent", "v1"),
    "No relationship table found"
  )
})

test_that("remove_relationship_table() allows re-adding after removal", {
  local_build_temp_database()

  test_table <- data.frame(from = "A", to = "B", type = "is a")
  add_relationship_table(
    test_table,
    relationship_metadata("readd_rel", relationship_version = "v1")
  )
  remove_relationship_table("readd_rel", "v1")

  expect_no_error(
    add_relationship_table(
      test_table,
      relationship_metadata("readd_rel", relationship_version = "v1")
    )
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "readd_rel_relationship_v1"))
})
