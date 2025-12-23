# Lookup tables ---------------------------------------------------------------------------------------------------

test_that("add_lookup_table works with dummy data", {
  local_build_temp_database()

  test_table <- data.frame(
    code = c("a", "b", "c"),
    description = c("letter A", "letter B", "letter C")
  )
  test_metadata <- lookup_metadata("_test", lookup_version = "v99")

  expect_no_error(
    add_lookup_table(test_table, test_metadata)
  )
  expect_true(table_exists(
    con = connect_to_db(),
    test_metadata$lookup_table_name
  ))
})

test_that("add_lookup_table fails without valid database", {
  # Uninitialised db
  local_temp_database()

  expect_error(
    add_lookup_table(
      data.frame(code = "foo", description = "bar"),
      lookup_metadata("foo", lookup_version = "v0")
    ),
    "The database is not initialised"
  )
})

test_that("add_lookup_table warns when lookup_table_name already exists", {
  local_build_temp_database()

  test_table <- data.frame(code = "foo", description = "bar")
  test_metadata <- lookup_metadata("foo", lookup_version = "v0")

  # Adding same metadata twice should warn
  expect_no_error(add_lookup_table(test_table, test_metadata))
  expect_warning(
    add_lookup_table(test_table, test_metadata),
    "The lookup table foo_v0 already exists."
  )
})

test_that("add_lookup_table fails with invalid metadata", {
  local_build_temp_database()

  test_table <- data.frame(code = "foo", description = "bar")
  test_metadata <- list(foo = "bar")

  expect_error(
    add_lookup_table(test_table, test_metadata),
    "The metadata in `test_metadata` is incomplete"
  )
})

test_that("add_lookup_table fails when column names (my_code and my_description) in metadata don't exist in table", {
  test_table <- data.frame(my_code = "E10", my_description = "Type 1 diabetes")
  test_metadata <- lookup_metadata(
    code_type = "test",
    lookup_code_col = "code",
    lookup_description_col = "description"
  )

  expect_error(
    add_lookup_table(test_table, test_metadata),
    "Invalid metadata supplied.*lookup_code_col"
  )
})

test_that("add_lookup_table succeeds with custom column names (my_code and my_description)", {
  local_build_temp_database()

  test_table <- data.frame(my_code = "a", my_description = "letter A")
  test_metadata <- lookup_metadata(
    code_type = "custom_cols",
    lookup_code_col = "my_code",
    lookup_description_col = "my_description"
  )

  expect_no_error(add_lookup_table(test_table, test_metadata))
})

# mapping tables --------------------------------------------------------------------------------------------------

test_that("add_mapping_table works with example data", {
  local_build_temp_database()

  test_table <- example_ontology$mapping_tables$capital_to_lowercase_v3
  test_metadata <- example_ontology$mapping_metadata |>
    dplyr::filter(mapping_table_name == "capital_to_lowercase_v3")

  expect_no_error(
    add_mapping_table(test_table, test_metadata)
  )
  expect_true(table_exists(
    con = connect_to_db(),
    test_metadata$mapping_table_name
  ))
})

test_that("add_mapping_table fails without valid database", {
  # Uninitialised db
  local_temp_database()

  expect_error(
    add_mapping_table(
      data.frame(from = "foo1", to = "bar1"),
      mapping_metadata("foo", "bar", map_version = "v0")
    ),
    "The database is not initialised"
  )
})

test_that("add_mapping_table warns when mapping_table_name already exists", {
  local_build_temp_database()

  test_table <- data.frame(from = "foo1", to = "bar1")
  test_metadata <- mapping_metadata("foo", "bar", map_version = "v0")

  # Adding same metadata twice should warn
  expect_no_error(add_mapping_table(test_table, test_metadata))
  expect_warning(
    add_mapping_table(test_table, test_metadata),
    "The mapping table foo_bar_v0 already exists."
  )
})

test_that("add_mapping_table fails with invalid metadata", {
  local_build_temp_database()

  test_table <- data.frame(from = "foo", to = "bar")
  test_metadata <- list(foo = "bar")

  expect_error(
    add_mapping_table(test_table, test_metadata),
    "The metadata in `test_metadata` is incomplete"
  )
})

# Relationship tables ---------------------------------------------------------------------------------------------

test_that("add_relationship_table works with dummy data", {
  local_build_temp_database()

  test_table <- data.frame(
    from = c("a", "b", "c"),
    to = c("parent_a", "parent_b", "parent_c"),
    type = c("is a", "is a", "is a")
  )
  test_metadata <- relationship_metadata("_test", relationship_version = "v99")

  expect_no_error(
    add_relationship_table(test_table, test_metadata)
  )
  expect_true(table_exists(
    con = connect_to_db(),
    test_metadata$relationship_table_name
  ))
})

test_that("add_relationship_table fails without valid database", {
  # Uninitialised db
  local_temp_database()

  expect_error(
    add_relationship_table(
      data.frame(from = "foo", to = "bar", type = "is a"),
      relationship_metadata("foo", relationship_version = "v0")
    ),
    "The database is not initialised"
  )
})

test_that("add_relationship_table warns when relationship_table_name already exists", {
  local_build_temp_database()

  test_table <- data.frame(from = "foo", to = "bar", type = "is a")
  test_metadata <- relationship_metadata("foo", relationship_version = "v0")

  # Adding same metadata twice should warn
  expect_no_error(add_relationship_table(test_table, test_metadata))
  expect_warning(
    add_relationship_table(test_table, test_metadata),
    "The relationship table foo_relationship_v0 already exists."
  )
})

test_that("add_relationship_table fails with invalid metadata", {
  local_build_temp_database()

  test_table <- data.frame(from = "foo", to = "bar", type = "is a")
  test_metadata <- list(foo = "bar")

  expect_error(
    add_relationship_table(test_table, test_metadata),
    "The metadata in `test_metadata` is incomplete"
  )
})
