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

test_that("add_lookup_table handles a table name ending in a data-file extension (#171)", {
  local_build_temp_database()

  test_table <- data.frame(
    code = c("01", "0101"),
    description = c("Chapter", "Section")
  )
  # A version derived from a CSV file name yields a table name ending in
  # ".csv"; DuckDB's replacement scan would otherwise treat it as a file and
  # abort the write transaction.
  test_metadata <- lookup_metadata(
    "BNF",
    lookup_version = "bnf_code_current_202503_version_88.csv"
  )

  expect_no_error(add_lookup_table(test_table, test_metadata))

  result <- dplyr::collect(get_lookup_table("BNF"))
  expect_setequal(result$code, c("01", "0101"))
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

test_that("relationship_metadata() defaults to a purely hierarchical table", {
  meta <- relationship_metadata("foo", relationship_version = "v0")
  expect_equal(meta$type_col, NA_character_)
  expect_equal(meta$child_parent_relationship_code, NA_character_)
})

test_that("relationship_metadata() requires type_col and child_parent_relationship_code to be set together", {
  # type_col set but no hierarchical value
  expect_error(
    relationship_metadata("foo", type_col = "type"),
    class = "codeminer_error"
  )
  # hierarchical value set but no type column
  expect_error(
    relationship_metadata(
      "foo",
      child_parent_relationship_code = "is a"
    ),
    class = "codeminer_error"
  )
  # both set is valid
  expect_no_error(
    relationship_metadata(
      "foo",
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  )
  # both NA (default) is valid
  expect_no_error(relationship_metadata("foo"))
})

test_that("add_relationship_table() rejects a half-specified type pairing on read-back", {
  local_build_temp_database()

  # Bypass the constructor's check to confirm validate_relationship_metadata()
  # also catches a contradictory pairing.
  meta <- relationship_metadata(
    "foo",
    type_col = "type",
    child_parent_relationship_code = "is a"
  )
  meta$child_parent_relationship_code <- NA_character_

  expect_error(
    add_relationship_table(
      data.frame(from = "a", to = "b", type = "is a"),
      meta
    ),
    class = "codeminer_error"
  )
})
