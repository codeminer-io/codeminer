withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)


# Lookup tables ---------------------------------------------------------------------------------------------------

test_that("add_lookup_table works with example data", {
  local_build_temp_database()

  test_table <- example_ontology$lookup_tables$capital_letters_v3
  test_metadata <- example_ontology$lookup_metadata |>
    dplyr::filter(lookup_table_name == "capital_letters_v3")

  expect_no_error(
    add_lookup_table(test_table, test_metadata)
  )
})

test_that("add_lookup_table fails without valid database", {
  # Uninitialised db
  local_temp_database()

  expect_error(
    add_lookup_table(
      data.frame(code = "foo", description = "bar"),
      lookup_metadata("foo", version = "v0")
    ),
    "The database is not initialised"
  )
})

test_that("add_lookup_table warns when lookup_table_name already exists", {
  local_build_temp_database()

  test_table <- data.frame(code = "foo", description = "bar")
  test_metadata <- lookup_metadata("foo", version = "v0")

  # Adding same metadata twice should warn
  expect_no_error(add_lookup_table(test_table, test_metadata))
  expect_warning(
    add_lookup_table(test_table, test_metadata),
    "The lookup table foo_v0 already exists."
  )
})

test_that("add_lookup_metadata can take multiple entries", {
  local_build_temp_database()
  con <- connect_to_db()

  test_metadata <- lookup_metadata(c("foo", "bar", "baz"))

  expect_no_error(add_lookup_metadata(con, test_metadata))
  expect_equal(nrow(get_lookup_metadata(con)), 3)
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


# mapping tables --------------------------------------------------------------------------------------------------

test_that("add_mapping_table works with example data", {
  local_build_temp_database()

  test_table <- example_ontology$mapping_tables$capital_to_lowercase_v3
  test_metadata <- example_ontology$mapping_metadata |>
    dplyr::filter(mapping_table_name == "capital_to_lowercase_v3")

  expect_no_error(
    add_mapping_table(test_table, test_metadata)
  )
})

test_that("add_mapping_table fails without valid database", {
  # Uninitialised db
  local_temp_database()

  expect_error(
    add_mapping_table(
      data.frame(from = "foo1", to = "bar1"),
      mapping_metadata("foo", "bar", version = "v0")
    ),
    "The database is not initialised"
  )
})

test_that("add_mapping_table warns when mapping_table_name already exists", {
  local_build_temp_database()

  test_table <- data.frame(from = "foo1", to = "bar1")
  test_metadata <- mapping_metadata("foo", "bar", version = "v0")

  # Adding same metadata twice should warn
  expect_no_error(add_mapping_table(test_table, test_metadata))
  expect_warning(
    add_mapping_table(test_table, test_metadata),
    "The mapping table foo_bar_v0 already exists."
  )
})

test_that("add_mapping_metadata can take multiple entries", {
  local_build_temp_database()
  con <- connect_to_db()

  test_metadata <- mapping_metadata(
    c("foo", "bar", "baz"),
    c("alice", "bob", "charlie")
  )

  expect_no_error(add_mapping_metadata(con, test_metadata))
  expect_equal(nrow(get_mapping_metadata(con)), 3)
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
