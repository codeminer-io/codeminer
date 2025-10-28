withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("add_lookup_metadata fails when lookup_table_name already exists", {
  local_build_temp_database()
  con <- connect_to_db()

  test_metadata <- lookup_metadata("foo", version = "v0")

  # Adding same metadata twice should fail
  expect_no_error(add_lookup_metadata(con, test_metadata))
  expect_error(
    add_lookup_metadata(con, test_metadata),
    "Metadata for foo_v0 already exists."
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

  test_table <- data.frame()
  test_metadata <- list(foo = "bar")

  expect_error(
    add_lookup_table(test_table, test_metadata),
    "The metadata in `test_metadata` is incomplete"
  )
})

test_that("add_lookup_table works with example data", {
  local_build_temp_database()

  test_table <- example_ontology$lookup_tables$capital_letters_v3
  test_metadata <- example_ontology$lookup_metadata |>
    dplyr::filter(lookup_table_name == "capital_letters_v3")

  expect_no_error(
    add_lookup_table(test_table, test_metadata)
  )
})
