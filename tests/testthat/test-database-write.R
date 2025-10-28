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

test_that("add_lookup_metadata fails with invalid metadata", {
  local_build_temp_database()
  con <- connect_to_db()

  test_metadata <- list(foo = "bar")

  expect_error(
    add_lookup_metadata(con, test_metadata),
    "The metadata in `test_metadata` is incomplete"
  )
})
