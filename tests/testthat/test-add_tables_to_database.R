local_build_temp_database()

test_that("add_tables_to_database() works with nested table structure", {
  # Create a nested structure with lookup, mapping, and relationship tables
  test_data <- list(
    test_sheet = list(
      lookup = list(
        table = data.frame(
          code = c("A", "B", "C"),
          description = c("Letter A", "Letter B", "Letter C")
        ),
        metadata = lookup_metadata("test_codes", lookup_version = "v1")
      ),
      mapping = list(
        table = data.frame(
          from = c("A", "B", "C"),
          to = c("1", "2", "3")
        ),
        metadata = mapping_metadata(
          "test_codes",
          "test_numbers",
          map_version = "v1"
        )
      ),
      relationship = list(
        table = data.frame(
          from = c("A", "B"),
          to = c("C", "C"),
          type = c("is a", "is a")
        ),
        metadata = relationship_metadata(
          "test_codes",
          relationship_version = "v1"
        )
      )
    )
  )

  expect_no_error(
    add_tables_to_database(table_list = test_data)
  )

  # Verify tables were added
  con <- connect_to_db()
  expect_true(table_exists(con, "test_codes_v1"))
  expect_true(table_exists(con, "test_codes_test_numbers_v1"))
  expect_true(table_exists(con, "test_codes_relationship_v1"))
})

test_that("add_tables_to_database() handles invalid table type", {
  test_data <- list(
    test_sheet = list(
      invalid_type = list(
        table = data.frame(x = 1),
        metadata = list()
      )
    )
  )

  expect_error(
    add_tables_to_database(table_list = test_data),
    "Invalid table type 'invalid_type' for argument 'test_sheet'"
  )
})

test_that("add_tables_to_database() works with single table type", {
  test_data <- list(
    just_lookup = list(
      lookup = list(
        table = data.frame(
          code = "X",
          description = "Letter X"
        ),
        metadata = lookup_metadata("single_test", lookup_version = "v1")
      )
    )
  )

  expect_no_error(
    add_tables_to_database(table_list = test_data)
  )

  con <- connect_to_db()
  expect_true(table_exists(con, "single_test_v1"))
})
