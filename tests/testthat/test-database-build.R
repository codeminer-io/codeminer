withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

lookup_metadata_name <- "_lookup_metadata"
mapping_metadata_name <- "_mapping_metadata"

test_that("db_path() returns correct path based on environment variable", {
  custom_path <- "/tmp/test_db.duckdb"
  withr::with_envvar(
    new = c(CODEMINER_DB_PATH = custom_path),
    expect_equal(db_path(), custom_path)
  )

  # Test with empty env var (default behavior)
  withr::with_envvar(
    new = c(CODEMINER_DB_PATH = ""),
    {
      result <- db_path()
      expect_identical(basename(dirname(result)), "codeminer")
      expect_identical(basename(result), "ontology.duckdb")
      expect_true(fs::is_absolute_path(result))
    }
  )
})

test_that("connect_to_db() creates a valid DuckDB connection", {
  local_temp_database()

  con <- connect_to_db()
  expect_s4_class(con, "duckdb_connection")

  # Verify connection works
  expect_true(DBI::dbIsValid(con))
  result <- DBI::dbListTables(con)
  expect_type(result, "character")
})

test_that("build_database() creates a valid codeminer database", {
  temp_db <- local_temp_database()

  expect_false(file.exists(temp_db))
  build_database()
  expect_true(file.exists(temp_db))

  con <- connect_to_db()
  tables <- DBI::dbListTables(con)
  expect_type(tables, "character")
  expect_length(tables, 2)
  expect_true(lookup_metadata_name %in% tables)
  expect_true(mapping_metadata_name %in% tables)

  # Check table schema
  lookup_fields <- DBI::dbListFields(con, lookup_metadata_name)
  required_lookup_fields <- required_lookup_metadata_columns()
  for (x in required_lookup_fields) {
    expect_true(x %in% lookup_fields)
  }

  mapping_fields <- DBI::dbListFields(con, mapping_metadata_name)
  required_mapping_fields <- required_mapping_metadata_columns()
  for (x in required_mapping_fields) {
    expect_true(x %in% mapping_fields)
  }
})

test_that("build_database() is idempotent", {
  local_temp_database()

  # Create database first time
  build_database(overwrite = FALSE)

  # Should work when called again
  result <- build_database(overwrite = FALSE)
  expect_true(result)
})

test_that("create_lookup_metadata_table() respects overwrite parameter", {
  local_temp_database()
  con <- connect_to_db()

  # Create table first time
  create_lookup_metadata_table(con, overwrite = FALSE)

  # Insert a test row
  DBI::dbExecute(
    con,
    "INSERT INTO _lookup_metadata (lookup_table_name) VALUES ('test')"
  )
  row_count_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM _lookup_metadata"
  )$n
  expect_equal(row_count_before, 1)

  # Try to create again without overwrite - should not recreate
  create_lookup_metadata_table(con, overwrite = FALSE)
  row_count_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM _lookup_metadata"
  )$n
  expect_equal(row_count_after, 1)

  # Create with overwrite - should recreate empty table
  create_lookup_metadata_table(con, overwrite = TRUE)
  row_count_overwrite <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM _lookup_metadata"
  )$n
  expect_equal(row_count_overwrite, 0)
})


test_that("create_mapping_metadata_table() respects overwrite parameter", {
  local_temp_database()
  con <- connect_to_db()

  # Create table and insert data
  create_mapping_metadata_table(con, overwrite = FALSE)
  DBI::dbExecute(
    con,
    "INSERT INTO _mapping_metadata (mapping_table_name) VALUES ('test')"
  )
  row_count_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM _mapping_metadata"
  )$n
  expect_equal(row_count_before, 1)

  # Overwrite should clear the table
  create_mapping_metadata_table(con, overwrite = TRUE)
  row_count_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM _mapping_metadata"
  )$n
  expect_equal(row_count_after, 0)
})
