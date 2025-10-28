withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("db_path() returns correct path based on environment variable", {
  original_path <- Sys.getenv("CODEMINER_DB_PATH")
  on.exit(Sys.setenv(CODEMINER_DB_PATH = original_path), add = TRUE)

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
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = temp_db)

  con <- connect_to_db()
  expect_s4_class(con, "duckdb_connection")

  # Verify connection works
  expect_true(DBI::dbIsValid(con))
  result <- DBI::dbListTables(con)
  expect_type(result, "character")
})

test_that("build_database() creates a valid codeminer database", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = temp_db)

  expect_false(file.exists(temp_db))
  build_database()
  expect_true(file.exists(temp_db))

  con <- connect_to_db()
  tables <- DBI::dbListTables(con)
  expect_type(tables, "character")
  expect_length(tables, 2)
  tables <- DBI::dbListTables(con)
  expect_true("lookup_metadata" %in% tables)
  expect_true("mapping_metadata" %in% tables)

  # Check table schema
  lookup_fields <- DBI::dbListFields(con, "lookup_metadata")
  required_lookup_fields <- required_lookup_metadata_columns()
  for (x in required_lookup_fields) {
    expect_true(x %in% lookup_fields)
  }

  mapping_fields <- DBI::dbListFields(con, "mapping_metadata")
  required_mapping_fields <- required_mapping_metadata_columns()
  for (x in required_mapping_fields) {
    expect_true(x %in% mapping_fields)
  }
})

test_that("build_database() is idempotent", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = temp_db)

  # Create database first time
  build_database(overwrite = FALSE)

  # Should work when called again
  result <- build_database(overwrite = FALSE)
  expect_true(result)
})

test_that("create_lookup_metadata_table() respects overwrite parameter", {
  temp_db <- tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = temp_db)
  on.exit(unlink(temp_db), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table first time
  create_lookup_metadata_table(con, overwrite = FALSE)

  # Insert a test row
  DBI::dbExecute(
    con,
    "INSERT INTO lookup_metadata (lookup_table_name) VALUES ('test')"
  )
  row_count_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM lookup_metadata"
  )$n
  expect_equal(row_count_before, 1)

  # Try to create again without overwrite - should not recreate
  create_lookup_metadata_table(con, overwrite = FALSE)
  row_count_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM lookup_metadata"
  )$n
  expect_equal(row_count_after, 1)

  # Create with overwrite - should recreate empty table
  create_lookup_metadata_table(con, overwrite = TRUE)
  row_count_overwrite <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM lookup_metadata"
  )$n
  expect_equal(row_count_overwrite, 0)
})


test_that("create_mapping_metadata_table() respects overwrite parameter", {
  temp_db <- tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = temp_db)
  on.exit(unlink(temp_db), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table and insert data
  create_mapping_metadata_table(con, overwrite = FALSE)
  DBI::dbExecute(
    con,
    "INSERT INTO mapping_metadata (mapping_table_name) VALUES ('test')"
  )
  row_count_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM mapping_metadata"
  )$n
  expect_equal(row_count_before, 1)

  # Overwrite should clear the table
  create_mapping_metadata_table(con, overwrite = TRUE)
  row_count_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM mapping_metadata"
  )$n
  expect_equal(row_count_after, 0)
})
