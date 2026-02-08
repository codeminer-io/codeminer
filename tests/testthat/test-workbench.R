# codeminer_connect() -------------------------------------------------------

test_that("codeminer_connect() creates a valid persistent connection", {
  temp_db <- local_build_temp_database()

  expect_true(exists("con", envir = .codeminer_env))
  expect_true(DBI::dbIsValid(.codeminer_env$con))
})

test_that("get_db_con() returns the same connection on repeated calls", {
  local_build_temp_database()

  con1 <- get_db_con()
  con2 <- get_db_con()
  expect_identical(con1, con2)
})

test_that("get_db_con() auto-initializes workbench on first query", {
  temp_db <- local_temp_database()
  build_database(overwrite = TRUE)

  # Don't call codeminer_connect() -- let auto-init handle it
  con <- get_db_con()
  expect_true(DBI::dbIsValid(con))
  expect_true(exists("con", envir = .codeminer_env))
})

# codeminer_disconnect() ----------------------------------------------------

test_that("codeminer_disconnect() tears down connection", {
  local_build_temp_database()

  codeminer_disconnect()

  expect_false(exists("con", envir = .codeminer_env))
  expect_false(exists("db_paths", envir = .codeminer_env))
  expect_false(exists("metadata", envir = .codeminer_env))
})

test_that("codeminer_disconnect() preserves non-connection state", {
  local_build_temp_database()

  .codeminer_env$snomed_path <- "/tmp/test"
  codeminer_disconnect()

  expect_equal(.codeminer_env$snomed_path, "/tmp/test")

  # Clean up
  rm("snomed_path", envir = .codeminer_env)
})

# Metadata cache -----------------------------------------------------------

test_that("metadata cache list is initialized after connect", {
  local_build_temp_database()

  # The metadata list should exist (even if individual entries are NULL
  # for an empty database with no data rows)
  expect_true(!is.null(.codeminer_env$metadata))
  expect_true(is.list(.codeminer_env$metadata))
})

# READ_ONLY ----------------------------------------------------------------

test_that("main database is attached as READ_ONLY", {
  local_build_temp_database()

  expect_error(
    DBI::dbExecute(
      .codeminer_env$con,
      paste0(
        "CREATE TABLE ", CODEMINER_ALIAS_MAIN, "._test_write (x INTEGER)"
      )
    )
  )
})

# Search path --------------------------------------------------------------

test_that("search_path prioritises extra over main", {
  main_path <- withr::local_tempfile(fileext = ".duckdb")
  extra_path <- withr::local_tempfile(fileext = ".duckdb")

  # Populate main with a test table
  con_m <- DBI::dbConnect(duckdb::duckdb(), main_path)
  DBI::dbExecute(
    con_m,
    "CREATE TABLE _test_priority AS SELECT 'main' AS source"
  )
  DBI::dbDisconnect(con_m, shutdown = TRUE)

  # Populate extra with the same table name but different data
  con_e <- DBI::dbConnect(duckdb::duckdb(), extra_path)
  DBI::dbExecute(
    con_e,
    "CREATE TABLE _test_priority AS SELECT 'extra' AS source"
  )
  DBI::dbDisconnect(con_e, shutdown = TRUE)

  codeminer_connect(main = main_path, extra = extra_path)
  withr::defer(codeminer_disconnect())

  # Unqualified query should resolve to extra (search_path = 'user_db,core')
  res <- DBI::dbGetQuery(
    .codeminer_env$con,
    "SELECT source FROM _test_priority"
  )
  expect_equal(res$source, "extra")

  # Explicitly qualified queries should reach their respective databases
  query <- paste0(
    "SELECT source FROM ",
    CODEMINER_ALIAS_MAIN, "._test_priority"
  )
  res_main <- DBI::dbGetQuery(.codeminer_env$con, query)
  expect_equal(res_main$source, "main")
})

# connect_to_db() write path -----------------------------------------------

test_that("connect_to_db(read_only=FALSE) detaches and re-attaches", {
  temp_db <- local_build_temp_database()

  # Workbench should have the main db attached

  expect_identical(.codeminer_env$db_paths$main, temp_db)

  # Open a write connection (this should DETACH main from workbench)
  write_con <- connect_to_db(read_only = FALSE)

  # While write_con is open, workbench should not hold the file
  expect_null(.codeminer_env$db_paths$main)

  # Write connection should be valid and writable
  expect_true(DBI::dbIsValid(write_con))
})

# codeminer_set_version() --------------------------------------------------

test_that("codeminer_set_version() stores lookup pins", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  expect_equal(
    .codeminer_env$active_versions$lookup[["ICD-10"]],
    "UKB v4"
  )
})

test_that("codeminer_set_version() stores relationship pins", {
  local_build_temp_database()

  codeminer_set_version(relationship = c("ICD-10" = "UKB v4"))
  expect_equal(
    .codeminer_env$active_versions$relationship[["ICD-10"]],
    "UKB v4"
  )
})

test_that("codeminer_set_version() stores mapping pins", {
  local_build_temp_database()

  codeminer_set_version(mapping = c("Read 3 > ICD-10" = "UKB v4"))
  expect_equal(
    .codeminer_env$active_versions$mapping[["Read 3 > ICD-10"]],
    "UKB v4"
  )
})

test_that("codeminer_set_version() normalizes mapping key spacing", {
  local_build_temp_database()

  # No spaces around >
  codeminer_set_version(mapping = c("Read 3>ICD-10" = "UKB v4"))
  expect_equal(
    .codeminer_env$active_versions$mapping[["Read 3 > ICD-10"]],
    "UKB v4"
  )

  codeminer_clear_versions()

  # Extra spaces around >
  codeminer_set_version(mapping = c("Read 3  >   ICD-10" = "UKB v4"))
  expect_equal(
    .codeminer_env$active_versions$mapping[["Read 3 > ICD-10"]],
    "UKB v4"
  )
})

test_that("codeminer_set_version() trims whitespace from keys and values", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("  ICD-10  " = "  UKB v4  "))
  expect_equal(
    .codeminer_env$active_versions$lookup[["ICD-10"]],
    "UKB v4"
  )
})

test_that("codeminer_set_version() merges with existing pins", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  codeminer_set_version(lookup = c("Read 3" = "UKB v4"))

  expect_equal(.codeminer_env$active_versions$lookup[["ICD-10"]], "UKB v4")
  expect_equal(.codeminer_env$active_versions$lookup[["Read 3"]], "UKB v4")
})

test_that("codeminer_set_version() overwrites existing pin for same key", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  suppressWarnings(
    codeminer_set_version(lookup = c("ICD-10" = "UKB v5"))
  )

  expect_equal(.codeminer_env$active_versions$lookup[["ICD-10"]], "UKB v5")
})

test_that("codeminer_set_version() errors with no arguments", {
  local_build_temp_database()

  expect_error(
    codeminer_set_version(),
    "At least one"
  )
})

test_that("codeminer_set_version() errors with unnamed vector", {
  local_build_temp_database()

  expect_error(
    codeminer_set_version(lookup = c("UKB v4")),
    "named character vector"
  )
})

test_that("codeminer_set_version() errors when pinning to 'latest'", {
  local_build_temp_database()

  expect_error(
    codeminer_set_version(lookup = c("ICD-10" = "latest")),
    "latest.*cannot be used"
  )
})

test_that("codeminer_set_version() warns for non-existent version", {
  suppressMessages(local_build_temp_dummy_database())

  expect_warning(
    codeminer_set_version(lookup = c("ICD-10" = "nonexistent_version")),
    "No lookup metadata found"
  )
})

test_that("codeminer_set_version() errors for bad mapping key format", {
  local_build_temp_database()

  expect_error(
    codeminer_set_version(mapping = c("bad_format" = "v1")),
    "from > to"
  )
})

# codeminer_clear_versions() -----------------------------------------------

test_that("codeminer_clear_versions() removes all pins", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  expect_true(length(.codeminer_env$active_versions) > 0)

  codeminer_clear_versions()
  expect_equal(length(.codeminer_env$active_versions), 0)
})

test_that("codeminer_disconnect() also clears version pins", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  codeminer_disconnect()

  expect_false(exists("active_versions", envir = .codeminer_env))
})

# Pinned versions affect resolution ----------------------------------------

test_that("pinned lookup version overrides 'latest' resolution", {
  suppressMessages(local_build_temp_dummy_database())

  # Add a second version of ICD-10
  test_table <- data.frame(
    code = c("PIN1", "PIN2"),
    description = c("Pinned code 1", "Pinned code 2")
  )
  add_lookup_table(
    test_table,
    lookup_metadata("ICD-10", lookup_version = "UKB v5")
  )

  # Without pin, "latest" resolves to v5 (highest numeric)
  result_latest <- CODES("all", type = "ICD-10", lookup_version = "latest")
  expect_true("PIN1" %in% result_latest$code)

  # Pin to v4
  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))

  # Now "latest" should resolve to v4
  result_pinned <- CODES("all", type = "ICD-10", lookup_version = "latest")
  expect_false("PIN1" %in% result_pinned$code)

  # Explicit version still overrides the pin
  result_explicit <- CODES("all", type = "ICD-10", lookup_version = "UKB v5")
  expect_true("PIN1" %in% result_explicit$code)
})

test_that("pinned relationship version overrides 'latest' resolution", {
  suppressMessages(local_build_temp_dummy_database())

  con <- get_db_con()
  meta <- get_metadata_for_relationship(con, "ICD-10", "latest")
  expect_equal(meta$relationship_version, "UKB v4")

  # Pin to the known version and verify it's used
  codeminer_set_version(relationship = c("ICD-10" = "UKB v4"))
  meta_pinned <- get_metadata_for_relationship(con, "ICD-10", "latest")
  expect_equal(meta_pinned$relationship_version, "UKB v4")
})

test_that("pinned mapping version overrides 'latest' resolution", {
  suppressMessages(local_build_temp_dummy_database())

  con <- get_db_con()
  meta <- get_metadata_for_mapping(con, "Read 3", "ICD-10", "latest")
  expect_equal(meta$map_version, "UKB v4")

  # Pin the mapping version
  codeminer_set_version(mapping = c("Read 3 > ICD-10" = "UKB v4"))
  meta_pinned <- get_metadata_for_mapping(con, "Read 3", "ICD-10", "latest")
  expect_equal(meta_pinned$map_version, "UKB v4")
})

# Reserved "latest" version name -----------------------------------------

test_that("add_metadata_table() rejects 'latest' as a version name", {
  local_build_temp_database()

  meta <- lookup_metadata("test-type", lookup_version = "latest")
  test_table <- data.frame(code = "X1", description = "Test")

  expect_error(
    add_lookup_table(test_table, meta),
    "latest.*reserved"
  )
})

test_that("add_metadata_table() rejects '>' in code_type values", {
  local_build_temp_database()

  meta <- lookup_metadata("ICD > 10", lookup_version = "v1")
  test_table <- data.frame(code = "X1", description = "Test")

  expect_error(
    add_lookup_table(test_table, meta),
    "must not contain"
  )
})

# codeminer_status() with pins ---------------------------------------------

test_that("codeminer_status() shows pinned versions", {
  local_build_temp_database()

  codeminer_set_version(lookup = c("ICD-10" = "UKB v4"))
  expect_message(codeminer_status(), "Pinned versions")
})
