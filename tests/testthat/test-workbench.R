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
