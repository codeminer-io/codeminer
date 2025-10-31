withr::local_options(
  # Silence logs generated with cli
  cli.default_handler = function(...) {}
)

test_that("create_dummy_database() creates a valid database file", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  result <- create_dummy_database(temp_db)

  expect_true(file.exists(temp_db))
  expect_identical(result, temp_db)

  con <- connect_to_db(temp_db)
  expect_true(check_database(con))
})

test_that("create_dummy_database() sets CODEMINER_DB_PATH environment variable", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")

  withr::local_envvar(CODEMINER_DB_PATH = "")
  create_dummy_database(temp_db)

  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), temp_db)
})

test_that("create_dummy_database() uses default temp path when no path provided", {
  result <- create_dummy_database()

  expect_true(file.exists(result))
  expect_match(result, "\\.duckdb$")
  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), result)
})

test_that("create_dummy_database() fails with extra arguments", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")

  expect_error(
    create_dummy_database(temp_db, extra_arg = "foo"),
    "must be empty"
  )
})
