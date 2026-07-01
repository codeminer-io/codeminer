test_that("create_dummy_database() creates a valid database file", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  suppressMessages(create_dummy_database(temp_db, .local = TRUE))

  expect_true(file.exists(temp_db))

  con <- connect_to_db(temp_db)
  expect_true(check_database(con))
})

test_that("create_dummy_database() sets CODEMINER_DB_PATH", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")

  withr::local_envvar(CODEMINER_DB_PATH = "")
  suppressMessages(create_dummy_database(temp_db))

  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), temp_db)
})

test_that("create_dummy_database() connects the workbench", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  suppressMessages(create_dummy_database(temp_db, .local = TRUE))

  # Workbench should already be connected — no lazy init needed
  expect_true(DBI::dbIsValid(.codeminer_env$con))
  expect_equal(.codeminer_env$db_path, temp_db)
})

test_that("create_dummy_database() uses default temp path", {
  suppressMessages(result <- create_dummy_database(.local = TRUE))

  expect_true(file.exists(result))
  expect_match(result, "\\.duckdb$")
  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), result)
})

test_that("create_dummy_database() returns db_path invisibly", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  expect_invisible(
    suppressMessages(create_dummy_database(temp_db, .local = TRUE))
  )
  expect_identical(
    suppressMessages(create_dummy_database(temp_db, .local = TRUE)),
    temp_db
  )
})

test_that("create_dummy_database() fails with extra arguments", {
  temp_db <- withr::local_tempfile(fileext = ".duckdb")

  expect_error(
    create_dummy_database(temp_db, extra_arg = "foo"),
    "must be empty"
  )
})

test_that(".local = TRUE cleans up on scope exit", {
  withr::local_envvar(CODEMINER_DB_PATH = "original_value")

  local({
    suppressMessages(create_dummy_database(.local = TRUE))
    # Inside scope: env var points to dummy db
    expect_true(Sys.getenv("CODEMINER_DB_PATH") != "original_value")
  })

  # After scope exit: env var restored and workbench disconnected
  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), "original_value")
})

test_that(".local = FALSE persists env var", {
  original <- Sys.getenv("CODEMINER_DB_PATH", unset = NA)
  withr::defer({
    if (is.na(original)) {
      Sys.unsetenv("CODEMINER_DB_PATH")
    } else {
      Sys.setenv(CODEMINER_DB_PATH = original)
    }
    suppressMessages(codeminer_disconnect())
  })

  temp_db <- withr::local_tempfile(fileext = ".duckdb")
  suppressMessages(create_dummy_database(temp_db))

  # Env var persists after the function returns
  expect_equal(Sys.getenv("CODEMINER_DB_PATH"), temp_db)
})

test_that("reconnection message shown when previous db exists", {
  temp_db1 <- withr::local_tempfile(fileext = ".duckdb")
  temp_db2 <- withr::local_tempfile(fileext = ".duckdb")

  # First: set up a "real" database
  suppressMessages(create_dummy_database(temp_db1, .local = TRUE))

  # Second: create another dummy — should mention reconnection
  msgs <- capture.output(
    create_dummy_database(temp_db2),
    type = "message"
  )
  msg_text <- paste(msgs, collapse = "\n")
  expect_match(msg_text, "reconnect", ignore.case = TRUE)
})
