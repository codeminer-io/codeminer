test_that("add_read3_trud() errors on nonexistent path", {
  expect_error(
    add_read3_trud(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_read3_trud() returns the read data invisibly", {
  dir <- create_dummy_read3_dir()
  local_build_temp_database()

  result <- suppressMessages(add_read3_trud(path = dir, version = "test_v1"))

  expect_named(result, c("read3_lkp", "read3_relationship"))
  expect_s3_class(result$read3_lkp$lookup$table, "data.frame")
  expect_s3_class(result$read3_relationship$relationship$table, "data.frame")
})

test_that("add_read3_trud() adds both tables to database", {
  dir <- create_dummy_read3_dir()
  local_build_temp_database()

  suppressMessages(add_read3_trud(path = dir, version = "test_v1"))

  con <- connect_to_db()
  expect_true(table_exists(con, "Read v3_test_v1"))
  expect_true(table_exists(con, "Read v3_relationship_test_v1"))
})

test_that("add_read3_trud() tables argument passed through to read_read3_trud()", {
  dir <- create_dummy_read3_dir()
  local_build_temp_database()

  result <- suppressMessages(add_read3_trud(
    path = dir,
    tables = "read3_lkp",
    version = "test_v1"
  ))

  expect_named(result, "read3_lkp")
})
