test_that("add_read2_trud() errors on nonexistent path", {
  expect_error(
    add_read2_trud(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_read2_trud() returns the read data invisibly", {
  dir <- create_dummy_read2_dir()
  local_build_temp_database()

  result <- suppressMessages(add_read2_trud(path = dir, version = "test_v1"))

  expect_named(result, c("read2_lkp", "read2_ctv3", "ctv3_read2"))
  expect_s3_class(result$read2_lkp$lookup$table, "data.frame")
})

test_that("add_read2_trud() adds all tables to database", {
  dir <- create_dummy_read2_dir()
  local_build_temp_database()

  suppressMessages(add_read2_trud(path = dir, version = "test_v1"))

  con <- connect_to_db()
  expect_true(table_exists(con, "read2_test_v1"))
  expect_true(table_exists(con, "read2_read3_test_v1"))
  expect_true(table_exists(con, "read3_read2_test_v1"))
})

test_that("add_read2_trud() tables argument passed through to read_read2_trud()", {
  dir <- create_dummy_read2_dir()
  local_build_temp_database()

  result <- suppressMessages(add_read2_trud(
    path = dir,
    tables = "read2_lkp",
    version = "test_v1"
  ))

  expect_named(result, "read2_lkp")
})
