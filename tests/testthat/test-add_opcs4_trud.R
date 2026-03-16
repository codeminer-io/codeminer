test_that("add_opcs4_trud() errors on nonexistent path", {
  expect_error(add_opcs4_trud(path = "/nonexistent/path"), "Path does not exist")
})

test_that("add_opcs4_trud() returns the read data invisibly", {
  dir <- create_dummy_opcs4_dir()
  local_build_temp_database()

  result <- suppressMessages(add_opcs4_trud(path = dir, version = "test_v1"))

  expect_named(result, "opcs4_lkp")
  expect_s3_class(result$opcs4_lkp$lookup$table, "data.frame")
})

test_that("add_opcs4_trud() adds lookup table to database", {
  dir <- create_dummy_opcs4_dir()
  local_build_temp_database()

  suppressMessages(add_opcs4_trud(path = dir, version = "test_v1"))

  con <- connect_to_db()
  expect_true(table_exists(con, "OPCS4_test_v1"))
})
