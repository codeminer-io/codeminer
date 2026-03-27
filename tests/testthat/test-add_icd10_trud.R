test_that("add_icd10_trud() errors on nonexistent path", {
  expect_error(
    add_icd10_trud(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_icd10_trud() returns the read data invisibly", {
  dir <- create_dummy_icd10_dir()
  local_build_temp_database()

  result <- suppressMessages(add_icd10_trud(path = dir, version = "test_v1"))

  expect_named(result, c("icd10_lkp", "icd10_relationship"))
  expect_s3_class(result$icd10_lkp$lookup$table, "data.frame")
})

test_that("add_icd10_trud() adds lookup table to database", {
  dir <- create_dummy_icd10_dir()
  local_build_temp_database()

  suppressMessages(add_icd10_trud(path = dir, version = "test_v1"))

  con <- connect_to_db()
  expect_true(table_exists(con, "ICD-10_test_v1"))
})
