test_that("add_icd10cm() errors on nonexistent path", {
  expect_error(
    add_icd10cm(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_icd10cm() returns the read data invisibly", {
  path <- create_dummy_icd10cm_file()
  local_build_temp_database()

  result <- suppressMessages(add_icd10cm(path = path))

  expect_named(result, c("icd10cm_lkp", "icd10cm_relationship"))
  expect_s3_class(result$icd10cm_lkp$lookup$table, "data.frame")
})

test_that("add_icd10cm() adds lookup and relationship tables to the database", {
  path <- create_dummy_icd10cm_file()
  local_build_temp_database()

  suppressMessages(add_icd10cm(path = path))

  con <- connect_to_db()
  expect_true(table_exists(con, "ICD-10-CM_20261001"))
  expect_true(table_exists(con, "ICD-10-CM_relationship_20261001"))
})

test_that("add_icd10cm() tables work with CODES(), CHILDREN() and PARENTS()", {
  path <- create_dummy_icd10cm_file()
  local_build_temp_database()
  suppressMessages(add_icd10cm(path = path))

  codes <- suppressMessages(CODES("T07XXXA", type = "ICD-10-CM"))
  expect_equal(
    codes$description,
    "Dummy unspecified multiple injuries, initial encounter"
  )

  children <- suppressMessages(CHILDREN("T07", type = "ICD-10-CM"))
  expect_setequal(children$code, c("T07", "T07XXXA"))

  parents <- suppressMessages(PARENTS("E113211", type = "ICD-10-CM"))
  expect_setequal(parents$code, c("E113211", "E11321", "E1132", "E113", "E11"))
})

test_that("add_icd10cm() skips when all tables already exist", {
  path <- create_dummy_icd10cm_file()
  local_build_temp_database()
  suppressMessages(add_icd10cm(path = path))

  expect_message(
    result <- add_icd10cm(path = path),
    "already exist.*skipping"
  )
  expect_null(result)
})

test_that("add_icd10cm() uses an explicit version", {
  path <- create_dummy_icd10cm_file()
  local_build_temp_database()

  suppressMessages(add_icd10cm(path = path, version = "custom"))

  con <- connect_to_db()
  expect_true(table_exists(con, "ICD-10-CM_custom"))
})

test_that("add_icd10cm() works against a parquet folder backend", {
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    codeminer_disconnect()
  }
  temp_dir <- withr::local_tempfile()
  dir.create(temp_dir)
  withr::local_envvar(CODEMINER_DB_PATH = temp_dir)
  withr::defer(codeminer_disconnect())
  build_database(overwrite = TRUE, format = "parquet")
  codeminer_connect(main = temp_dir)

  path <- create_dummy_icd10cm_file()
  suppressMessages(add_icd10cm(path = path))

  expect_true(file.exists(file.path(temp_dir, "ICD-10-CM_20261001.parquet")))
  codes <- suppressMessages(CODES("E11", type = "ICD-10-CM"))
  expect_equal(codes$description, "Dummy type 2 diabetes mellitus")

  # Re-adding is a no-op on this backend too
  expect_message(
    result <- add_icd10cm(path = path),
    "already exist.*skipping"
  )
  expect_null(result)
})
