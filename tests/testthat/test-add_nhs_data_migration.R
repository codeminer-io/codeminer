test_that("add_nhs_data_migration() errors on nonexistent path", {
  expect_error(
    add_nhs_data_migration(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_nhs_data_migration() returns the read data invisibly", {
  dir <- create_dummy_nhs_data_migration_dir()
  local_build_temp_database()

  result <- suppressMessages(add_nhs_data_migration(
    path = dir,
    version = "test_v1"
  ))

  expect_named(
    result,
    c("ctv3sctmap2", "rcsctmap2", "read2_ctv3", "ctv3_read2")
  )
})

test_that("add_nhs_data_migration() adds all four mapping tables to database", {
  dir <- create_dummy_nhs_data_migration_dir()
  local_build_temp_database()

  suppressMessages(add_nhs_data_migration(path = dir, version = "test_v1"))

  con <- connect_to_db()
  expect_true(table_exists(con, "Read v3_SNOMED CT_test_v1"))
  expect_true(table_exists(con, "Read v2_SNOMED CT_test_v1"))
  expect_true(table_exists(con, "Read v2_Read v3_test_v1"))
  expect_true(table_exists(con, "Read v3_Read v2_test_v1"))
})

test_that("add_nhs_data_migration() tables argument passed through", {
  dir <- create_dummy_nhs_data_migration_dir()
  local_build_temp_database()

  result <- suppressMessages(add_nhs_data_migration(
    path = dir,
    tables = c("read2_ctv3", "ctv3_read2"),
    version = "test_v1"
  ))

  expect_named(result, c("read2_ctv3", "ctv3_read2"))
})
