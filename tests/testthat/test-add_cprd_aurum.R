test_that("add_cprd_aurum() errors on nonexistent path", {
  expect_error(
    add_cprd_aurum(path = "/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("add_cprd_aurum() adds all tables and returns them invisibly", {
  dir <- create_dummy_cprd_aurum_dir()
  local_build_temp_database()

  expect_invisible(result <- suppressMessages(add_cprd_aurum(dir)))
  expect_named(result, cprd_aurum_tables())

  meta <- get_codeminer_metadata()
  expect_setequal(
    meta$lookup$lookup_table_name,
    c("CPRD Aurum medcode_209901", "CPRD Aurum prodcode_209901")
  )
  expect_setequal(
    meta$mapping$mapping_table_name,
    c(
      "CPRD Aurum medcode_SNOMED CT_209901",
      "CPRD Aurum medcode_Read v2_209901",
      "CPRD Aurum prodcode_SNOMED CT_209901"
    )
  )
})

test_that("add_cprd_aurum() skips tables that already exist", {
  dir <- create_dummy_cprd_aurum_dir()
  local_build_temp_database()

  suppressMessages(add_cprd_aurum(dir, tables = "cprd_aurum_medcode_lkp"))

  # Only the missing tables are read and added
  result <- suppressMessages(add_cprd_aurum(dir))
  expect_named(result, setdiff(cprd_aurum_tables(), "cprd_aurum_medcode_lkp"))

  # Everything exists now, so a re-run does nothing
  expect_message(
    result <- add_cprd_aurum(dir),
    "already exist"
  )
  expect_null(result)
})

test_that("default prodcode queries keep rows with a blank route or formulation", {
  # Column filters match exact values and drop NA. Blank routes and
  # formulations are stored as "" and
  # included in the default filter values, so a default query returns every
  # prodcode.
  dir <- create_dummy_cprd_aurum_dir()
  local_build_temp_database()
  suppressMessages(add_cprd_aurum(dir))

  all_prodcodes <- get_lookup_table("CPRD Aurum prodcode") |>
    dplyr::collect()
  expect_setequal(all_prodcodes$code, c("PROD001", "PROD002", "PROD003"))

  unfiltered <- get_lookup_table("CPRD Aurum prodcode", col_filters = NULL) |>
    dplyr::collect()
  expect_equal(nrow(all_prodcodes), nrow(unfiltered))

  # An explicit filter narrows to the exact value only
  oral_only <- get_lookup_table(
    "CPRD Aurum prodcode",
    col_filters = list(
      lookup = list(
        "CPRD Aurum prodcode" = list(routeofadministration = "Dummy route 1")
      )
    )
  ) |>
    dplyr::collect()
  expect_equal(oral_only$code, "PROD001")

  # The empty string selects prodcodes with no route recorded
  no_route <- get_lookup_table(
    "CPRD Aurum prodcode",
    col_filters = list(
      lookup = list("CPRD Aurum prodcode" = list(routeofadministration = ""))
    )
  ) |>
    dplyr::collect()
  expect_equal(no_route$code, "PROD003")
})

test_that("CPRD Aurum codes map to SNOMED CT and Read v2", {
  dir <- create_dummy_cprd_aurum_dir()
  local_build_temp_database()
  suppressMessages(add_cprd_aurum(dir))

  medcode_maps <- get_mapping_table("CPRD Aurum medcode", "Read v2") |>
    dplyr::collect()
  expect_setequal(medcode_maps$from, c("MED001", "MED002"))
  expect_setequal(medcode_maps$to, "DUM01")
})
