# SETUP ---------------------------------------------------------------

ukb_codings <- read_ukb_codings_dummy()

all_lkps_maps_raw <- read_all_lkps_maps_dummy()
all_lkps_maps <- build_all_lkps_maps_dummy()

all_lkps_maps_db <- suppressMessages(all_lkps_maps_to_db(
  all_lkps_maps = all_lkps_maps,
  db_path = tempfile(fileext = ".db")
))

icd10_phecode_map_dummy <- read_icd10_phecode_map_dummy()

# TESTS -------------------------------------------------------------------

# `all_lkps_maps_to_db()` -------------------------------------------------

test_that("`all_lkps_maps_to_db()` raises error/warning if attempting to overwrite existing db", {
  expect_error(
    suppressWarnings(all_lkps_maps_to_db(
      all_lkps_maps = all_lkps_maps,
      db_path = all_lkps_maps_db,
      overwrite = FALSE
    )),
    "The following tables are already present in the database"
  )

  expect_warning(
    suppressMessages(all_lkps_maps_to_db(
      all_lkps_maps = all_lkps_maps,
      db_path = all_lkps_maps_db,
      overwrite = TRUE
    )),
    "The following tables are already present in the database"
  )
})
