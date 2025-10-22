# Dummy data path functions -----------------------------------------------

test_that("Functions that return a path to dummy data work", {
  dummy_file_paths_class <- list(
    dummy_all_lkps_maps_path(),
    dummy_ukb_codings_path(),
    dummy_phecode_lkp_path(),
    dummy_icd10_phecode_map_path(),
    dummy_caliber_dir_path()
  ) %>%
    purrr::map_chr(class) %>%
    unique()

  expect_equal(
    dummy_file_paths_class,
    "character"
  )
})


# `dummy_clinical_events_tidy()` -------------------------

test_that("`dummy_clinical_events_tidy()` returns a tibble", {
  expect_true(tibble::is_tibble(dummy_clinical_events_tidy()))
})

# `read_ukb_codings_dummy()` ----------------------------------------------

test_that("`read_ukb_codings_dummy()` returns a tibble", {
  expect_true(tibble::is_tibble(read_ukb_codings_dummy()))
})

# `read_phecode_lkp_dummy()` ----------------------------------------------

test_that("`read_phecode_lkp_dummy()` returns a tibble", {
  expect_true(tibble::is_tibble(read_phecode_lkp_dummy()))
})

# `read_icd10_phecode_map_dummy()` ----------------------------------------------

test_that("`read_icd10_phecode_map_dummy()` returns a tibble", {
  expect_true(tibble::is_tibble(read_icd10_phecode_map_dummy()))
})
