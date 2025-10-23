# SETUP -------------------------------------------------------------------

caliber_raw <- read_caliber_raw(dummy_caliber_dir_path())

all_lkps_maps <- build_all_lkps_maps_dummy()

caliber_ukb <- suppressWarnings(
  reformat_caliber_for_ukb(
    caliber_raw,
    all_lkps_maps = all_lkps_maps,
    overlapping_disease_categories_csv = default_overlapping_disease_categories_csv()
  )
)

caliber_ukb_no_filter <- suppressWarnings(
  reformat_caliber_for_ukb(
    caliber_raw,
    all_lkps_maps = all_lkps_maps,
    col_filters = NULL,
    overlapping_disease_categories_csv = default_overlapping_disease_categories_csv()
  )
)

# TESTS -------------------------------------------------------------------

# `read_caliber_raw()` -------------------------------------------------------

test_that("`read_caliber_raw()` output is expected format", {
  expect_equal(
    names(caliber_raw),
    c(
      "read2",
      "icd10",
      "opcs4"
    )
  )
})

# `reformat_caliber_for_ukb()` ----------------------------------------------

## Undivided 3 character ICD10 codes (e.g. A38, Scarlet fever) -------------

test_that("Mapped CALIBER codes include undivided 3 char ICD10 codes without 'X' appended", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(description == "Scarlet fever") %>%
      dplyr::pull(code),
    "A38"
  )
})

## 3 character ICD10 codes that need expanding -----------------------------

# For CALIBER, where only the parent 3 characters are shown then it implies that
# all the children codes fall under the same disease category. These need
# expanding to include all children codes. e.g. E10 (has modifier 4), M90.0 (has
# modifier 5), J45 (no modifier 4/5, but does have 4 character children))

test_that("ICD10 code E10 (with MODIFIER_4) is expanded", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        stringr::str_detect(
          code,
          "^E10"
        ) &
          code_type == "icd10"
      ) %>%
      dplyr::pull(code),
    c(
      "E10",
      "E100",
      "E101",
      "E102",
      "E103",
      "E104",
      "E105",
      "E106",
      "E107",
      "E108",
      "E109"
    )
  )
})

test_that("ICD10 code M90.0 (with MODIFIER_5) is expanded", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        stringr::str_detect(
          code,
          "^M90"
        ) &
          code_type == "icd10"
      ) %>%
      dplyr::pull(code),
    c(
      "M900",
      "M9000",
      "M9001",
      "M9002",
      "M9003",
      "M9004",
      "M9005",
      "M9006",
      "M9007",
      "M9008",
      "M9009"
    )
  )
})

test_that("ICD10 code J45 (with no MODIFIER_4/MODIFIER_5) is expanded", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        stringr::str_detect(
          code,
          "^J45"
        ) &
          code_type == "icd10"
      ) %>%
      dplyr::pull(code),
    c(
      "J45",
      "J450",
      "J451",
      "J458",
      "J459"
    )
  )
})

test_that("Full description appended for expanded 3 character ICD10 codes", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        stringr::str_detect(
          code,
          "^J45"
        ) &
          code_type == "icd10"
      ) %>%
      dplyr::pull(description),
    c(
      "Asthma",
      "Predominantly allergic asthma",
      "Nonallergic asthma",
      "Mixed asthma",
      "Asthma, unspecified"
    )
  )
})


## `filter_cols` arg works as expected ----------------------------------

test_that("`filter_cols` arg to `reformat_caliber_for_ukb()` is working", {
  expect_true(nrow(caliber_ukb_no_filter) > nrow(caliber_ukb))
})

## `default_overlapping_disease_categories_csv()` -----------------------------

test_that("Error raised if overlapping disease categories in mapped CALIBER codes", {
  expect_error(
    suppressWarnings(reformat_caliber_for_ukb(
      caliber_raw,
      all_lkps_maps = all_lkps_maps,
      overlapping_disease_categories_csv = NULL
    )),
    regexp = "The following 2 diseases include categories with non-distinct codes: Diabetic neurological complications, End stage renal disease"
  )
})


## ICD9 mappings ----------------------

test_that("ICD9 has been mapped to", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        category == "Diagnosis of Asthma" &
          code_type == "icd9"
      ) %>%
      dplyr::pull(code) %>%
      sort(),
    c(
      "4930",
      "4931",
      "4939"
    )
  )
})

## Read 3 mappings ----------------------

test_that("Read 3 has been mapped to", {
  expect_equal(
    caliber_ukb %>%
      dplyr::filter(
        category == "Type I diabetes mellitus (3)" &
          code_type == "read3"
      ) %>%
      dplyr::pull(code),
    "X40J4"
  )
})

# `get_caliber_categories_mapping()` --------------------------------------

test_that("`get_caliber_categories_mapping()` returns a tibble", {
  expect_true(tibble::is_tibble(get_caliber_categories_mapping()))
})
