## Set up dummy database
create_dummy_database()

test_that("MAP() returns the expected data format", {
  test_codes <- c("C10..", "XE0Uc", "C10..", "C10..", "XE0Uc")
  test_from <- "read3"
  test_to <- "icd10"

  result <- MAP(test_codes, from = test_from, to = test_to, version = "v0")

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("code", "description", "code_type"))
  expect_true(nrow(result) >= length(test_codes))
  expect_identical(unique(result$code_type), test_to)
})

test_that("MAP fails for wrong argument types", {
  expect_error(
    MAP("all", from = c("icd10", "icd11", "icd12"), to = "read3"),
    "`from` must have length 1"
  )
  expect_error(
    MAP("all", from = "read3", to = c("icd10", "icd11", "icd12")),
    "`to` must have length 1"
  )
})

test_that("MAP fails for missing mapping table", {
  expected_msg <- "No mapping table found"
  expect_error(
    MAP("all", from = "idontexist", to = "icd10"),
    expected_msg
  )
  expect_error(
    MAP("all", from = "read3", to = "idontexist"),
    expected_msg
  )
  expect_error(
    MAP("all", from = "read3", to = "icd10", version = "nope"),
    expected_msg
  )
})


# `MAP()` -----------------------------------------------------------

test_that("`MAP()` raises warning if any of the supplied codes are not present in the coding system being mapped from", {
  expect_warning(
    MAP(
      codes = c("C10E.", "foo", "bar"),
      from = "read2",
      to = "read3",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "warning"
    ),
    regexp = "The following 2 codes were not found for 'read2' in table 'read_v2_read_ctv3': 'foo', 'bar'",
    fixed = TRUE
  )
})

test_that("`MAP()` returns the expected codes", {
  # codes only
  expect_equal(
    MAP(
      codes = c("C10E."),
      from = "read2",
      to = "read3",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    "X40J4"
  )

  # codes and ALL descriptions
  expect_equal(
    nrow(MAP(
      codes = c("C10E."),
      from = "read2",
      to = "read3",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = FALSE,
      standardise_output = FALSE
    )),
    3
  )
})

test_that("`MAP` returns the expected output when `standardise_output` is `TRUE`", {
  expect_equal(
    MAP(
      codes = c("C10E.", "C108."),
      from = "read2",
      to = "read3",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    "X40J4"
  )
})

# Tests default settings for `element_num` and `block_num` (should equal '0'
# only) in read3-to-icd10 mapping table. Mapping the Read 3 code 'XE0e0'
# ('Infection of urinary tract') should by default only map to ICD10 'N390'
# ('Urinary tract infection, site not specified'). Including non-0 values for
# `element_num`/`block_num` means it will also map to ICD10 codes for
# Tuberculosis.
test_that("`MAP` returns the expected output for Read 3 ('XE0e0') to ICD10 example with default `col_filters`", {
  expect_equal(
    MAP(
      codes = "XE0e0",
      from = "read3",
      to = "icd10",
      col_filters = default_col_filters(),
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    "N390"
  )
})

test_that("`MAP` returns the expected output for Read 3 ('XE0e0') to ICD10 example with default `col_filters`", {
  expect_equal(
    MAP(
      codes = "XE0e0",
      from = "read3",
      to = "icd10",
      col_filters = NULL,
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    ),
    tibble::tribble(
      ~code  , ~description                                                                                     , ~code_type ,
      "A181" , "Tuberculosis of genitourinary system"                                                           , "icd10"    ,
      "N291" , "Other disorders of kidney and ureter in infectious and parasitic diseases classified elsewhere" , "icd10"    ,
      "N330" , "Tuberculous cystitis"                                                                           , "icd10"    ,
      "N390" , "Urinary tract infection, site not specified"                                                    , "icd10"    ,
      "O234" , "Unspecified infection of urinary tract in pregnancy"                                            , "icd10"    ,
      "O862" , "Urinary tract infection following delivery"                                                     , "icd10"    ,
      "P001" , "Fetus and newborn affected by maternal renal and urinary tract diseases"                        , "icd10"    ,
      "P393" , "Neonatal urinary tract infection"                                                               , "icd10"
    )
  )
})

# icd10 to icd9 mapping
test_that("`MAP()` works as expected for mapping icd10 to icd9 codes", {
  expect_equal(
    suppressWarnings(
      MAP(
        codes = "D751",
        from = "icd10",
        to = "icd9",
        all_lkps_maps = all_lkps_maps,
        unrecognised_codes = "error",
        codes_only = FALSE,
        preferred_description_only = TRUE,
        standardise_output = TRUE,
        reverse_mapping = "warning"
      )$code
    ),
    "2890"
  )
})

test_that("`MAP()` works when mapping icd9 to icd10", {
  expect_equal(
    MAP(
      codes = "0020",
      from = "icd9",
      to = "icd10",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    expected = "A010"
  )
})

# `MAP()` with all_lkps_maps_db -------------------------------------

test_that("`MAP()` works when mapping icd9 to icd10", {
  expect_equal(
    MAP(
      codes = "0020",
      from = "icd9",
      to = "icd10",
      all_lkps_maps = all_lkps_maps_db,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    expected = "A010"
  )
})
