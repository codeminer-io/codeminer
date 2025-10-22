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

# `reformat_read_v2_icd10()` ------------------------------------------------

test_that("`reformat_read_v2_icd10()` works as expected", {
  # Note that `icd10_code` 'C836' should be removed from the result. Also,
  # "A63-A64" should have 'X' appended to 'A64' (i.e. 'A64X').

  df <- tibble::tribble(
    ~read_code , ~icd10_code        , ~icd10_code_def ,
    "C10E."    , "E100-E109"        , "2"             ,
    "A13.."    , "A170D-A179D"      , "2"             ,
    "A00.."    , "A00"              , "1"             ,
    "A0221"    , "A022D G01XA"      , "7"             ,
    "A13y."    , "A178D"            , "8"             ,
    "A34.."    , "J020,A38X"        , "3"             ,
    "A365."    , "A390D G01XA+A392" , "15"            ,
    "A3805"    , "A408+U830"        , "15"            ,
    "Cyu8Q"    , "E90XA"            , "5"             ,
    "F0073"    , "A022D G01XA"      , "7"             ,
    "A9z.."    , "A63-A64"          , "2"             ,
    "B6278"    , "C836"             , "1"
  )

  expected_result <- tibble::tribble(
    ~read_code , ~icd10_code , ~icd10_code_def , ~icd10_dagger_asterisk ,
    "C10E."    , "E100"      , "2"             , NA                     ,
    "C10E."    , "E101"      , "2"             , NA                     ,
    "C10E."    , "E102"      , "2"             , NA                     ,
    "C10E."    , "E103"      , "2"             , NA                     ,
    "C10E."    , "E104"      , "2"             , NA                     ,
    "C10E."    , "E105"      , "2"             , NA                     ,
    "C10E."    , "E106"      , "2"             , NA                     ,
    "C10E."    , "E107"      , "2"             , NA                     ,
    "C10E."    , "E108"      , "2"             , NA                     ,
    "C10E."    , "E109"      , "2"             , NA                     ,
    "A13.."    , "A170"      , "2"             , "D"                    ,
    "A13.."    , "A171"      , "2"             , "D"                    ,
    "A13.."    , "A178"      , "2"             , "D"                    ,
    "A13.."    , "A179"      , "2"             , "D"                    ,
    "A00.."    , "A00"       , "1"             , NA                     ,
    "A0221"    , "A022"      , "7"             , "D"                    ,
    "A0221"    , "G01X"      , "7"             , "A"                    ,
    "A13y."    , "A178"      , "8"             , "D"                    ,
    "A34.."    , "J020"      , "3"             , NA                     ,
    "A34.."    , "A38X"      , "3"             , NA                     ,
    "A365."    , "A390"      , "15"            , "D"                    ,
    "A365."    , "G01X"      , "15"            , "A"                    ,
    "A365."    , "A392"      , "15"            , NA                     ,
    "A3805"    , "A408"      , "15"            , NA                     ,
    "A3805"    , "U830"      , "15"            , NA                     ,
    "Cyu8Q"    , "E90X"      , "5"             , "A"                    ,
    "F0073"    , "A022"      , "7"             , "D"                    ,
    "F0073"    , "G01X"      , "7"             , "A"                    ,
    "A9z.."    , "A63"       , "2"             , NA                     ,
    "A9z.."    , "A630"      , "2"             , NA                     ,
    "A9z.."    , "A638"      , "2"             , NA                     ,
    "A9z.."    , "A64X"      , "2"             , NA
  )

  expect_equal(
    reformat_read_v2_icd10(
      read_v2_icd10 = df,
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    expected_result
  )
})

# `reformat_icd9_icd10()` -------------------------------------------------

test_that("`reformat_icd9_icd10()` converts 'UNDEF' codes to `NA`", {
  df <- tibble::tribble(
    ~ICD9   , ~DESCRIPTION_ICD9 , ~ICD10  , ~DESCRIPTION_ICD10 ,
    "UNDEF" , NA                , "E10"   , "DIABETES"         ,
    "1234"  , "DIABETES"        , "UNDEF" , NA
  )

  df_WRONG <- tibble::tribble(
    ~ICD9   , ~DESCRIPTION_ICD9 , ~ICD10  , ~DESCRIPTION_ICD10 ,
    "UNDEF" , NA                , "E10"   , NA                 ,
    "1234"  , "DIABETES"        , "UNDEF" , NA
  )

  expect_equal(
    reformat_icd9_icd10(df),
    tibble::tribble(
      ~ICD9  , ~DESCRIPTION_ICD9 , ~ICD10 , ~DESCRIPTION_ICD10 ,
      NA     , NA                , "E10"  , "DIABETES"         ,
      "1234" , "DIABETES"        , NA     , NA
    )
  )

  expect_error(reformat_icd9_icd10(df_WRONG), regexp = "are not true")
})

# `reformat_icd10_phecode_map_1_2()` --------------------------------------

test_that("`reformat_icd10_phecode_map_1_2()` returns expected results", {
  icd10_phecode_map_dummy_reformatted <-
    reformat_icd10_phecode_map_1_2(
      icd10_phecode_map_dummy,
      all_lkps_maps = all_lkps_maps
    )

  expect_equal(
    names(icd10_phecode_map_dummy_reformatted),
    c(
      "ICD10_CODE",
      "ALT_CODE",
      "PHECODE",
      "Exl. Phecodes",
      "Excl. Phenotypes"
    )
  )
})

test_that("`reformat_icd10_phecode_map_1_2()` returns undivided 3 character ICD10 code I10 as I10X", {
  icd10_phecode_map_dummy_reformatted <-
    reformat_icd10_phecode_map_1_2(
      icd10_phecode_map_dummy,
      all_lkps_maps = all_lkps_maps
    )

  expect_equal(
    icd10_phecode_map_dummy_reformatted %>%
      dplyr::filter(ICD10_CODE == "I10") %>%
      dplyr::pull(ALT_CODE),
    "I10X"
  )
})
