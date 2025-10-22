# SETUP ---------------------------------------------------------------

all_lkps_maps_dummy <- build_all_lkps_maps_dummy()

ALL_LKPS_MAPS_DUMMY <- all_lkps_maps_to_db(
  all_lkps_maps_dummy,
  db_path = tempfile(fileext = ".db")
)

clinical_events_dummy <- dummy_clinical_events_tidy()

clinical_events_phecodes_dummy <- map_clinical_events_to_phecodes(
  clinical_events = clinical_events_dummy,
  all_lkps_maps = all_lkps_maps_dummy,
  min_date_only = FALSE
)

phecode_reverse_map_dummy <-
  make_phecode_reverse_map(
    clinical_events_phecodes = clinical_events_phecodes_dummy,
    all_lkps_maps = all_lkps_maps_dummy
  )

# TESTS -------------------------------------------------------------------

# `map_clinical_events_to_phecodes()` -------------------------------------

test_that("`map_clinical_events_to_phecodes()` works with `all_lkps_maps` as a database", {
  expect_true(is.data.frame(
    map_clinical_events_to_phecodes(
      clinical_events = clinical_events_dummy,
      all_lkps_maps = ALL_LKPS_MAPS_DUMMY,
      min_date_only = FALSE
    )
  ))
})

# check:

## - Read code for hypertension maps to 3 character 'I10' icd10, which is
## undivided and therefore ends with 'X' - the 'X' should be removed from icd10
## column in final result

test_that("`map_clinical_events_to_phecodes()` returns expected output with default `col_filters`", {
  expect_equal(
    clinical_events_phecodes_dummy,
    tibble::tribble(
      ~eid , ~source   , ~index , ~code   , ~date        , ~icd10 , ~phecode ,
         1 , "f40001"  , "0_0"  , NA      , "1917-10-08" , "I10"  , "401.1"  ,
         1 , "f40002"  , "0_0"  , NA      , "1955-02-11" , "E109" , "250.1"  ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "L721" , "706.2"  ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "L721" , "704"    ,
         1 , "gpc3_r3" , "3"    , "XE0Uc" , "1917-10-08" , "I10"  , "401.1"  ,
         1 , "f41271"  , "0_0"  , "4019"  , "1910-02-19" , "I10"  , "401.1"
    )
  )
})

# note that now Read 3 'XaIP9' (Sebaceous cyst) maps to male and female specific
# ICD10 codes (N508 Other specified disorders of male genital organs, and N948
# Other specified conditions associated with female genital organs and menstrual
# cycle)

# note that Read 2 "C10.." (Diabetes mellitus) now maps to all diabetes subtypes
test_that("`map_clinical_events_to_phecodes()` returns expected output with no `col_filters`", {
  result_no_filters <- map_clinical_events_to_phecodes(
    clinical_events = clinical_events_dummy,
    all_lkps_maps = all_lkps_maps_dummy,
    min_date_only = FALSE,
    col_filters = NULL
  )

  expect_equal(
    result_no_filters,
    tibble::tribble(
      ~eid , ~source   , ~index , ~code   , ~date        , ~icd10 , ~phecode ,
         1 , "f40001"  , "0_0"  , NA      , "1917-10-08" , "I10"  , "401.1"  ,
         1 , "f40002"  , "0_0"  , NA      , "1955-02-11" , "E109" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E10"  , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E100" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E101" , "250.11" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E102" , "250.12" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E103" , "250.23" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E103" , "250.7"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E103" , "250.13" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E104" , "250.14" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E104" , "250.24" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E105" , "443.7"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E106" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E107" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E108" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E109" , "250.1"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E11"  , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E110" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E111" , "250.21" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E112" , "250.22" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E113" , "250.23" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E114" , "250.24" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E115" , "443.7"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E116" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E117" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E118" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E119" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E121" , "260.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E121" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E122" , "260.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E122" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E123" , "250.23" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E125" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E125" , "260.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E127" , "260.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E127" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E128" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E129" , "249"    ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E13"  , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E131" , "250.21" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E133" , "250.7"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E134" , "250.24" ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E135" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E136" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E137" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E138" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E139" , "250.2"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E145" , "443.7"  ,
         1 , "gpc1_r2" , "1"    , "C10.." , "1965-08-08" , "E149" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E10"  , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E100" , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E101" , "250.11" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E102" , "250.12" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E103" , "250.23" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E103" , "250.7"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E103" , "250.13" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E104" , "250.14" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E104" , "250.24" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E105" , "443.7"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E106" , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E107" , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E108" , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E109" , "250.1"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E11"  , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E110" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E111" , "250.21" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E112" , "250.22" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E113" , "250.23" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E114" , "250.24" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E115" , "443.7"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E116" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E117" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E118" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E119" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E121" , "260.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E121" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E122" , "260.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E122" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E123" , "250.23" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E125" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E125" , "260.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E127" , "260.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E127" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E128" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E129" , "249"    ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E13"  , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E131" , "250.21" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E133" , "250.7"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E134" , "250.24" ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E135" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E136" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E137" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E138" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E139" , "250.2"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E145" , "443.7"  ,
         1 , "gpc1_r2" , "2"    , "C10.." , "1917-10-08" , "E149" , "250.2"  ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "L721" , "706.2"  ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "L721" , "704"    ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "H028" , "374"    ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "N508" , "608"    ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "N608" , "610.8"  ,
         1 , "gpc3_r3" , "3"    , "XaIP9" , "1917-10-08" , "N948" , "625"    ,
         1 , "gpc3_r3" , "3"    , "XE0Uc" , "1917-10-08" , "I10"  , "401.1"  ,
         1 , "f41271"  , "0_0"  , "4019"  , "1910-02-19" , "I10"  , "401.1"
    )
  )
})

# `make_phecode_reverse_map()` --------------------------------------------

test_that("`make_phecode_reverse_map()` works with `all_lkps_maps` as a database", {
  expect_true(is.data.frame(
    make_phecode_reverse_map(
      clinical_events_phecodes = clinical_events_phecodes_dummy,
      all_lkps_maps = ALL_LKPS_MAPS_DUMMY
    )
  ))
})

test_that("`make_phecode_reverse_map()` returns expected results", {
  expect_equal(
    phecode_reverse_map_dummy,
    tibble::tribble(
      ~phecode , ~phecode_description                  , ~data_coding , ~code   , ~description                                     , ~icd10_equivalent , ~icd10_description                               ,
      "401.1"  , "Essential hypertension"              , "icd10"      , "I10"   , "Essential (primary) hypertension"               , "I10"             , "Essential (primary) hypertension"               ,
      "250.1"  , "Type 1 diabetes"                     , "icd10"      , "E109"  , "Type 1 diabetes mellitus Without complications" , "E109"            , "Type 1 diabetes mellitus Without complications" ,
      "401.1"  , "Essential hypertension"              , "icd9"       , "4019"  , "ESSENTIAL HYPERTENSION NOT SPECIFIED"           , "I10"             , "Essential (primary) hypertension"               ,
      "706.2"  , "Sebaceous cyst"                      , "read3"      , "XaIP9" , "Sebaceous cyst"                                 , "L721"            , "Trichilemmal cyst"                              ,
      "704"    , "Diseases of hair and hair follicles" , "read3"      , "XaIP9" , "Sebaceous cyst"                                 , "L721"            , "Trichilemmal cyst"                              ,
      "401.1"  , "Essential hypertension"              , "read3"      , "XE0Uc" , "Essential hypertension"                         , "I10"             , "Essential (primary) hypertension"
    )
  )
})
