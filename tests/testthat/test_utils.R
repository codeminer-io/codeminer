# `rm_footer_rows_all_lkps_maps_df()` -------------------------------------

test_that("`rm_footer_rows_all_lkps_maps_df()` raises error if more than 3 rows are removed", {
  df <- tibble::tibble(
    x = c(1, 2, 3, 4, 5, 6, NA, 8, 9, 10),
    y = c(1, 2, 3, 4, 5, 6, 7, NA, 9, 10)
  )

  expect_error(
    rm_footer_rows_all_lkps_maps_df(df, footer_metadata_col_idx = 1),
    regexp = "Attempted to remove all rows after row number 7."
  )

  expect_equal(
    nrow(rm_footer_rows_all_lkps_maps_df(df, footer_metadata_col_idx = 2)),
    7
  )
})


# `update_code_selection()` -----------------------------------------------

test_that("`update_code_selection()` works as expected", {
  result <-
    update_code_selection(
      current_selection = ukbwranglr::example_clinical_codes() %>%
        dplyr::mutate(category = NA),
      previous_codelist = ukbwranglr::example_clinical_codes()[1:3, ]
    )

  expect_equal(
    result,
    tibble::tribble(
      ~disease   , ~description                              , ~category              , ~code_type      , ~code   , ~author , ~selected ,
      "Diabetes" , "diabetes"                                , "Diabetes unspecified" , "data_coding_6" , "1220"  , "ukbwr" , "Yes"     ,
      "Diabetes" , "gestational diabetes"                    , "Gestational diabetes" , "data_coding_6" , "1221"  , "ukbwr" , "Yes"     ,
      "Diabetes" , "type 1 diabetes"                         , "Type 1 DM"            , "data_coding_6" , "1222"  , "ukbwr" , "Yes"     ,
      "Diabetes" , "type 2 diabetes"                         , NA                     , "data_coding_6" , "1223"  , "ukbwr" , ""        ,
      "Diabetes" , "Type 1 diabetes mellitus"                , NA                     , "icd10"         , "E10"   , "ukbwr" , ""        ,
      "Diabetes" , "Type 2 diabetes mellitus"                , NA                     , "icd10"         , "E11"   , "ukbwr" , ""        ,
      "Diabetes" , "Insulin dependent diabetes mellitus"     , NA                     , "read2"         , "C108." , "ukbwr" , ""        ,
      "Diabetes" , "Non-insulin dependent diabetes mellitus" , NA                     , "read2"         , "C109." , "ukbwr" , ""
    )
  )
})
