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
