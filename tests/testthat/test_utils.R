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

# `strip_icd10_x_placeholder()` -------------------------------------------

test_that("`strip_icd10_x_placeholder()` strips X from 3-char categories", {
  expect_equal(strip_icd10_x_placeholder("J46X"), "J46")
  expect_equal(strip_icd10_x_placeholder("R69X"), "R69")
})

test_that("`strip_icd10_x_placeholder()` leaves non-placeholder codes unchanged", {
  # Already-stripped 3-char categories
  expect_equal(strip_icd10_x_placeholder("J46"), "J46")
  # 4-char codes with a digit (not X) at position 4
  expect_equal(strip_icd10_x_placeholder("E113"), "E113")
  # 5+ char codes ending in X — strict regex requires exact 4 chars
  expect_equal(strip_icd10_x_placeholder("J460X"), "J460X")
  # First chars must be letter + 2 digits
  expect_equal(strip_icd10_x_placeholder("XY12"), "XY12")
})

test_that("`strip_icd10_x_placeholder()` is vectorised", {
  expect_equal(
    strip_icd10_x_placeholder(c("J46X", "E113", "R69X", "J450")),
    c("J46", "E113", "R69", "J450")
  )
})
