# SETUP ---------------------------------------------------------------

ukb_codings <- read_ukb_codings_dummy()

all_lkps_maps_raw <- read_all_lkps_maps_dummy()
all_lkps_maps <- build_all_lkps_maps_dummy()

all_lkps_maps_db <- all_lkps_maps_to_db(
  all_lkps_maps = all_lkps_maps,
  db_path = tempfile(fileext = ".db")
)

# TESTS -------------------------------------------------------------------

# `all_lkps_maps` -----------------------------------------------------

test_that("`all_lkps_maps` table 'icd10_lkp' has no rows with values in both the 'MODIFER-4' and 'MODIFER-5' columns", {
  # relevant to `CODES()` when `standardise_output` is `TRUE`. Some
  # ICD-10 codes have a description modifier in one of these 2 columns (e.g.
  # `E10` for T1DM (MODIFER-4) and `S27` for traumatic pneumothorax
  # (MODIFER-5)). `CODES()` creates a description column by pasting
  # together the 'DESCRIPTION' column with *only* one of these. Therefore only
  # one of these columns should contain a description.
  expect_true(
    sum(
      !is.na(all_lkps_maps$icd10_lkp$MODIFIER_4) &
        !is.na(all_lkps_maps$icd10_lkp$MODIFIER_5)
    ) ==
      0
  )
})

# `codes_starting_with()` -----------------------------------------------------

test_that("`codes_starting_with()` returns the expected nuber of results, escaping '.'", {
  # return - codes only

  # escaping '.'
  expect_equal(
    codes_starting_with(
      codes = c("C10E."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE,
      escape_dot = TRUE
    ),
    expected = "C10E."
  )

  # no '.'
  expect_equal(
    length(codes_starting_with(
      codes = c("C10"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE,
      escape_dot = TRUE
    )),
    expected = 3
  )

  # return codes and descriptions as a data frame
  expect_equal(
    nrow(
      codes_starting_with(
        codes = c("C10E"),
        code_type = "read2",
        all_lkps_maps = all_lkps_maps,
        codes_only = FALSE,
        preferred_description_only = FALSE,
        escape_dot = TRUE
      )
    ),
    expected = 3
  )

  expect_equal(
    nrow(
      codes_starting_with(
        codes = c("C10E"),
        code_type = "read2",
        all_lkps_maps = all_lkps_maps,
        codes_only = FALSE,
        preferred_description_only = TRUE,
        escape_dot = TRUE
      )
    ),
    expected = 1
  )
})


# `CHILDREN()` -----------------------------------------------------

test_that("`CHILDREN()` returns error for unrecognised codes", {
  expect_error(
    CHILDREN(
      codes = c("C10"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    regexp = "not found for 'read2' in table 'read_v2_lkp"
  )
})

test_that("`CHILDREN()` works as expected for read2", {
  expect_equal(
    CHILDREN(
      codes = c("C10.."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    c("C10..", "C108.", "C10E.")
  )
})

test_that("`CHILDREN()` raises error for unsupported code types e.g. read3", {
  expect_error(
    CHILDREN(
      codes = "C10..",
      code_type = "read3",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    ),
    "Currently codeminer is unable to retrieve child codes for read3"
  )
})


# `handle_unrecognised_codes()` ------------------------------------------

test_that("`handle_unrecognised_codes()` produces an error/warning message appropriately", {
  # should raise an error
  expect_error(
    handle_unrecognised_codes(
      unrecognised_codes = "error",
      missing_codes = "foo",
      table_name = "table",
      code_type = "imaginary_coding_system"
    ),
    regexp = "The following 1 codes were not found for 'imaginary_coding_system' in table 'table'"
  )

  # should raise a warning
  expect_warning(
    handle_unrecognised_codes(
      unrecognised_codes = "warning",
      missing_codes = "foo",
      table_name = "table",
      code_type = "imaginary_coding_system"
    ),
    regexp = "The following 1 codes were not found for 'imaginary_coding_system' in table 'table': 'foo'"
  )

  # should return NULL
  expect_null(
    handle_unrecognised_codes(
      unrecognised_codes = "error",
      table_name = "table",
      missing_codes = character(),
      code_type = "imaginary_coding_system"
    )
  )
})

# `reformat_icd10_codes()` ------------------------------------------------
test_that("`reformat_icd10_codes()` returns the expected values for ICD10_CODE to ALT_CODE", {
  expect_equal(
    # warning raised because "I714" not present in ICD10_CODE col of icd10_lkp
    # table
    suppressWarnings(reformat_icd10_codes(
      icd10_codes = c(
        "D75.1",
        "I11", # will be the same for ICD10_CODE and ALT_CODE
        "I11.0",
        "I792", # not in ICD10_CODE col
        "M90.0"
      ), # multiple associated ALT_CODEs
      all_lkps_maps = all_lkps_maps,
      input_icd10_format = "ICD10_CODE",
      output_icd10_format = "ALT_CODE",
      unrecognised_codes = "warning"
    )),
    c(
      "D751",
      "I11",
      "I110",
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

test_that("`reformat_icd10_codes()` returns the expected values for ALT_CODE to ICD10_CODE", {
  expect_equal(
    reformat_icd10_codes(
      icd10_codes = c(
        "D751",
        "I11", # will be the same for ICD10_CODE and ALT_CODE
        "I110",
        "I792", # not in ICD10_CODE col
        "M900", # multiple associated ALT_CODEs - all map to "M00.0"
        "M9001",
        "M9002"
      ),
      all_lkps_maps = all_lkps_maps,
      input_icd10_format = "ALT_CODE",
      output_icd10_format = "ICD10_CODE"
    ),
    c("D75.1", "I11", "I11.0", "I79.2", "M90.0")
  )
})

test_that("`reformat_icd10_codes()` returns the expected values for ICD10_CODE to ALT_CODE for a 3 character code with no children", {
  expect_equal(
    reformat_icd10_codes(
      icd10_codes = c("A38"),
      all_lkps_maps = all_lkps_maps,
      input_icd10_format = "ICD10_CODE",
      output_icd10_format = "ALT_CODE"
    ),
    "A38X"
  )
})

test_that("`reformat_icd10_codes()` strips 'X' from undivided 3 character codes in `ALT_CODE` format (when `strip_x` is `TRUE`)", {
  expect_equal(
    reformat_icd10_codes(
      icd10_codes = c("A38"),
      all_lkps_maps = all_lkps_maps,
      input_icd10_format = "ICD10_CODE",
      output_icd10_format = "ALT_CODE",
      strip_x = TRUE
    ),
    "A38"
  )
})

# `filter_cols` -----------------------------------------------------------

test_that("`filter_cols` filters columns as expected (or returns `df` unchanged, if appropriate)", {
  # change `Species` column to class 'character'
  iris_chr <- iris %>%
    dplyr::mutate(Species = as.character(Species))

  # check returns expected number of rows for single/multiple column/value combinations
  expect_equal(
    nrow(
      filter_cols(
        df = iris_chr,
        df_name = "iris",
        col_filters = list(iris = list(Species = c("setosa")))
      )
    ),
    50
  )

  expect_equal(
    nrow(
      filter_cols(
        df = iris_chr,
        df_name = "iris",
        col_filters = list(iris = list(Species = c("setosa", "virginica")))
      )
    ),
    100
  )

  expect_equal(
    nrow(filter_cols(
      df = iris_chr,
      df_name = "iris",
      col_filters = list(
        iris = list(
          Species = c("setosa", "virginica"),
          Petal.Width = c(0.5, 0.6)
        )
      )
    )),
    2
  )

  # returns df unchanged if `df_name` not in `names(col_filters)`
  expect_equal(
    nrow(filter_cols(
      df = iris_chr,
      df_name = "iris",
      col_filters = list(
        FOO = list(
          Species = c("setosa", "virginica"),
          Petal.Width = c(0.5, 0.6)
        )
      )
    )),
    150
  )
})

test_that("`filter_cols` raises error if `col_filters` includes unrecognised/missing column names", {
  # unrecognised column name
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(
        iris = list(
          Species2 = c("setosa"),
          Foo = c("setosa")
        )
      )
    ),
    "are not present in"
  )

  # unnamed item in `col_filters`
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(
        iris = list(
          Species2 = c("setosa"),
          c("setosa")
        )
      )
    ),
    "must be named"
  )
})

test_that("`filter_cols` raises error if `col_filters` contains items that are not vectors", {
  # unrecognised column name
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species = iris))
    ),
    "Each item in `col_filters` must be a vector"
  )
})

test_that("`filter_cols` raises error if class of df column to be filtered does not match class of supplied filter values", {
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species = c("setosa")))
    ),
    "classes do not match"
  )
})

# `rm_footer_rows_all_lkps_maps_df()` ----------------------------------------

test_that("`rm_footer_rows_all_lkps_maps_df()` removes footer rows as expected", {
  df <- data.frame(
    col1 = c("A", NA, "C", "D", NA, "Footer text"),
    col2 = c(letters[1:4], NA, NA)
  )

  expect_equal(
    rm_footer_rows_all_lkps_maps_df(df),
    data.frame(
      col1 = c("A", NA, "C", "D"),
      col2 = c(letters[1:4])
    )
  )
})


# `get_icd10_code_range()` ------------------------------------------------

test_that("`get_icd10_code_range()` returns expected codes", {
  # 4 character ICD10 code range
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "E100",
      end_icd10_code = "E109",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
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

  # 'D' appended - expect error
  expect_error(
    get_icd10_code_range(
      start_icd10_code = "A170D",
      end_icd10_code = "A179D",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    regexp = "were not found for 'icd10' in table 'icd10_lkp'"
  )

  # 3 character ICD10 code range
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "I11",
      end_icd10_code = "I12",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
      "I11",
      "I110",
      "I119",
      "I12",
      "I120",
      "I129"
    )
  )

  # 3 character ICD10 code range, including final 'X' character
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "I10X",
      end_icd10_code = "I11",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
      "I10X",
      "I11",
      "I110",
      "I119"
    )
  )
})

# `rm_or_extract_appended_icd10_dxa()` -----------------------------

test_that("`rm_or_extract_appended_icd10_dxa()` works", {
  icd10_codes <- c(
    "A00",
    "A408",
    "A390D",
    "A38X",
    "G01XA"
  )

  # remove
  rm_expected_result <- c(
    "A00",
    "A408",
    "A390",
    "A38X",
    "G01X"
  )

  rm_expected_result_x_rm <- c(
    "A00",
    "A408",
    "A390",
    "A38",
    "G01"
  )

  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes),
    rm_expected_result
  )

  # remove twice - should return the same result
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes) %>%
      rm_or_extract_appended_icd10_dxa(),
    rm_expected_result
  )

  # remove 'X'
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes, keep_x = FALSE),
    rm_expected_result_x_rm
  )

  # extract
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes, rm_extract = "extract"),
    c(
      NA,
      NA,
      "D",
      NA,
      "A"
    )
  )

  # extract 'X'
  expect_equal(
    rm_or_extract_appended_icd10_dxa(
      icd10_codes,
      keep_x = FALSE,
      rm_extract = "extract"
    ),
    c(
      NA,
      NA,
      "D",
      "X",
      "XA"
    )
  )
})
