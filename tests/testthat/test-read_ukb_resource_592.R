# SETUP ---------------------------------------------------------------

ukb592 <- suppressMessages(read_ukb_resource_592(
  path = dummy_ukb_resource_592_path()
))

# Mock UKB 592 result for testing extension functions
mock_ukb592_result <- function() {
  ukb592
}

# TESTS -------------------------------------------------------------------

# `process_icd9_icd10()` --------------------------------------------------

test_that("`process_icd9_icd10()` converts 'UNDEF' codes to `NA`", {
  # fmt: skip
  .df <- tibble::tribble(
    ~ICD9   , ~DESCRIPTION_ICD9 , ~ICD10  , ~DESCRIPTION_ICD10 ,
    "UNDEF" , NA                , "E10"   , "DIABETES"         ,
    "1234"  , "DIABETES"        , "UNDEF" , NA
  )

  # fmt: skip
  expected <- tibble::tribble(
    ~ICD9  , ~DESCRIPTION_ICD9 , ~ICD10 , ~DESCRIPTION_ICD10 ,
    NA     , NA                , "E10"  , "DIABETES"         ,
    "1234" , "DIABETES"        , NA     , NA
  )

  result <- process_icd9_icd10(.df, "UKB v4", "test_source")

  expect_equal(
    result$mapping$table,
    expected
  )
})

# `extend_read_v2_icd10_from_ukb592()` ------------------------------------

test_that("`extend_read_v2_icd10_from_ukb592()` works as expected", {
  # Note that `icd10_code` 'C836' should be removed from the result. Also,
  # "A63-A64" should have 'X' appended to 'A64' (i.e. 'A64X').

  # fmt: skip
  .df <- tibble::tribble(
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
    "A9z.."    , "A63-A64"          , "2"
  )

  # fmt: skip
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
    "A0221"    , "G01"      , "7"             , "A"                    ,
    "A13y."    , "A178"      , "8"             , "D"                    ,
    "A34.."    , "J020"      , "3"             , NA                     ,
    "A34.."    , "A38"      , "3"             , NA                     ,
    "A365."    , "A390"      , "15"            , "D"                    ,
    "A365."    , "G01"      , "15"            , "A"                    ,
    "A365."    , "A392"      , "15"            , NA                     ,
    "A3805"    , "A408"      , "15"            , NA                     ,
    "A3805"    , "U830"      , "15"            , NA                     ,
    "Cyu8Q"    , "E90"      , "5"             , "A"                    ,
    "F0073"    , "A022"      , "7"             , "D"                    ,
    "F0073"    , "G01"      , "7"             , "A"                    ,
    "A9z.."    , "A63"       , "2"             , NA                     ,
    "A9z.."    , "A630"      , "2"             , NA                     ,
    "A9z.."    , "A638"      , "2"             , NA                     ,
    "A9z.."    , "A64"      , "2"             , NA
  )

  # Create mock result object
  mock_result <- mock_ukb592_result()
  mock_result$read_v2_icd10 <- list(
    mapping = list(
      table = .df,
      metadata = list(
        from_code_type = "Read v2",
        to_code_type = "ICD-10"
      )
    )
  )

  result <- extend_read_v2_icd10_from_ukb592(mock_result)

  expect_equal(
    result$mapping$table,
    expected_result
  )

  # Check metadata is preserved
  expect_equal(
    result$mapping$metadata$from_code_type,
    "Read v2"
  )

  expect_equal(
    result$mapping$metadata$to_code_type,
    "ICD-10"
  )
})

# `expand_icd10_code_range()` ---------------------------------------------

test_that("`expand_icd10_code_range()` returns expected codes", {
  # 4 character ICD10 code range
  expect_equal(
    expand_icd10_code_range(
      start_icd10_code = "E100",
      end_icd10_code = "E109",
      icd10_lkp = ukb592$icd10_lkp$lookup$table
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
    expand_icd10_code_range(
      start_icd10_code = "A170D",
      end_icd10_code = "A179D",
      icd10_lkp = ukb592$icd10_lkp$lookup$table
    )
  )

  # 3 character ICD10 code range
  expect_equal(
    expand_icd10_code_range(
      start_icd10_code = "I11",
      end_icd10_code = "I12",
      icd10_lkp = ukb592$icd10_lkp$lookup$table
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

  # 3 character ICD10 code range, including an undivided ICD-10 code 'I10'.
  # 'I10' appears as 'I10X' in UKB resource 592, where 'X' is appended to codes
  # with no child codes. However, EHR use 'I10'.
  expect_equal(
    expand_icd10_code_range(
      start_icd10_code = "I10",
      end_icd10_code = "I11",
      icd10_lkp = ukb592$icd10_lkp$lookup$table
    ),
    c(
      "I10",
      "I11",
      "I110",
      "I119"
    )
  )
})

# `icd10_dxa_pattern()` ---------------------------------------------------

test_that("`icd10_dxa_pattern()` matches D/X/A characters at end of string", {
  pattern <- icd10_dxa_pattern()

  test_codes <- c("A010D", "I398A", "A89X", "E109", "A010DX", "G01XA")

  extracted <- stringr::str_extract(test_codes, pattern)

  expect_equal(
    extracted,
    c("D", "A", "X", "", "DX", "XA")
  )

  # Test removal
  cleaned <- stringr::str_remove(test_codes, pattern)
  expect_equal(
    cleaned,
    c("A010", "I398", "A89", "E109", "A010", "G01")
  )
})

# `build_prefix_hierarchy_len()` ------------------------------------------

test_that("`build_prefix_hierarchy_len()` creates correct hierarchies", {
  # Test with BNF codes
  bnf_codes <- c("01", "0101", "010101", "02", "0201")

  result <- build_prefix_hierarchy_len(bnf_codes)

  expect_equal(nrow(result), 3)
  expect_true(all(c("from", "to", "type") %in% names(result)))
  expect_true(all(result$type == "is a"))

  # Check parent-child relationships
  expect_equal(
    result,
    # fmt: skip
    # nolint start
    tibble::tribble(
      ~from,    ~to,  ~type,
      "0101",   "01", "is a",
      "0201",   "02", "is a",
      "010101", "0101", "is a"
      )
    # nolint end
  )
})

test_that("`build_prefix_hierarchy_len()` handles edge cases", {
  # Single code - no relationships
  result <- build_prefix_hierarchy_len("A")
  expect_equal(nrow(result), 0)

  # Codes with same length - no relationships
  result <- build_prefix_hierarchy_len(c("AA", "BB", "CC"))
  expect_equal(nrow(result), 0)
})

# `process_icd10_lkp()` ---------------------------------------------------

test_that("`process_icd10_lkp()` removes 'X' suffix and combines modifiers", {
  # fmt: skip
  df <- tibble::tribble(
    ~ALT_CODE , ~DESCRIPTION             , ~MODIFIER_4 , ~MODIFIER_5 ,
    "A38X"    , "Scarlet fever"          , NA          , NA          ,
    "E10"     , "Type 1 diabetes"        , "With coma" , NA          ,
    "S27"     , "Traumatic pneumothorax" , NA          , "Bilateral"
  )

  result <- process_icd10_lkp(df, "UKB v4", "test_source")

  # Check 'X' removed
  expect_equal(result$lookup$table$ALT_CODE, c("A38", "E10", "S27"))

  # Check modifiers combined
  expect_equal(
    result$lookup$table$DESCRIPTION,
    c(
      "Scarlet fever",
      "Type 1 diabetes With coma",
      "Traumatic pneumothorax Bilateral"
    )
  )

  # Check relationship table created
  expect_true("relationship" %in% names(result))
  expect_true(nrow(result$relationship$table) >= 0)
})

# `process_bnf_lkp()` -----------------------------------------------------

test_that("`process_bnf_lkp()` creates hierarchy correctly", {
  .df <- data.frame(
    stringsAsFactors = FALSE,
    BNF_Presentation_Code = c("010101000BBABA0"),
    BNF_Presentation = c("Langdales_Cinnamon Tab"),
    BNF_Product = c("Proprietary Co Prepn Bnf 0101010"),
    BNF_Chemical_Substance = c("Other Antacid & Simeticone Preps"),
    BNF_Subparagraph = c("Antacids and Simeticone"),
    BNF_Paragraph = c("Antacids and Simeticone"),
    BNF_Section = c("Dyspep&Gastro-Oesophageal Reflux Disease"),
    BNF_Chapter = c("Gastro-Intestinal System")
  )

  result <- process_bnf_lkp(.df, "UKB v4", "test_source")

  # Check lookup table has multiple levels
  expect_true(nrow(result$lookup$table) > 1)

  # Check BNF_Code_Level column exists and has expected values
  expect_true("BNF_Code_Level" %in% names(result$lookup$table))
  expect_true("chapter" %in% result$lookup$table$BNF_Code_Level)
  expect_true("section" %in% result$lookup$table$BNF_Code_Level)

  # Check relationship table created
  expect_true("relationship" %in% names(result))
  expect_true(nrow(result$relationship$table) > 0)
})

# Auto-dependency tests ---------------------------------------------------

test_that("Auto-dependency inclusion works for read_v2_drugs_bnf", {
  path <- dummy_ukb_resource_592_path()

  # Request only read_v2_drugs_bnf, without its dependencies
  result <- suppressMessages(
    read_ukb_resource_592(path, sheets = "read_v2_drugs_bnf")
  )

  # Should automatically include bnf_lkp and read_v2_drugs_lkp
  expect_true("read_v2_drugs_bnf" %in% names(result))
  expect_true("bnf_lkp" %in% names(result))
  expect_true("read_v2_drugs_lkp" %in% names(result))

  # Should have 3 sheets total
  expect_equal(length(result), 3)
})

test_that("Auto-dependency inclusion works for read_v2_icd10", {
  path <- dummy_ukb_resource_592_path()

  # Request only read_v2_icd10, without its dependency
  result <- suppressMessages(
    read_ukb_resource_592(path, sheets = "read_v2_icd10")
  )

  # Should automatically include icd10_lkp
  expect_true("read_v2_icd10" %in% names(result))
  expect_true("icd10_lkp" %in% names(result))

  # Should have 2 sheets total
  expect_equal(length(result), 2)
})

test_that("add_ukb592_dependencies() does not add duplicates", {
  # Test with read_v2_drugs_bnf and its dependencies explicitly included
  sheets <- c("read_v2_drugs_bnf", "bnf_lkp", "read_v2_drugs_lkp")
  result <- add_ukb592_dependencies(sheets, inform = FALSE)

  # Should still have only 3 unique sheets (no duplicates)
  expect_equal(length(result), 3)
  expect_equal(length(unique(result)), 3)
  expect_true(all(
    c("read_v2_drugs_bnf", "bnf_lkp", "read_v2_drugs_lkp") %in% result
  ))
})

test_that("add_ukb592_dependencies() adds missing dependencies", {
  # Test with read_v2_drugs_bnf only
  result <- add_ukb592_dependencies("read_v2_drugs_bnf", inform = FALSE)
  expect_equal(length(result), 3)
  expect_true(all(
    c("read_v2_drugs_bnf", "bnf_lkp", "read_v2_drugs_lkp") %in% result
  ))

  # Test with read_v2_icd10 only
  result <- add_ukb592_dependencies("read_v2_icd10", inform = FALSE)
  expect_equal(length(result), 2)
  expect_true(all(c("read_v2_icd10", "icd10_lkp") %in% result))

  # Test with both sheets
  result <- add_ukb592_dependencies(
    c("read_v2_drugs_bnf", "read_v2_icd10"),
    inform = FALSE
  )
  expect_equal(length(result), 5)
  expect_true(all(
    c(
      "read_v2_drugs_bnf",
      "bnf_lkp",
      "read_v2_drugs_lkp",
      "read_v2_icd10",
      "icd10_lkp"
    ) %in%
      result
  ))
})

test_that("add_ukb592_dependencies() does nothing for sheets without dependencies", {
  # Test with sheets that don't need dependencies
  sheets <- c("icd9_lkp", "dmd_lkp")
  result <- add_ukb592_dependencies(sheets, inform = FALSE)

  expect_equal(length(result), 2)
  expect_equal(result, sheets)
})
