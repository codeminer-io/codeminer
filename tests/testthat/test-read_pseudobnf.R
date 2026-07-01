test_that("read_pseudobnf() returns a bnf_lkp lookup with canonical metadata", {
  result <- read_pseudobnf(dummy_pseudobnf_path())

  meta <- result$bnf_lkp$lookup$metadata
  expect_equal(meta$code_type, "BNF")
  expect_equal(meta$lookup_code_col, "BNF_Code")
  expect_equal(meta$lookup_description_col, "Description")
  expect_equal(meta$lookup_category_col, "BNF_Chapter")

  tbl <- result$bnf_lkp$lookup$table
  expect_true(all(
    c(
      meta$lookup_code_col,
      meta$lookup_description_col,
      meta$lookup_category_col
    ) %in%
      names(tbl)
  ))
})

test_that("read_pseudobnf() lookup covers every hierarchy level, one row per code", {
  tbl <- read_pseudobnf(dummy_pseudobnf_path())$bnf_lkp$lookup$table

  expect_setequal(
    unique(tbl$BNF_Code_Level),
    c(
      "chapter",
      "section",
      "paragraph",
      "subparagraph",
      "chemical_substance",
      "product_name",
      "full"
    )
  )

  # One distinct row per code
  expect_equal(nrow(tbl), dplyr::n_distinct(tbl$BNF_Code))

  # Category (chapter) is populated for every row, deeper columns are NA above
  # the row's own level (mirroring the UK Biobank resource 592 BNF lookup).
  expect_false(any(is.na(tbl$BNF_Chapter)))
  chapter_rows <- tbl[tbl$BNF_Code_Level == "chapter", ]
  expect_true(all(is.na(chapter_rows$BNF_Section)))
})

test_that("read_pseudobnf() level codes are taken from the explicit *_CODE columns", {
  tbl <- read_pseudobnf(dummy_pseudobnf_path())$bnf_lkp$lookup$table

  # Code lengths follow the BNF convention per level (2/4/6/7/9/11/15)
  level_len <- c(
    chapter = 2L,
    section = 4L,
    paragraph = 6L,
    subparagraph = 7L,
    chemical_substance = 9L,
    product_name = 11L,
    full = 15L
  )
  expect_equal(
    nchar(tbl$BNF_Code),
    unname(level_len[tbl$BNF_Code_Level])
  )
})

test_that("read_pseudobnf() builds a BNF relationship with valid parent prefixes", {
  result <- read_pseudobnf(dummy_pseudobnf_path())
  rel <- result$bnf_lkp$relationship$table
  meta <- result$bnf_lkp$relationship$metadata

  expect_equal(meta$code_type, "BNF")
  expect_equal(c(meta$from_col, meta$to_col), c("from", "to"))
  expect_true(all(c("from", "to") %in% names(rel)))
  expect_gt(nrow(rel), 0)

  # Each child code starts with its parent code, and both are known codes
  expect_true(all(startsWith(rel$from, rel$to)))
  codes <- result$bnf_lkp$lookup$table$BNF_Code
  expect_true(all(rel$from %in% codes))
  expect_true(all(rel$to %in% codes))
})

test_that("read_pseudobnf() errors clearly when expected columns are missing", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(foo = "1", bar = "2"), tmp)

  expect_error(read_pseudobnf(tmp), "missing expected column")
})

test_that("read_pseudobnf() derives the version label from the file name", {
  meta <- read_pseudobnf(dummy_pseudobnf_path())$bnf_lkp$lookup$metadata
  expect_equal(
    meta$lookup_version,
    as.character(fs::path_ext_remove(basename(dummy_pseudobnf_path())))
  )
})
