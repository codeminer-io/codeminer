test_that("expand_icd10_range() expands a within-letter range inclusively", {
  expect_equal(expand_icd10_range("E00", "E03"), c("E00", "E01", "E02", "E03"))
  expect_equal(expand_icd10_range("Z99", "Z99"), "Z99")
})

test_that("expand_icd10_range() spans letter boundaries", {
  out <- expand_icd10_range("A98", "B02")
  expect_equal(out, c("A98", "A99", "B00", "B01", "B02"))
})

test_that("expand_icd10_range() covers a full chapter spanning three letters", {
  out <- expand_icd10_range("V01", "Y98")
  expect_equal(head(out, 2), c("V01", "V02"))
  expect_equal(tail(out, 2), c("Y97", "Y98"))
  # V01..V99 (99) + W00..W99 (100) + X00..X99 (100) + Y00..Y98 (99) # nolint: commented_code_linter.
  expect_equal(length(out), 398L)
})

test_that("icd10_chapter_lookup() maps each chapter boundary correctly", {
  lookup <- icd10_chapter_lookup()
  chap_for <- function(prefix) lookup$chapter[lookup$prefix == prefix]

  expect_equal(chap_for("A00"), "Infectious")
  expect_equal(chap_for("B99"), "Infectious")
  expect_equal(chap_for("C00"), "Neoplasms")
  expect_equal(chap_for("D48"), "Neoplasms")
  expect_equal(chap_for("D50"), "Blood")
  expect_equal(chap_for("D89"), "Blood")
  expect_equal(chap_for("E10"), "Endocrine")
  expect_equal(chap_for("H59"), "Eye")
  expect_equal(chap_for("H60"), "Ear")
  expect_equal(chap_for("S00"), "Injury")
  expect_equal(chap_for("T98"), "Injury")
  expect_equal(chap_for("U85"), "Special purposes")
})

test_that("icd10_chapter_lookup() has no overlapping prefixes", {
  expect_equal(
    anyDuplicated(icd10_chapter_lookup()$prefix),
    0L
  )
})

# ICD-10-CM --------------------------------------------------------------

test_that("icd10cm_chapter_lookup() resolves CM-specific and alphanumeric categories", {
  prefixes <- c(
    "A00",
    "B99",
    "C4A",
    "D49",
    "D50",
    "H59",
    "H60",
    "K95",
    "O9A",
    "QA1",
    "T88",
    "U07",
    "V00",
    "Y99",
    "Z3A"
  )
  lookup <- icd10cm_chapter_lookup(prefixes)
  chap_for <- function(prefix) lookup$chapter[lookup$prefix == prefix]

  expect_equal(chap_for("A00"), "Infectious")
  expect_equal(chap_for("B99"), "Infectious")
  expect_equal(chap_for("C4A"), "Neoplasms")
  expect_equal(chap_for("D49"), "Neoplasms")
  expect_equal(chap_for("D50"), "Blood")
  expect_equal(chap_for("H59"), "Eye")
  expect_equal(chap_for("H60"), "Ear")
  expect_equal(chap_for("K95"), "Digestive")
  expect_equal(chap_for("O9A"), "Pregnancy")
  expect_equal(chap_for("QA1"), "Congenital")
  expect_equal(chap_for("T88"), "Injury")
  expect_equal(chap_for("U07"), "Special purposes")
  expect_equal(chap_for("V00"), "External causes")
  expect_equal(chap_for("Y99"), "External causes")
  expect_equal(chap_for("Z3A"), "Health status")
})

test_that("icd10cm_chapter_lookup() returns one row per unique prefix, NA when unmatched", {
  lookup <- icd10cm_chapter_lookup(c("E11", "E11", "F00"))

  expect_equal(nrow(lookup), 2L)
  expect_equal(lookup$chapter[lookup$prefix == "E11"], "Endocrine")
  # F00 is not an ICD-10-CM category (the chapter starts at F01)
  expect_true(is.na(lookup$chapter[lookup$prefix == "F00"]))
})

test_that("icd10cm_chapters() ranges do not overlap", {
  chapters <- icd10cm_chapters()
  pairs <- utils::combn(nrow(chapters), 2)
  overlaps <- purrr::map2_lgl(pairs[1, ], pairs[2, ], function(i, j) {
    chapters$start[i] <= chapters$end[j] && chapters$start[j] <= chapters$end[i]
  })
  expect_false(any(overlaps))
})
