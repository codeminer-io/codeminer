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
