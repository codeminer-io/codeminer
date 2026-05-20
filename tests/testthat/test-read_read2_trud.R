test_that("read_read2_trud() errors on nonexistent path", {
  expect_error(read_read2_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_read2_trud() validates tables argument", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_read2_trud(tmp, tables = "bad_table"),
    class = "rlang_error"
  )
})

test_that("read_read2_trud() errors when Standard/V2 directory missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_read2_trud(tmp)),
    "Standard/V2"
  )
})

test_that("read_read2_trud() errors when DESC.DBF missing", {
  tmp <- withr::local_tempdir()
  v2_dir <- file.path(tmp, "Standard", "V2")
  dir.create(v2_dir, recursive = TRUE)

  expect_error(
    suppressMessages(read_read2_trud(tmp, tables = "read2_lkp")),
    "DESC\\.DBF|Term\\.dbf|TERM198"
  )
})

test_that("read_read2_trud() coalesces TERM198 over TERM60 into `term`", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))
  tbl <- result$read2_lkp$lookup$table

  # TERMID 00001 has both TERM60 and TERM198 — should pick TERM198
  expect_match(
    tbl$term[tbl$TERMID == "00001"][1],
    "full long-form"
  )
  # TERMID 00002 has only TERM60 (no TERM198) — should fall back
  expect_equal(tbl$term[tbl$TERMID == "00002"], "Hypertension")
})

test_that("read_read2_trud() returns both lookup + relationship by default", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir))

  expect_named(result, c("read2_lkp", "read2_relationship"))
})

test_that("read_read2_trud() read2_lkp has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))

  meta <- result$read2_lkp$lookup$metadata
  expect_s3_class(result$read2_lkp$lookup$table, "data.frame")
  expect_equal(meta$code_type, "Read v2")
  expect_equal(meta$lookup_code_col, "CC")
  expect_equal(meta$lookup_description_col, "term")
  expect_equal(meta$preferred_description_col, "TERMTYPE")
  expect_equal(meta$preferred_description_indicator, "P")
})

test_that("read_read2_trud() read2_lkp retains synonyms and retired codes", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))
  tbl <- result$read2_lkp$lookup$table

  # Fixture has 4 rows total: 1234. (P + S), 5678. (P), 9999. (R, P)
  expect_equal(nrow(tbl), 4L)
  expect_setequal(unique(tbl$TERMTYPE), c("P", "S"))
  expect_setequal(unique(tbl$CCSTATUS), c("C", "R"))
  expect_equal(sum(tbl$CC == "1234."), 2L)
})

test_that("read_read2_trud() read2_lkp col_filters defaults to active codes", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, tables = "read2_lkp"))
  cf <- result$read2_lkp$lookup$metadata$col_filters

  cf_parsed <- deserialise_col_filters(cf)
  expect_setequal(names(cf_parsed), "CCSTATUS")
  expect_equal(cf_parsed$CCSTATUS$defaults, "C")
  expect_setequal(cf_parsed$CCSTATUS$values, c("C", "R"))
})

test_that("read_read2_trud() read2_relationship has correct structure", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(
    dir,
    tables = "read2_relationship"
  ))

  rel <- result$read2_relationship$relationship
  expect_s3_class(rel$table, "data.frame")
  expect_equal(rel$metadata$code_type, "Read v2")
  expect_equal(rel$metadata$child_parent_relationship_code, "is a")
})

test_that("read_read2_trud() tables argument selects subset", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(
    dir,
    tables = "read2_lkp"
  ))

  expect_named(result, "read2_lkp")
})

test_that("read_read2_trud() uses custom version", {
  dir <- create_dummy_read2_dir()
  result <- suppressMessages(read_read2_trud(dir, version = "RB_v2018"))

  expect_equal(result$read2_lkp$lookup$metadata$lookup_version, "RB_v2018")
})

test_that("read_read2_trud() accepts zip file input", {
  src <- create_dummy_read2_dir()
  tmp <- withr::local_tempdir()
  zip_name <- "ReadBrowser_Dummy_Release.zip"
  zip_path <- file.path(tmp, zip_name)

  # NHS Read Browser zips have Standard/ (and siblings) at the zip root,
  # with no single wrapping directory.
  withr::with_dir(
    src,
    utils::zip(zipfile = zip_path, files = "Standard")
  )

  result <- suppressMessages(read_read2_trud(zip_path))

  expect_named(result, c("read2_lkp", "read2_relationship"))
  expect_gt(nrow(result$read2_lkp$lookup$table), 0)
})
