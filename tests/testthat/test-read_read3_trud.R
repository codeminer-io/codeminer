test_that("read_read3_trud() errors on nonexistent path", {
  expect_error(read_read3_trud("/nonexistent/path"), "Path does not exist")
})

test_that("read_read3_trud() validates tables argument", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "V3"))
  expect_error(
    suppressMessages(read_read3_trud(tmp, tables = "bad_table")),
    class = "rlang_error"
  )
})

test_that("read_read3_trud() errors when V3 directory missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    suppressMessages(read_read3_trud(tmp)),
    "V3"
  )
})

test_that("read_read3_trud() returns both tables by default", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir))

  expect_named(result, c("read3_lkp", "read3_relationship"))
})

test_that("read_read3_trud() read3_lkp has correct structure", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))

  meta <- result$read3_lkp$lookup$metadata
  expect_s3_class(result$read3_lkp$lookup$table, "data.frame")
  expect_equal(meta$code_type, "Read v3")
  expect_equal(meta$lookup_code_col, "code")
  expect_equal(meta$lookup_description_col, "term")
  expect_equal(meta$preferred_description_col, "desc_type")
  expect_equal(meta$preferred_description_indicator, "P")
})

test_that("read_read3_trud() read3_lkp retains retired codes, synonyms, and non-clinical terms", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # Retired code X40J7 should be present
  expect_true("X40J7" %in% tbl$code)
  # Synonym desc_type ("S") and non-clinical term_type ("O") should be present
  expect_setequal(unique(tbl$desc_type), c("P", "S"))
  expect_setequal(unique(tbl$status), c("C", "R"))
  expect_setequal(unique(tbl$term_type), c("C", "O"))
})

test_that("read_read3_trud() col_filters default to active codes + clinical terms", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  cf <- deserialise_col_filters(
    result$read3_lkp$lookup$metadata$col_filters
  )

  expect_setequal(names(cf), c("status", "term_type"))
  expect_equal(cf$status$defaults, "C")
  expect_setequal(cf$status$values, c("C", "R"))
  expect_equal(cf$term_type$defaults, "C")
  expect_setequal(cf$term_type$values, c("C", "O"))
})

test_that("read_read3_trud() read3_relationship has correct structure", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(
    dir,
    tables = "read3_relationship"
  ))

  expect_true("relationship" %in% names(result$read3_relationship))
  expect_s3_class(result$read3_relationship$relationship$table, "data.frame")
  expect_equal(
    result$read3_relationship$relationship$metadata$code_type,
    "Read v3"
  )
  expect_true(is.na(
    result$read3_relationship$relationship$metadata$child_parent_relationship_code
  ))
})

test_that("read_read3_trud() relationship table has expected rows", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(
    dir,
    tables = "read3_relationship"
  ))
  tbl <- result$read3_relationship$relationship$table

  # Augmented fixture: CHAP1/CHAP2 -> root, X40J5 -> CHAP1, X40J5 -> CHAP2,
  # X40J6 -> X40J5, X40J7 -> X40J6, plus X40J8 -> X40J6 via non-"01"
  # sequence number (7 edges).
  expect_equal(nrow(tbl), 7L)
  expect_true("child_code" %in% names(tbl))
  expect_true("parent_code" %in% names(tbl))
})

test_that("read_read3_trud() tables argument selects subset", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  expect_named(result, "read3_lkp")
  expect_false("read3_relationship" %in% names(result))
})

test_that("read_read3_trud() uses custom version", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, version = "CTV3_v25"))
  expect_equal(result$read3_lkp$lookup$metadata$lookup_version, "CTV3_v25")
  expect_equal(
    result$read3_relationship$relationship$metadata$relationship_version,
    "CTV3_v25"
  )
})

test_that("read_read3_trud() accepts zip file input", {
  tmp <- withr::local_tempdir()
  release_name <- "Read3_Dummy_Release"
  release_dir <- file.path(tmp, release_name)
  v3_dir <- file.path(release_dir, "V3")
  dir.create(v3_dir, recursive = TRUE)

  writeLines(c("X40J5|C|P|"), file.path(v3_dir, "Concept.v3"))
  writeLines(c("X40J5|D001|P"), file.path(v3_dir, "Descrip.v3"))
  writeLines(c("D001|C|Test disorder||"), file.path(v3_dir, "Terms.v3"))
  writeLines(c("X40J5|X40J6|01"), file.path(v3_dir, "V3hier.v3"))

  zip_path <- file.path(tmp, paste0(release_name, ".zip"))
  withr::with_dir(
    tmp,
    utils::zip(zipfile = paste0(release_name, ".zip"), files = release_name)
  )

  result <- suppressMessages(read_read3_trud(zip_path))

  expect_named(result, c("read3_lkp", "read3_relationship"))
  expect_gt(nrow(result$read3_lkp$lookup$table), 0)
})

# Category attach ----------------------------------------------------------

test_that("read_read3_trud() lookup metadata sets lookup_category_col = 'category'", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  expect_equal(result$read3_lkp$lookup$metadata$lookup_category_col, "category")
})

test_that("read_read3_trud() chapter codes get their own description as category", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  expect_equal(unique(tbl$category[tbl$code == "CHAP1"]), "Body system")
  expect_equal(unique(tbl$category[tbl$code == "CHAP2"]), "Clinical findings")
})

test_that("read_read3_trud() root code gets its own description as category", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  expect_equal(unique(tbl$category[tbl$code == "....."]), "Read thesaurus")
})

test_that("read_read3_trud() multi-parent codes tie-break to alphabetically-first chapter description", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # X40J5 is a direct child of both CHAP1 ("Body system") and CHAP2
  # ("Clinical findings"). Alphabetical tie-break picks "Body system".
  expect_equal(unique(tbl$category[tbl$code == "X40J5"]), "Body system")
})

test_that("read_read3_trud() descendant codes inherit the chapter category transitively", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # X40J6 and X40J7 are reachable from both chapters via X40J5, so they
  # also pick "Body system" by the same tie-break.
  expect_equal(unique(tbl$category[tbl$code == "X40J6"]), "Body system")
  expect_equal(unique(tbl$category[tbl$code == "X40J7"]), "Body system")
})

test_that("read_read3_trud() orphan codes (no hierarchy edge) get NA category", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  expect_true(all(is.na(tbl$category[tbl$code == "ORPH1"])))
})

test_that("read_read3_trud() retired codes still inherit their chapter category", {
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table

  # X40J7 is retired (status = "R") but still inherits the chapter category
  # — category is independent of active-status.
  expect_equal(unique(tbl$category[tbl$code == "X40J7"]), "Body system")
})

# Relationship metadata (#148) ---------------------------------------------

test_that("read_read3_trud() relationship metadata uses NA child_parent_relationship_code", {
  # CTV3 V3hier.v3 uses `relationship_type` as a per-pair sequence number
  # (01..99), not a semantic label. The metadata signals this to
  # `graph_closure()` by setting `child_parent_relationship_code` to NA so
  # every row is treated as a valid hierarchical edge.
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(
    dir,
    tables = "read3_relationship"
  ))
  meta <- result$read3_relationship$relationship$metadata
  expect_true(is.na(meta$child_parent_relationship_code))
})

test_that("read_read3_trud() category walk includes parents reached via non-01 sequence numbers", {
  # X40J8 hangs off X40J6 via relationship_type = "05" in the dummy. Before
  # #148, the walk filtered by "01" and X40J8 dropped out. After #148, every
  # row is a valid edge so X40J8 inherits the chapter category like its
  # siblings.
  dir <- create_dummy_read3_dir()
  result <- suppressMessages(read_read3_trud(dir, tables = "read3_lkp"))
  tbl <- result$read3_lkp$lookup$table
  expect_equal(unique(tbl$category[tbl$code == "X40J8"]), "Body system")
})
