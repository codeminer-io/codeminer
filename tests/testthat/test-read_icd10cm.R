test_that("read_icd10cm() errors on nonexistent path", {
  expect_error(read_icd10cm("/nonexistent/path"), "Path does not exist")
})

test_that("read_icd10cm() returns lookup and relationship tables with metadata", {
  path <- create_dummy_icd10cm_file()
  result <- suppressMessages(read_icd10cm(path))

  expect_named(result, c("icd10cm_lkp", "icd10cm_relationship"))

  lkp <- result$icd10cm_lkp$lookup
  expect_s3_class(lkp$table, "data.frame")
  expect_equal(lkp$metadata$code_type, "ICD-10-CM")
  expect_equal(lkp$metadata$lookup_code_col, "icd10cm_code")
  expect_equal(lkp$metadata$lookup_description_col, "description")
  expect_equal(lkp$metadata$lookup_category_col, "category")

  rel <- result$icd10cm_relationship$relationship
  expect_s3_class(rel$table, "data.frame")
  expect_equal(rel$metadata$code_type, "ICD-10-CM")
  # Purely hierarchical: no type column, so both type fields are NA.
  expect_equal(rel$metadata$type_col, NA_character_)
  expect_equal(rel$metadata$child_parent_relationship_code, NA_character_)
})

test_that("read_icd10cm() parses the fixed-width columns", {
  path <- create_dummy_icd10cm_file()
  tbl <- suppressMessages(read_icd10cm(path))$icd10cm_lkp$lookup$table

  expect_named(
    tbl,
    c(
      "icd10cm_code",
      "icd10cm_code_dotted",
      "billable",
      "short_description",
      "description",
      "category"
    )
  )
  expect_equal(nrow(tbl), nrow(dummy_icd10cm_rows()))

  row <- tbl[tbl$icd10cm_code == "E113211", ]
  expect_equal(row$icd10cm_code_dotted, "E11.3211")
  expect_equal(row$billable, "1")
  expect_equal(row$short_description, "Dummy diab, right eye")
  expect_equal(
    row$description,
    "Dummy diabetes with mild retinopathy, right eye"
  )

  # 3-character categories have no decimal point and are headers
  cat_row <- tbl[tbl$icd10cm_code == "E11", ]
  expect_equal(cat_row$icd10cm_code_dotted, "E11")
  expect_equal(cat_row$billable, "0")
})

test_that("read_icd10cm() retains placeholder X characters in codes", {
  path <- create_dummy_icd10cm_file()
  tbl <- suppressMessages(read_icd10cm(path))$icd10cm_lkp$lookup$table

  expect_true("T07XXXA" %in% tbl$icd10cm_code)
  expect_equal(
    tbl[tbl$icd10cm_code == "T07XXXA", ]$icd10cm_code_dotted,
    "T07.XXXA"
  )
})

test_that("read_icd10cm() attaches the ICD-10-CM chapter as the category", {
  path <- create_dummy_icd10cm_file()
  tbl <- suppressMessages(read_icd10cm(path))$icd10cm_lkp$lookup$table
  chap_for <- function(code) tbl$category[tbl$icd10cm_code == code]

  expect_equal(chap_for("E11"), "Endocrine")
  expect_equal(chap_for("E113211"), "Endocrine")
  expect_equal(chap_for("C4A"), "Neoplasms")
  expect_equal(chap_for("K95"), "Digestive")
  expect_equal(chap_for("O9A"), "Pregnancy")
  expect_equal(chap_for("QA10"), "Congenital")
  expect_equal(chap_for("T07XXXA"), "Injury")
  expect_false(anyNA(tbl$category))
})

test_that("read_icd10cm() warns about categories outside the chapter ranges", {
  # F00 is not an ICD-10-CM category (the Mental chapter starts at F01)
  rows <- rbind(
    dummy_icd10cm_rows(),
    data.frame(code = "F00", billable = "0", short = "Dummy", long = "Dummy")
  )
  path <- create_dummy_icd10cm_file(lines = dummy_icd10cm_lines(rows))

  expect_warning(
    result <- suppressMessages(read_icd10cm(path)),
    "did not match any chapter range.*F00"
  )
  tbl <- result$icd10cm_lkp$lookup$table
  expect_true(is.na(tbl$category[tbl$icd10cm_code == "F00"]))
  expect_equal(sum(is.na(tbl$category)), 1L)
})

test_that("read_icd10cm() exposes billable as a column filter with both values by default", {
  path <- create_dummy_icd10cm_file()
  meta <- suppressMessages(read_icd10cm(path))$icd10cm_lkp$lookup$metadata

  cf <- deserialise_col_filters(meta$col_filters)
  expect_named(cf, "billable")
  expect_setequal(cf$billable$values, c("0", "1"))
  expect_setequal(cf$billable$defaults, c("0", "1"))
  expect_equal(cf$billable$value_labels[["1"]], "Billable")
  expect_equal(cf$billable$value_labels[["0"]], "Header")
})

test_that("read_icd10cm() derives the hierarchy using the longest valid prefix", {
  path <- create_dummy_icd10cm_file()
  rel <- suppressMessages(read_icd10cm(
    path
  ))$icd10cm_relationship$relationship$table
  parent_of <- function(code) rel$to[rel$from == code]

  expect_named(rel, c("from", "to"))
  expect_equal(parent_of("E113211"), "E11321")
  expect_equal(parent_of("E113"), "E11")
  expect_equal(parent_of("C4A0"), "C4A")
  expect_equal(parent_of("QA10"), "QA1")
  # T07XXX, T07XX and T07X are not codes, so T07XXXA attaches to T07
  expect_equal(parent_of("T07XXXA"), "T07")
  # 3-character categories are roots
  expect_false("E11" %in% rel$from)
})

test_that("read_icd10cm() derives the version from the file name", {
  path <- create_dummy_icd10cm_file(name = "icd10cm_order_20250401")
  result <- suppressMessages(read_icd10cm(path))

  expect_equal(result$icd10cm_lkp$lookup$metadata$lookup_version, "20250401")
  expect_equal(
    result$icd10cm_relationship$relationship$metadata$relationship_version,
    "20250401"
  )
})

test_that("read_icd10cm() uses an explicit version", {
  path <- create_dummy_icd10cm_file()
  result <- suppressMessages(read_icd10cm(path, version = "MY_VERSION"))
  expect_equal(result$icd10cm_lkp$lookup$metadata$lookup_version, "MY_VERSION")
})

test_that("read_icd10cm() errors when no version can be derived from the file name", {
  path <- create_dummy_icd10cm_file(name = "icd10cm-order-2027")
  expect_error(
    suppressMessages(read_icd10cm(path)),
    "Could not derive an ICD-10-CM version"
  )
})

test_that("read_icd10cm() uses custom source", {
  path <- create_dummy_icd10cm_file()
  result <- suppressMessages(read_icd10cm(
    path,
    source = "https://custom.source/"
  ))
  expect_equal(
    result$icd10cm_lkp$lookup$metadata$lookup_source,
    "https://custom.source/"
  )
  expect_equal(
    result$icd10cm_relationship$relationship$metadata$relationship_source,
    "https://custom.source/"
  )
})

test_that("read_icd10cm() reads the order file from a zip, ignoring the addenda", {
  path <- create_dummy_icd10cm_file(zip = TRUE)
  result <- suppressMessages(read_icd10cm(path))
  tbl <- result$icd10cm_lkp$lookup$table

  expect_equal(nrow(tbl), nrow(dummy_icd10cm_rows()))
  expect_false("Z999" %in% tbl$icd10cm_code)
  # version derived from the zip file name, not the inner file
  expect_equal(result$icd10cm_lkp$lookup$metadata$lookup_version, "20261001")
})

test_that("read_icd10cm() errors when a zip has no single order file", {
  dir <- withr::local_tempdir()
  writeLines("not an order file", file.path(dir, "readme.txt"))
  zip_path <- file.path(dir, "icd10cm_order_20261001.zip")
  withr::with_dir(
    dir,
    utils::zip(zipfile = basename(zip_path), files = "readme.txt", flags = "-q")
  )

  expect_error(
    suppressMessages(read_icd10cm(zip_path)),
    "Expected exactly one ICD-10-CM order file"
  )
})

test_that("read_icd10cm() handles CRLF line endings and trailing blank lines", {
  lines <- c(dummy_icd10cm_lines(), "")
  path <- create_dummy_icd10cm_file(lines = lines, sep = "\r\n")
  tbl <- suppressMessages(read_icd10cm(path))$icd10cm_lkp$lookup$table

  expect_equal(nrow(tbl), nrow(dummy_icd10cm_rows()))
  expect_false(any(grepl("\r", tbl$description)))
  expect_equal(
    tbl[tbl$icd10cm_code == "T07XXXA", ]$description,
    "Dummy unspecified multiple injuries, initial encounter"
  )
})

test_that("read_icd10cm() errors on a file that is not in the fixed-width layout", {
  lines <- c(
    dummy_icd10cm_lines(),
    "E12 1 This line is not aligned to the fixed-width columns"
  )
  path <- create_dummy_icd10cm_file(lines = lines)

  expect_error(
    suppressMessages(read_icd10cm(path)),
    "does not have the expected fixed-width layout"
  )
})

test_that("read_icd10cm() errors on duplicate codes", {
  rows <- dummy_icd10cm_rows()
  rows <- rbind(rows, rows[rows$code == "E11", ])
  path <- create_dummy_icd10cm_file(lines = dummy_icd10cm_lines(rows))

  expect_error(
    suppressMessages(read_icd10cm(path)),
    "Duplicate ICD-10-CM codes.*E11"
  )
})
