test_that("read_cprd_aurum() errors on nonexistent path", {
  expect_error(read_cprd_aurum("/nonexistent/path"), "Path does not exist")
})

test_that("read_cprd_aurum() errors when a dictionary file is missing", {
  dir <- create_dummy_cprd_aurum_dir()
  file.remove(file.path(dir, "CPRDAurumProduct.txt"))

  expect_error(
    suppressMessages(read_cprd_aurum(dir)),
    "Could not find.*set `tables`"
  )
  # The medcode tables alone don't need the product file
  expect_no_error(
    suppressMessages(
      read_cprd_aurum(
        dir,
        tables = c(
          "cprd_aurum_medcode_lkp",
          "cprd_aurum_medcode_snomed",
          "cprd_aurum_medcode_read2"
        )
      )
    )
  )
})

test_that("read_cprd_aurum() rejects unknown table names", {
  dir <- create_dummy_cprd_aurum_dir()
  expect_error(read_cprd_aurum(dir, tables = "not_a_table"))
})

test_that("read_cprd_aurum() returns every table by default, in a fixed order", {
  dir <- create_dummy_cprd_aurum_dir()
  result <- suppressMessages(read_cprd_aurum(dir))

  expect_named(
    result,
    c(
      "cprd_aurum_medcode_lkp",
      "cprd_aurum_prodcode_lkp",
      "cprd_aurum_medcode_snomed",
      "cprd_aurum_medcode_read2",
      "cprd_aurum_prodcode_snomed"
    )
  )
  expect_named(result$cprd_aurum_medcode_lkp, "lookup")
  expect_named(result$cprd_aurum_medcode_snomed, "mapping")
})

test_that("read_cprd_aurum() reads only the requested tables", {
  dir <- create_dummy_cprd_aurum_dir()
  result <- suppressMessages(
    read_cprd_aurum(
      dir,
      tables = c("cprd_aurum_prodcode_snomed", "cprd_aurum_medcode_lkp")
    )
  )

  expect_named(
    result,
    c("cprd_aurum_medcode_lkp", "cprd_aurum_prodcode_snomed")
  )
})

test_that("read_cprd_aurum() takes the version from the release name, with a message", {
  dir <- create_dummy_cprd_aurum_dir()
  expect_message(
    result <- read_cprd_aurum(dir, tables = "cprd_aurum_medcode_lkp"),
    "Using version"
  )
  expect_equal(
    result$cprd_aurum_medcode_lkp$lookup$metadata$lookup_version,
    "209901"
  )
})

test_that("read_cprd_aurum() errors if the release date can't be inferred", {
  other <- create_dummy_cprd_aurum_dir(dir_name = "my_aurum_release")
  expect_error(
    suppressMessages(read_cprd_aurum(other)),
    "Could not find a release date"
  )

  # An explicit version works for any name, without the inference message
  result <- suppressMessages(read_cprd_aurum(other, version = "v_test"))
  expect_equal(
    result$cprd_aurum_medcode_lkp$lookup$metadata$lookup_version,
    "v_test"
  )
  expect_no_message(
    read_cprd_aurum(
      other,
      tables = "cprd_aurum_medcode_lkp",
      version = "v_test"
    ),
    message = "Using version"
  )
})

test_that("read_cprd_aurum() uses an explicit version", {
  dir <- create_dummy_cprd_aurum_dir()
  result <- suppressMessages(read_cprd_aurum(dir, version = "v_test"))
  expect_equal(
    result$cprd_aurum_medcode_lkp$lookup$metadata$lookup_version,
    "v_test"
  )
})

test_that("read_cprd_aurum() builds the medcode lookup", {
  dir <- create_dummy_cprd_aurum_dir()
  lkp <- suppressMessages(read_cprd_aurum(dir))$cprd_aurum_medcode_lkp$lookup

  expect_equal(lkp$metadata$code_type, "CPRD Aurum medcode")
  expect_equal(lkp$metadata$lookup_code_col, "medcodeid")
  expect_equal(lkp$metadata$lookup_description_col, "term")

  tbl <- lkp$table
  expect_equal(
    names(tbl),
    c(
      "medcodeid",
      "observations",
      "originalreadcode",
      "cleansedreadcode",
      "term",
      "snomedctconceptid",
      "snomedctdescriptionid",
      "release",
      "emiscodecategoryid"
    )
  )
  expect_equal(nrow(tbl), 3L)
  expect_type(tbl$observations, "integer")
  # Identifiers are always read as character
  expect_type(tbl$medcodeid, "character")
  expect_type(tbl$emiscodecategoryid, "character")
  # The empty Release column is kept, not dropped
  expect_true(all(is.na(tbl$release)))

  filters <- deserialise_col_filters(lkp$metadata$col_filters)
  expect_named(filters, "emiscodecategoryid")
  expect_equal(filters$emiscodecategoryid$values, c("1", "2"))
  expect_equal(
    filters$emiscodecategoryid$defaults,
    filters$emiscodecategoryid$values
  )
  expect_null(filters$emiscodecategoryid$value_labels)
  expect_equal(tbl$observations, c(10L, 0L, 5L))
  # Terms keep their quotes, and blanks are NA
  expect_equal(tbl$term[[2]], "Dummy term \"B\"")
  expect_true(is.na(tbl$cleansedreadcode[[3]]))
  # No stray carriage returns from the CRLF line endings
  expect_false(any(grepl("\r", unlist(tbl), fixed = TRUE)))
})

test_that("read_cprd_aurum() builds the prodcode lookup with column filters", {
  dir <- create_dummy_cprd_aurum_dir()
  lkp <- suppressMessages(read_cprd_aurum(dir))$cprd_aurum_prodcode_lkp$lookup

  expect_equal(lkp$metadata$code_type, "CPRD Aurum prodcode")
  expect_equal(lkp$metadata$lookup_code_col, "prodcodeid")
  expect_equal(lkp$metadata$lookup_description_col, "term_from_emis")

  tbl <- lkp$table
  expect_true(all(
    c(
      "prodcodeid",
      "dmdid",
      "term_from_emis",
      "routeofadministration",
      "formulation"
    ) %in%
      names(tbl)
  ))
  expect_type(tbl$drugissues, "integer")

  # Blank route and formulation are empty strings, not NA, so they can be
  # filter values; blanks elsewhere are NA
  expect_equal(tbl$routeofadministration[[3]], "")
  expect_equal(tbl$formulation[[3]], "")
  expect_true(is.na(tbl$dmdid[[3]]))
  expect_true(is.na(tbl$drugsubstancename[[3]]))

  filters <- deserialise_col_filters(lkp$metadata$col_filters)
  expect_named(filters, c("routeofadministration", "formulation"))
  expect_equal(
    filters$routeofadministration$values,
    c("", "Dummy route 1", "Dummy route 1/Dummy route 2")
  )
  # Every value, including the empty string, is a default
  expect_equal(
    filters$routeofadministration$defaults,
    filters$routeofadministration$values
  )
  expect_equal(filters$formulation$defaults, filters$formulation$values)
  expect_true("" %in% filters$formulation$defaults)
})

test_that("read_cprd_aurum() builds the medcode to SNOMED CT mapping", {
  dir <- create_dummy_cprd_aurum_dir()
  map <- suppressMessages(read_cprd_aurum(
    dir
  ))$cprd_aurum_medcode_snomed$mapping

  expect_equal(map$metadata$from_code_type, "CPRD Aurum medcode")
  expect_equal(map$metadata$to_code_type, "SNOMED CT")
  expect_equal(map$metadata$from_col, "medcodeid")
  expect_equal(map$metadata$to_col, "snomedctconceptid")
  expect_equal(
    names(map$table),
    c("medcodeid", "snomedctconceptid", "snomedctdescriptionid")
  )
  expect_equal(nrow(map$table), 3L)
})

test_that("read_cprd_aurum() maps medcodes to Read v2 from the cleansed code", {
  dir <- create_dummy_cprd_aurum_dir()
  map <- suppressMessages(read_cprd_aurum(dir))$cprd_aurum_medcode_read2$mapping

  expect_equal(map$metadata$to_code_type, "Read v2")
  expect_equal(map$metadata$to_col, "read2_code")

  # The medcode without a cleansed Read code is dropped
  expect_equal(map$table$medcodeid, c("MED001", "MED002"))
  expect_equal(map$table$read2_code, c("DUM01", "DUM01"))
  expect_equal(map$table$read2_term_code, c("00", "11"))
})

test_that("read_cprd_aurum() maps prodcodes to SNOMED CT via dmdid", {
  dir <- create_dummy_cprd_aurum_dir()
  map <- suppressMessages(read_cprd_aurum(
    dir
  ))$cprd_aurum_prodcode_snomed$mapping

  expect_equal(map$metadata$from_code_type, "CPRD Aurum prodcode")
  expect_equal(map$metadata$to_code_type, "SNOMED CT")
  expect_equal(map$metadata$to_col, "dmdid")
  # The prodcode without a dmdid is dropped
  expect_equal(map$table$prodcodeid, c("PROD001", "PROD002"))
  expect_equal(map$table$dmdid, c("DMD001", "DMD002"))
})

test_that("read_cprd_aurum() errors when an expected column is missing", {
  dir <- create_dummy_cprd_aurum_dir()
  file <- file.path(dir, "CPRDAurumMedical.txt")
  lines <- readLines(file)
  writeLines(sub("\tTerm\t", "\tTermRenamed\t", lines), file)

  expect_error(
    suppressMessages(read_cprd_aurum(dir, tables = "cprd_aurum_medcode_lkp")),
    "missing expected column"
  )
})

test_that("read_cprd_aurum() reads unexpected columns as character, with a warning", {
  dir <- create_dummy_cprd_aurum_dir()
  file <- file.path(dir, "CPRDAurumProduct.txt")
  lines <- readLines(file)
  lines <- paste0(lines, "\t", c("NewColumn", "001", "002", "003"))
  writeLines(lines, file)

  expect_warning(
    result <- suppressMessages(
      read_cprd_aurum(dir, tables = "cprd_aurum_prodcode_lkp")
    ),
    "unexpected column"
  )
  tbl <- result$cprd_aurum_prodcode_lkp$lookup$table
  expect_equal(tbl$newcolumn, c("001", "002", "003"))
})

test_that("read_cprd_aurum() matches column names ignoring case", {
  dir <- create_dummy_cprd_aurum_dir()
  file <- file.path(dir, "CPRDAurumMedical.txt")
  lines <- readLines(file)
  lines[[1]] <- toupper(lines[[1]])
  writeLines(lines, file)

  result <- suppressMessages(
    read_cprd_aurum(dir, tables = "cprd_aurum_medcode_lkp")
  )
  expect_type(
    result$cprd_aurum_medcode_lkp$lookup$table$observations,
    "integer"
  )
})
