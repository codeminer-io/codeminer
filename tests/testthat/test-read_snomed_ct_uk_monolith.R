# Helper tests -----------------------------------------------------------

test_that("find_snomed_file selects correct file", {
  tmp <- withr::local_tempdir()
  fs::file_create(file.path(tmp, "sct2_Concept_Snapshot_2024.txt"))
  fs::file_create(file.path(tmp, "sct2_Other_File.txt"))

  result <- find_snomed_file(tmp, "^sct2_Concept_")
  expect_match(result, "sct2_Concept_Snapshot_2024.txt")
})

test_that("find_snomed_file errors on missing file", {
  tmp <- withr::local_tempdir()
  expect_error(find_snomed_file(tmp, "MissingFile"), "Could not find file")
})

test_that("snomed_attach_category derives category from active FSN", {
  tbl <- data.frame(
    conceptId = c("1", "1", "2", "2"),
    typeId_description = c(
      "900000000000003001", # FSN
      "900000000000013009", # synonym
      "900000000000003001",
      "900000000000013009"
    ),
    term_description = c(
      "Hypertensive disorder (disorder)",
      "Hypertensive disorder",
      "Has active ingredient (attribute)",
      "Has active ingredient"
    ),
    active_description = c("1", "1", "1", "1"),
    active_concept = c("1", "1", "1", "1"),
    stringsAsFactors = FALSE
  )

  out <- snomed_attach_category(tbl)

  expect_equal(
    out$category,
    c("Disorder", "Disorder", "Attribute", "Attribute")
  )
})

test_that("snomed_attach_category prefixes the extracted category with `(Inactive)` for retired concepts", {
  tbl <- data.frame(
    conceptId = c("1", "2"),
    typeId_description = c("900000000000003001", "900000000000003001"),
    term_description = c(
      "Retired disorder (disorder)",
      "Active disorder (disorder)"
    ),
    active_description = c("0", "1"),
    active_concept = c("0", "1"),
    stringsAsFactors = FALSE
  )

  out <- snomed_attach_category(tbl)

  expect_equal(out$category[out$conceptId == "1"], "(Inactive) Disorder")
  expect_equal(out$category[out$conceptId == "2"], "Disorder")
})

test_that("snomed_attach_category uses bare `(Inactive)` when the retired FSN has no parenthetical", {
  tbl <- data.frame(
    conceptId = "1",
    typeId_description = "900000000000003001",
    term_description = "Retired with no parenthetical",
    active_description = "0",
    active_concept = "0",
    stringsAsFactors = FALSE
  )

  out <- snomed_attach_category(tbl)

  expect_equal(out$category, "(Inactive)")
})

test_that("snomed_attach_category returns NA for FSNs with no parenthetical", {
  tbl <- data.frame(
    conceptId = "1",
    typeId_description = "900000000000003001",
    term_description = "Hypertensive disorder",
    active_description = "1",
    active_concept = "1",
    stringsAsFactors = FALSE
  )

  out <- snomed_attach_category(tbl)

  expect_true(is.na(out$category))
})

# Main function tests -----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() returns all tables by default", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(dummy_snomed_ct_uk_monolith_path())
  )

  expect_equal(
    names(result),
    c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4")
  )
})

test_that("read_snomed_ct_uk_monolith() returns correct structure", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup"
    )
  )

  # Check nested structure
  expect_true("lookup" %in% names(result$sct_lookup))
  expect_true("table" %in% names(result$sct_lookup$lookup))
  expect_true("metadata" %in% names(result$sct_lookup$lookup))

  # Check table is data frame
  expect_s3_class(result$sct_lookup$lookup$table, "data.frame")

  # Check metadata has expected fields
  expect_equal(result$sct_lookup$lookup$metadata$code_type, "SNOMED CT")
})

test_that("read_snomed_ct_uk_monolith() tables argument works", {
  # Request only lookup and ICD-10
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = c("sct_lookup", "sct_icd10")
    )
  )

  expect_equal(names(result), c("sct_lookup", "sct_icd10"))
  expect_false("sct_relationship" %in% names(result))
  expect_false("sct_opcs4" %in% names(result))
})

test_that("read_snomed_ct_uk_monolith() attaches FSN-derived category to lookup", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup"
    )
  )

  tbl <- result$sct_lookup$lookup$table
  meta <- result$sct_lookup$lookup$metadata

  expect_true("category" %in% names(tbl))
  expect_equal(meta$lookup_category_col, "category")

  # Both the FSN and synonym rows for MS should carry the FSN-derived category
  ms_rows <- tbl[tbl$conceptId == "24700007", ]
  expect_true(nrow(ms_rows) >= 1)
  expect_true(all(ms_rows$category == "Disorder"))
})

test_that("read_snomed_ct_uk_monolith() declares moduleId_concept as a no-default filter", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup"
    )
  )

  tbl <- result$sct_lookup$lookup$table
  cf <- deserialise_col_filters(
    result$sct_lookup$lookup$metadata$col_filters
  )

  # moduleId_concept is declared as a filterable column...
  expect_true("moduleId_concept" %in% names(cf))
  # ...with its `values` derived from the data (every module present)...
  expect_setequal(
    cf$moduleId_concept$values,
    sort(unique(tbl$moduleId_concept[!is.na(tbl$moduleId_concept)]))
  )
  # ...and empty `defaults`, so the default "SNOMED CT" query is not filtered
  # by module and still returns the full UK release.
  expect_length(cf$moduleId_concept$defaults, 0)

  # ...carrying a self-documenting description. `value_labels` are derived from
  # the modules' own FSNs, so names are always a subset of `values` (the GP
  # subset fixture has no module concept rows, so the set may be empty here —
  # see the snomed_module_labels() unit test for the derivation itself).
  expect_match(cf$moduleId_concept$description, "^SNOMED CT module")
  labels <- cf$moduleId_concept$value_labels
  expect_true(
    is.null(labels) || all(names(labels) %in% cf$moduleId_concept$values)
  )

  resolved <- resolve_col_filters(
    result$sct_lookup$lookup$metadata$col_filters,
    pin_type = "lookup",
    pin_key = "SNOMED CT"
  )
  expect_false("moduleId_concept" %in% names(resolved))
})

test_that("snomed_module_labels() derives labels from active FSNs, tag stripped", {
  tbl <- tibble::tribble(
    ~conceptId                                                   ,
    ~typeId_description                                          ,
    ~active_description                                          ,
    ~term_description                                            ,
    # Module: active FSN (used) plus a synonym (ignored).
    "999000011000001104"                                         ,
    "900000000000003001"                                         ,
    "1"                                                          ,
    "SNOMED CT UK drug extension module (core metadata concept)" ,
    "999000011000001104"                                         ,
    "900000000000013009"                                         ,
    "1"                                                          ,
    "dm+d module synonym"                                        ,
    # Module: only an inactive FSN -> left unlabelled.
    "900000000000207008"                                         ,
    "900000000000003001"                                         ,
    "0"                                                          ,
    "SNOMED CT core module (core metadata concept)"              ,
    # Non-module concept -> excluded.
    "22298006"                                                   ,
    "900000000000003001"                                         ,
    "1"                                                          ,
    "Myocardial infarction (disorder)"
  )

  labs <- snomed_module_labels(
    tbl,
    module_ids = c("999000011000001104", "900000000000207008")
  )

  # Active FSN, trailing "(...)" tag stripped; synonym and non-module ignored.
  expect_identical(
    labs[["999000011000001104"]],
    "SNOMED CT UK drug extension module"
  )
  # Module with only an inactive FSN is omitted, so names stay a subset of ids.
  expect_false("900000000000207008" %in% names(labs))
  expect_true(all(
    names(labs) %in% c("999000011000001104", "900000000000207008")
  ))
})

test_that("read_snomed_ct_uk_monolith() filters mappings correctly", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = c("sct_icd10", "sct_opcs4")
    )
  )

  # Check ICD-10 table has correct refset and no blocks
  icd10_tbl <- result$sct_icd10$mapping$table
  expect_true(all(icd10_tbl$refsetId == "999002271000000101"))
  expect_false(any(grepl("#", icd10_tbl$mapTarget)))

  # Check dagger/asterisk stripped from mapTarget into separate column
  expect_true("icd10_dagger_asterisk" %in% names(icd10_tbl))
  expect_false(any(grepl("[AD]$", icd10_tbl$mapTarget)))

  # X placeholder for 3-char categories also stripped (e.g. J46X -> J46), so
  # mapTarget joins cleanly to the ICD-10 lookup
  expect_false(any(grepl("^[A-Z][0-9]{2}X$", icd10_tbl$mapTarget)))

  # MS (24700007) should have dagger flag stripped
  ms_row <- icd10_tbl[icd10_tbl$referencedComponentId == "24700007", ]
  expect_equal(ms_row$mapTarget, "G35")
  expect_equal(ms_row$icd10_dagger_asterisk, "D")

  # Optic neuritis (66760008) should have asterisk flag stripped
  on_row <- icd10_tbl[icd10_tbl$referencedComponentId == "66760008", ]
  expect_equal(on_row$mapTarget, "H46")
  expect_equal(on_row$icd10_dagger_asterisk, "A")

  # Chronic pharyngitis (140004) should have no flag
  cp_row <- icd10_tbl[icd10_tbl$referencedComponentId == "140004", ]
  expect_equal(cp_row$mapTarget, "J312")
  expect_true(is.na(cp_row$icd10_dagger_asterisk))

  # Check OPCS-4 table has correct refset and no blocks. The refset id is
  # auto-detected from the description terms (default `.opcs4_refset_id = NULL`);
  # the dummy release describes refset 999002321000000109 as an OPCS-4 complex
  # map, so detection resolves back to it.
  expect_true(all(
    result$sct_opcs4$mapping$table$refsetId == "999002321000000109"
  ))
  expect_false(any(grepl("#", result$sct_opcs4$mapping$table$mapTarget)))
})

test_that("detect_opcs4_refset_id() picks the highest OPCS-4 version", {
  desc_file <- withr::local_tempfile(fileext = ".txt")
  opcs4_term <- function(v) {
    paste0(
      "Office of Population Censuses and Surveys Classification of ",
      "Interventions and Procedures Version 4.",
      v,
      " complex map reference set"
    )
  }
  desc <- data.frame(
    id = c("1", "2", "3"),
    effectiveTime = "20260101",
    active = "1",
    moduleId = "m",
    conceptId = c("1126441000000105", "1382401000000109", "1891651000000103"),
    languageCode = "en",
    typeId = "t",
    term = vapply(c(9L, 10L, 11L), opcs4_term, character(1)),
    caseSignificanceId = "c",
    stringsAsFactors = FALSE
  )
  utils::write.table(
    desc,
    desc_file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  # 4.11 > 4.10 > 4.9, despite 4.9 sorting last lexically among the ids
  candidates <- c("1126441000000105", "1382401000000109", "1891651000000103")
  expect_equal(
    detect_opcs4_refset_id(desc_file, candidates),
    "1891651000000103"
  )

  # Only loadable refsets are candidates: drop 4.11 and we fall back to 4.10
  expect_equal(
    detect_opcs4_refset_id(
      desc_file,
      c("1126441000000105", "1382401000000109")
    ),
    "1382401000000109"
  )

  # No OPCS-4 refset present -> clear error
  expect_error(
    detect_opcs4_refset_id(desc_file, "999999999999999999"),
    "Could not auto-detect an OPCS-4 map reference set"
  )
})

test_that("read_snomed_ct_uk_monolith() honours an explicit .opcs4_refset_id", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_opcs4",
      .opcs4_refset_id = "999002321000000109"
    )
  )

  expect_gt(nrow(result$sct_opcs4$mapping$table), 0)
  expect_true(all(
    result$sct_opcs4$mapping$table$refsetId == "999002321000000109"
  ))
})

# Error handling tests ----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() errors on non-existent path", {
  expect_error(
    read_snomed_ct_uk_monolith("/nonexistent/path"),
    "Path does not exist"
  )
})

test_that("read_snomed_ct_uk_monolith() errors on missing subdirectories", {
  tmp <- withr::local_tempdir()

  # Create Snapshot dir but missing Terminology
  fs::dir_create(file.path(tmp, "Snapshot"))

  expect_error(
    suppressMessages(read_snomed_ct_uk_monolith(tmp)),
    "required subdirectories are missing"
  )
})

# Zip file input tests ----------------------------------------------------

test_that("read_snomed_ct_uk_monolith() accepts zip file input", {
  # Get the package zip file directly
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(
    read_snomed_ct_uk_monolith(zip_path, tables = "sct_lookup")
  )

  expect_true("sct_lookup" %in% names(result))
  expect_s3_class(result$sct_lookup$lookup$table, "data.frame")
  expect_gt(nrow(result$sct_lookup$lookup$table), 0)
})

test_that("read_snomed_ct_uk_monolith() derives version from zip filename", {
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(
    read_snomed_ct_uk_monolith(zip_path, tables = "sct_lookup")
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_version,
    "snomed_gps.zip"
  )
})

test_that("read_snomed_ct_uk_monolith() extracts all tables from zip", {
  zip_path <- system.file("extdata", "snomed_gps.zip", package = "codeminer")

  result <- suppressMessages(read_snomed_ct_uk_monolith(zip_path))

  expect_equal(
    names(result),
    c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4")
  )

  # Check all tables have data

  expect_gt(nrow(result$sct_lookup$lookup$table), 0)
  expect_gt(nrow(result$sct_relationship$relationship$table), 0)
  expect_gt(nrow(result$sct_icd10$mapping$table), 0)
  expect_gt(nrow(result$sct_opcs4$mapping$table), 0)
})

# Parameter variation tests -----------------------------------------------

test_that("read_snomed_ct_uk_monolith() uses custom version parameter", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup",
      version = "CUSTOM_v1.0"
    )
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_version,
    "CUSTOM_v1.0"
  )
})

test_that("read_snomed_ct_uk_monolith() uses custom source parameter", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup",
      source = "https://custom.source.org/"
    )
  )

  expect_equal(
    result$sct_lookup$lookup$metadata$lookup_source,
    "https://custom.source.org/"
  )
})

test_that("read_snomed_ct_uk_monolith() defaults relationships to active edges only", {
  result <- suppressMessages(
    read_snomed_ct_uk_monolith(
      dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_relationship"
    )
  )

  cf <- deserialise_col_filters(
    result$sct_relationship$relationship$metadata$col_filters
  )

  expect_equal(cf$active$values, c("0", "1"))
  expect_equal(cf$active$defaults, "1")
})

test_that("read_snomed_ct_uk_monolith() errors on an unmatched ICD-10 refset", {
  # A non-matching .icd10_refset_id should fail loudly rather than silently
  # storing an empty mapping table (the guard that was missing for OPCS-4).
  expect_error(
    suppressMessages(
      read_snomed_ct_uk_monolith(
        dummy_snomed_ct_uk_monolith_path(),
        tables = "sct_icd10",
        .icd10_refset_id = "999999999999999999"
      )
    ),
    "No rows found for ICD-10 map refset"
  )
})
