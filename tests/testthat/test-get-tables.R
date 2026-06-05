# test-get-tables.R
# Tests for get_lookup_table(), get_mapping_table(), get_relationship_table()
# (GitHub issue #90)

## Set up dummy database
suppressMessages(create_dummy_database(.local = TRUE))

# get_lookup_table() --------------------------------------------------------

test_that("get_lookup_table() returns a lazy tbl without explicit con", {
  result <- get_lookup_table("ICD-10")
  expect_s3_class(result, "tbl_lazy")
})
test_that("get_lookup_table() returns standardised columns plus extras", {
  result <- get_lookup_table("ICD-10") |> dplyr::collect()
  expect_true(all(
    c(
      "code",
      "description",
      "code_type",
      "preferred_description",
      "category"
    ) %in%
      names(result)
  ))
  expect_true(nrow(result) > 0)
  # Should have more columns than just the standard ones
  expect_true(ncol(result) > 3)
})

test_that("get_lookup_table() tolerates source columns that case-fold onto standardised aliases", {
  # Regression: a source column whose case-insensitive name matches a
  # standardised alias (e.g. TRUD ICD-10 ships both `CODE` and `ALT_CODE`,
  # and `ALT_CODE` is the lookup_code_col so it gets renamed to `code`).
  # DuckDB folds unquoted identifiers, so an outer SELECT projecting both
  # `code` and `CODE` collapses them and tibble blows up. The fix keeps all
  # renames in one innermost SELECT so the outer wrappers stay `q01.*` and
  # the backend auto-disambiguates duplicates with `_1` suffixes.
  collision_tbl <- tibble::tibble(
    CODE = c("X.0", "X.1"),
    ALT_CODE = c("X0", "X1"),
    DESCRIPTION = c("First", "Second"),
    CATEGORY_LABEL = c("Cat A", "Cat B")
  )

  suppressMessages(add_lookup_table(
    collision_tbl,
    lookup_metadata(
      "collision_type",
      lookup_version = "v1",
      lookup_code_col = "ALT_CODE",
      lookup_description_col = "DESCRIPTION",
      lookup_category_col = "CATEGORY_LABEL"
    )
  ))

  result <- get_lookup_table("collision_type") |> dplyr::collect()

  # `code` is the renamed ALT_CODE; the original CODE survives as CODE_1.
  expect_true(all(c("code", "CODE_1", "category") %in% names(result)))
  expect_equal(sort(result$code), c("X0", "X1"))
  expect_equal(sort(result$CODE_1), c("X.0", "X.1"))
  expect_equal(sort(result$category), c("Cat A", "Cat B"))
})

test_that("get_lookup_table() exposes BNF chapter as `category`", {
  result <- get_lookup_table("BNF") |> dplyr::collect()
  expect_true("category" %in% names(result))
  # BNF_Chapter has been renamed to `category` via metadata
  expect_false("BNF_Chapter" %in% names(result))
  expect_true(any(!is.na(result$category)))
})

test_that("get_lookup_table() returns NA category when no source column is mapped", {
  # Read v3 does not have a category source yet — `category` should be all NA
  result <- get_lookup_table("Read v3") |> dplyr::collect()
  expect_true("category" %in% names(result))
  expect_true(all(is.na(result$category)))
})

test_that("get_lookup_table() respects explicit version", {
  result_v4 <- get_lookup_table("ICD-10", lookup_version = "UKB v4") |>
    dplyr::collect()
  expect_true(nrow(result_v4) > 0)
  expect_identical(unique(result_v4$code_type), "ICD-10")
})

test_that("get_lookup_table() resolves 'latest' version", {
  result <- get_lookup_table("ICD-10", lookup_version = "latest") |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_lookup_table() accepts explicit con", {
  con <- connect_to_db()
  result <- get_lookup_table("ICD-10", con = con) |> dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_lookup_table() errors for unknown code type", {
  expect_error(get_lookup_table("nonexistent"))
})

test_that("get_lookup_table() col_filters = NULL returns unfiltered", {
  filtered <- get_lookup_table("ICD-10") |> dplyr::collect()
  unfiltered <- get_lookup_table("ICD-10", col_filters = NULL) |>
    dplyr::collect()
  # Unfiltered should have at least as many rows

  expect_gte(nrow(unfiltered), nrow(filtered))
})

# get_mapping_table() -------------------------------------------------------

test_that("get_mapping_table() returns a lazy tbl without explicit con", {
  result <- get_mapping_table("Read v3", "ICD-10")
  expect_s3_class(result, "tbl_lazy")
})

test_that("get_mapping_table() returns standardised columns plus extras", {
  result <- get_mapping_table("Read v3", "ICD-10") |> dplyr::collect()
  expect_true(all(c("from", "to") %in% names(result)))
  expect_true(nrow(result) > 0)
  # Should have more columns than just from/to
  expect_true(ncol(result) > 2)
})

test_that("get_mapping_table() respects explicit version", {
  result <- get_mapping_table(
    "Read v3",
    "ICD-10",
    map_version = "UKB v4"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() resolves 'latest' version", {
  result <- get_mapping_table(
    "Read v3",
    "ICD-10",
    map_version = "latest"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() accepts explicit con", {
  con <- connect_to_db()
  result <- get_mapping_table("Read v3", "ICD-10", con = con) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() errors for unknown code types", {
  expect_error(get_mapping_table("nonexistent", "ICD-10"))
})

test_that("get_mapping_table() col_filters = NULL returns unfiltered", {
  filtered <- get_mapping_table("Read v3", "ICD-10") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read v3", "ICD-10", col_filters = NULL) |>
    dplyr::collect()
  expect_gte(nrow(unfiltered), nrow(filtered))
})

# get_relationship_table() --------------------------------------------------

test_that("get_relationship_table() returns a lazy tbl without explicit con", {
  result <- get_relationship_table("ICD-10")
  expect_s3_class(result, "tbl_lazy")
})

test_that("get_relationship_table() returns standardised columns plus extras", {
  result <- get_relationship_table("ICD-10") |> dplyr::collect()
  expect_true(all(c("from", "to", "type", "code_type") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_identical(unique(result$code_type), "ICD-10")
  # At minimum the 4 standardised columns
  expect_true(ncol(result) >= 4)
})

test_that("get_relationship_table() respects explicit version", {
  result <- get_relationship_table(
    "ICD-10",
    relationship_version = "UKB v4"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_relationship_table() resolves 'latest' version", {
  result <- get_relationship_table(
    "ICD-10",
    relationship_version = "latest"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_relationship_table() accepts explicit con", {
  con <- connect_to_db()
  result <- get_relationship_table("ICD-10", con = con) |> dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_relationship_table() errors for unknown code type", {
  expect_error(get_relationship_table("nonexistent"))
})

test_that("get_relationship_table() col_filters = NULL unfiltered", {
  filtered <- get_relationship_table("ICD-10") |> dplyr::collect()
  unfiltered <- get_relationship_table("ICD-10", col_filters = NULL) |>
    dplyr::collect()
  expect_gte(nrow(unfiltered), nrow(filtered))
})

# col_filters stored during ingestion -----------------------------------------

test_that("SNOMED CT lookup has active_description col_filter", {
  # Add SNOMED CT data to the dummy database
  suppressMessages(
    add_snomed_ct_uk_monolith(
      path = dummy_snomed_ct_uk_monolith_path(),
      tables = "sct_lookup"
    )
  )

  # Verify col_filters metadata is stored
  con <- connect_to_db()
  meta <- get_codeminer_metadata("lookup", con = con) |>
    dplyr::filter(code_type == "SNOMED CT") |>
    dplyr::collect()
  expect_false(is.na(meta$col_filters))

  # Verify get_lookup_table works with default col_filters
  result <- get_lookup_table("SNOMED CT") |> dplyr::collect()
  expect_true("active_description" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("Read 2 -> ICD-10 col_filter excludes icd10_code_def '2'", {
  default <- get_mapping_table("Read v2", "ICD-10") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read v2", "ICD-10", col_filters = NULL) |>
    dplyr::collect()
  # Dummy data has icd10_code_def == 2 rows that should be excluded by default
  expect_gt(nrow(unfiltered), nrow(default))
  expect_false("2" %in% as.character(default$icd10_code_def))
  expect_true("2" %in% as.character(unfiltered$icd10_code_def))
})

test_that("Read 2 -> Read 3 col_filter keeps only IS_ASSURED == 1", {
  default <- get_mapping_table("Read v2", "Read v3") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read v2", "Read v3", col_filters = NULL) |>
    dplyr::collect()
  expect_gt(nrow(unfiltered), nrow(default))
  expect_true(all(as.character(default$IS_ASSURED) == "1"))
})

test_that("Read 3 -> ICD-10 col_filter applies mapping_status filter", {
  default <- get_mapping_table("Read v3", "ICD-10") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read v3", "ICD-10", col_filters = NULL) |>
    dplyr::collect()
  expect_gt(nrow(unfiltered), nrow(default))
  # Default keeps only E, G, D
  expect_true(all(default$mapping_status %in% c("E", "G", "D")))
})

test_that("Read 3 -> Read 2 col_filter keeps only IS_ASSURED == 1", {
  default <- get_mapping_table("Read v3", "Read v2") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read v3", "Read v2", col_filters = NULL) |>
    dplyr::collect()
  expect_gt(nrow(unfiltered), nrow(default))
  expect_true(all(as.character(default$IS_ASSURED) == "1"))
})

# Internal callers still work ------------------------------------------------

test_that("CODES() still works after get_lookup_table() refactor", {
  result <- CODES("E10", "E11", type = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(nrow(result), 2)
})

test_that("MAP() still works after get_mapping_table() refactor", {
  result <- MAP("X40J4", from = "Read v3", to = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_true(nrow(result) > 0)
})

test_that("CHILDREN() still works after get_relationship_table() addition", {
  result <- CHILDREN("E10", type = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_true(nrow(result) > 0)
})
