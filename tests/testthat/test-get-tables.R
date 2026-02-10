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
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_true(nrow(result) > 0)
  # Should have more columns than just the standard ones
  expect_true(ncol(result) > 3)
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
  result <- get_mapping_table("Read 3", "ICD-10")
  expect_s3_class(result, "tbl_lazy")
})

test_that("get_mapping_table() returns standardised columns plus extras", {
  result <- get_mapping_table("Read 3", "ICD-10") |> dplyr::collect()
  expect_true(all(c("from", "to") %in% names(result)))
  expect_true(nrow(result) > 0)
  # Should have more columns than just from/to
  expect_true(ncol(result) > 2)
})

test_that("get_mapping_table() respects explicit version", {
  result <- get_mapping_table(
    "Read 3",
    "ICD-10",
    map_version = "UKB v4"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() resolves 'latest' version", {
  result <- get_mapping_table(
    "Read 3",
    "ICD-10",
    map_version = "latest"
  ) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() accepts explicit con", {
  con <- connect_to_db()
  result <- get_mapping_table("Read 3", "ICD-10", con = con) |>
    dplyr::collect()
  expect_true(nrow(result) > 0)
})

test_that("get_mapping_table() errors for unknown code types", {
  expect_error(get_mapping_table("nonexistent", "ICD-10"))
})

test_that("get_mapping_table() col_filters = NULL returns unfiltered", {
  filtered <- get_mapping_table("Read 3", "ICD-10") |> dplyr::collect()
  unfiltered <- get_mapping_table("Read 3", "ICD-10", col_filters = NULL) |>
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

# Internal callers still work ------------------------------------------------

test_that("CODES() still works after get_lookup_table() refactor", {
  result <- CODES("E10", "E11", type = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(nrow(result), 2)
})

test_that("MAP() still works after get_mapping_table() refactor", {
  result <- MAP("X40J4", from = "Read 3", to = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_true(nrow(result) > 0)
})

test_that("CHILDREN() still works after get_relationship_table() addition", {
  result <- CHILDREN("E10", type = "ICD-10")
  expect_s3_class(result, "codeminer_codelist")
  expect_true(nrow(result) > 0)
})
