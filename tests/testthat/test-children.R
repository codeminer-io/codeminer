create_dummy_database()

test_that("CHILDREN() returns the expected data format", {
  test_codes <- c("E10", "E11")
  test_type <- "icd10"

  result <- CHILDREN(test_codes, code_type = test_type, lookup_version = "v0")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_true(nrow(result) >= length(test_codes))
  expect_identical(unique(result$code_type), test_type)
})

test_that("CHILDREN() works with codes_only = TRUE", {
  test_codes <- c("E10", "E11")
  test_type <- "icd10"

  result <- CHILDREN(test_codes, code_type = test_type, codes_only = TRUE)

  expect_type(result, "character")
  expect_true(length(result) >= length(test_codes))
})

test_that("CHILDREN() returns all hierarchy descendants and ignores non-is_a relationships", {
  test_type <- "dummy"

  dummy_lookup <- data.frame(
    code = c("parent", "child1", "child2", "grandchild", "notachild"),
    description = c(
      "a parent",
      "a child",
      "another child",
      "a grandchild",
      "not a child"
    )
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("child1", "child2", "grandchild", "notachild"),
    to = c("parent", "parent", "child1", "parent"),
    type = c("is a", "is a", "is a", "has a")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  result <- CHILDREN("parent", code_type = "dummy", codes_only = TRUE)

  expect_identical(
    sort(result),
    c("child1", "child2", "grandchild", "parent")
  )
})


test_that("CHILDREN() works with the codeminer.code_type option", {
  test_codes <- c("E10", "E11")
  test_type <- "icd10"

  result <- withr::with_options(
    list(codeminer.code_type = test_type),
    CHILDREN(test_codes)
  )

  expect_s3_class(result, "data.frame")
  expect_identical(unique(result$code_type), test_type)
})

test_that("CHILDREN() handles versions correctly", {
  test_codes <- c("E10")
  test_type <- "icd10"
  test_version <- "v0"

  result_v0 <- CHILDREN(
    test_codes,
    code_type = test_type,
    lookup_version = test_version
  )
  expect_s3_class(result_v0, "data.frame")

  result_latest <- CHILDREN(
    test_codes,
    code_type = test_type,
    lookup_version = "latest"
  )
  expect_s3_class(result_latest, "data.frame")
  expect_equal(result_latest, result_v0)
})

test_that("CHILDREN() uses correct latest version", {
  test_type <- "icd10"
  test_version <- "v2"

  # Add test relationship table as new version
  test_relationship_table <- data.frame(
    from = c("A", "B", "C"),
    to = c("PARENT_A", "PARENT_B", "PARENT_C"),
    type = c("is a", "is a", "is a")
  )

  # Add corresponding lookup table for the new codes
  test_lookup_table <- data.frame(
    code = c("A", "B", "C", "PARENT_A", "PARENT_B", "PARENT_C"),
    description = c(
      "Code A",
      "Code B",
      "Code C",
      "Parent A",
      "Parent B",
      "Parent C"
    )
  )

  add_lookup_table(test_lookup_table, lookup_metadata(test_type, test_version))
  add_relationship_table(
    test_relationship_table,
    relationship_metadata(test_type, test_version)
  )

  v2_result <- CHILDREN(
    "PARENT_A",
    code_type = test_type,
    lookup_version = test_version,
    codes_only = TRUE
  )
  expect_identical(v2_result, c("A", "PARENT_A"))

  latest_result <- CHILDREN(
    "PARENT_A",
    code_type = test_type,
    lookup_version = "latest",
    codes_only = TRUE
  )
  expect_identical(latest_result, v2_result)
})

test_that("CHILDREN() fails for wrong argument types", {
  expect_error(
    CHILDREN("E10", code_type = c("icd10", "icd11")),
    "`code_type` must be a string"
  )

  expect_error(
    CHILDREN(123, code_type = "icd10"),
    "`codes` must be a character vector"
  )
})

test_that("CHILDREN() fails for missing code_type", {
  expect_error(
    CHILDREN("E10", code_type = "idontexist"),
    "Code type 'idontexist' not found in relationship metadata"
  )
})

test_that("CHILDREN() fails for wrong version", {
  expect_error(
    CHILDREN("E10", code_type = "icd10", relationship_version = "nope"),
    "No relationship metadata found"
  )
})

test_that("CHILDREN() warns about missing codes", {
  test_codes <- c("foo", "bar")
  expect_warning(
    with_mocked_bindings(
      CHILDREN(test_codes, "icd10"),

      # 2 codeminer_missing_codes warnings are raised, one for CHILDREN() and one for
      # CODES(). Here we are only testing CHILDREN(), so CODES() is mocked
      CODES = function(...) invisible()
    ),
    class = "codeminer_missing_codes"
  )
})

test_that("CHILDREN() returns empty result for invalid codes", {
  test_codes <- c("nonexistent1", "nonexistent2")

  suppressMessages(suppressWarnings(
    result_df <- CHILDREN(test_codes, "icd10", codes_only = FALSE)
  ))

  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 0)

  suppressMessages(suppressWarnings(
    result_vec <- CHILDREN(test_codes, "icd10", codes_only = TRUE)
  ))
  expect_type(result_vec, "character")
  expect_equal(length(result_vec), 0)
})

test_that("CHILDREN() handles empty input", {
  expect_warning(result <- CHILDREN(character(0), "icd10"))
  expect_equal(nrow(result), 0)
})

test_that("get_metadata_for_relationship() works correctly", {
  con <- connect_to_db()

  # Test valid code_type and version
  meta <- get_metadata_for_relationship(con, "icd10", "v0")
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 1)
  expect_true(all(
    c("code_type", "relationship_version", "relationship_table_name") %in%
      names(meta)
  ))

  # Test latest version
  meta_latest <- get_metadata_for_relationship(con, "icd10", "latest")
  expect_s3_class(meta_latest, "data.frame")
  expect_equal(nrow(meta_latest), 1)
})
