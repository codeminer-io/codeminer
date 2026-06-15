suppressMessages(create_dummy_database(.local = TRUE))

test_that("CHILDREN() and PARENTS() return the expected data format", {
  test_codes <- c("E10", "E11")
  test_type <- "ICD-10"

  children_result <- CHILDREN(
    test_codes,
    type = test_type,
    lookup_version = "UKB v4"
  )
  expect_s3_class(children_result, "data.frame")
  expect_true(all(
    c("code", "description", "code_type") %in% names(children_result)
  ))
  expect_true(nrow(children_result) >= length(test_codes))
  expect_identical(unique(children_result$code_type), test_type)

  parents_result <- PARENTS(
    test_codes,
    type = test_type,
    lookup_version = "UKB v4"
  )
  expect_s3_class(parents_result, "data.frame")
  expect_true(all(
    c("code", "description", "code_type") %in% names(parents_result)
  ))
  expect_true(nrow(parents_result) >= length(test_codes))
  expect_identical(unique(parents_result$code_type), test_type)
})

test_that("CHILDREN() and PARENTS() work with code extraction", {
  test_codes <- c("E10", "E11")
  test_type <- "ICD-10"

  children_result <- CHILDREN(
    test_codes,
    type = test_type
  )
  expect_type(children_result$code, "character")
  expect_true(length(children_result$code) >= length(test_codes))

  parents_result <- PARENTS(
    test_codes,
    type = test_type
  )
  expect_type(parents_result$code, "character")
  expect_true(length(parents_result$code) >= length(test_codes))
})

test_that("CHILDREN() and PARENTS() return all hierarchy descendants/ancestors and ignore non-is_a relationships", {
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

  children_result <- CHILDREN("parent", type = "dummy")
  expect_identical(
    sort(children_result$code),
    c("child1", "child2", "grandchild", "parent")
  )

  parents_result <- PARENTS(
    "grandchild",
    type = "dummy"
  )
  expect_identical(
    sort(parents_result$code),
    c("child1", "grandchild", "parent")
  )
})

test_that("N_CHILDREN() and N_PARENTS() respect depth parameter", {
  test_type <- "dummy2"

  dummy_lookup <- data.frame(
    code = c("root", "level1", "level2", "level3"),
    description = c("Root", "Level 1", "Level 2", "Level 3")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("level1", "level2", "level3"),
    to = c("root", "level1", "level2"),
    type = c("is a", "is a", "is a")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # depth = 1 should return only immediate children
  children_1 <- N_CHILDREN(
    "root",
    depth = 1,
    type = test_type
  )
  expect_identical(sort(children_1$code), c("level1", "root"))

  # depth = 2 should return up to 2 levels
  children_2 <- N_CHILDREN(
    "root",
    depth = 2,
    type = test_type
  )
  expect_identical(sort(children_2$code), c("level1", "level2", "root"))

  # depth = Inf should return all
  children_all <- N_CHILDREN(
    "root",
    depth = Inf,
    type = test_type
  )
  expect_identical(
    sort(children_all$code),
    c("level1", "level2", "level3", "root")
  )

  # Test parents with depth
  parents_1 <- N_PARENTS(
    "level3",
    depth = 1,
    type = test_type
  )
  expect_identical(sort(parents_1$code), c("level2", "level3"))

  parents_2 <- N_PARENTS(
    "level3",
    depth = 2,
    type = test_type
  )
  expect_identical(sort(parents_2$code), c("level1", "level2", "level3"))

  parents_all <- N_PARENTS(
    "level3",
    depth = Inf,
    type = test_type
  )
  expect_identical(
    sort(parents_all$code),
    c("level1", "level2", "level3", "root")
  )
})

test_that("CHILDREN() and N_CHILDREN() with depth=Inf return same results", {
  test_codes <- c("E10")
  test_type <- "ICD-10"

  children_inf <- CHILDREN(test_codes, type = test_type)
  n_children_inf <- N_CHILDREN(
    test_codes,
    depth = Inf,
    type = test_type
  )

  expect_identical(children_inf$code, n_children_inf$code)
})

test_that("PARENTS() and N_PARENTS() with depth=Inf return same results", {
  test_codes <- c("E10")
  test_type <- "ICD-10"

  parents_inf <- PARENTS(test_codes, type = test_type)
  n_parents_inf <- N_PARENTS(
    test_codes,
    depth = Inf,
    type = test_type
  )

  expect_identical(parents_inf$code, n_parents_inf$code)
})

test_that("CHILDREN() works with the codeminer.code_type option", {
  test_codes <- c("E10", "E11")
  test_type <- "ICD-10"

  result <- withr::with_options(
    list(codeminer.code_type = test_type),
    CHILDREN(test_codes)
  )

  expect_s3_class(result, "data.frame")
  expect_identical(unique(result$code_type), test_type)
})

test_that("CHILDREN() handles versions correctly", {
  test_codes <- c("E10")
  test_type <- "ICD-10"
  test_version <- "UKB v4"

  result_v0 <- CHILDREN(
    test_codes,
    type = test_type,
    lookup_version = test_version
  )
  expect_s3_class(result_v0, "data.frame")

  result_latest <- CHILDREN(
    test_codes,
    type = test_type,
    lookup_version = "latest"
  )
  expect_s3_class(result_latest, "data.frame")
  expect_equal(result_latest, result_v0)
})

test_that("CHILDREN() uses correct latest version", {
  test_type <- "ICD-10"
  test_version <- "UKB v5"

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
  # Clear cached "latest" so it re-resolves to the newly added versions
  codeminer_clear_versions(
    lookup = test_type,
    relationship = test_type
  )

  v2_result <- CHILDREN(
    "PARENT_A",
    type = test_type,
    lookup_version = test_version
  )
  expect_identical(v2_result$code, c("A", "PARENT_A"))

  latest_result <- CHILDREN(
    "PARENT_A",
    type = test_type,
    lookup_version = "latest"
  )
  expect_identical(latest_result$code, v2_result$code)
})

test_that("CHILDREN() fails for wrong argument types", {
  expect_error(
    CHILDREN("E10", type = c("ICD-10", "icd11")),
    "`code_type` must be a string"
  )

  expect_error(
    CHILDREN(123, type = "ICD-10"),
    "All inputs must be character vectors or a single data frame"
  )
})

test_that("CHILDREN() fails for missing code_type", {
  expect_error(
    CHILDREN("E10", type = "idontexist"),
    "not found in relationship metadata"
  )
})

test_that("CHILDREN() fails for wrong version", {
  expect_error(
    CHILDREN("E10", type = "ICD-10", relationship_version = "nope"),
    "No relationship metadata found"
  )
})

test_that("CHILDREN() warns about missing codes", {
  test_codes <- c("foo", "bar")
  expect_warning(
    with_mocked_bindings(
      CHILDREN(test_codes, type = "ICD-10"),

      # 2 codeminer_missing_codes warnings are raised, one for CHILDREN() and one for
      # CODES(). Here we are only testing CHILDREN(), so CODES() is mocked
      CODES = function(...) invisible()
    ),
    class = "codeminer_missing_codes"
  )
})

test_that("CHILDREN() and PARENTS() return empty result for invalid codes", {
  test_codes <- c("nonexistent1", "nonexistent2")

  suppressMessages(suppressWarnings(
    children_df <- CHILDREN(test_codes, type = "ICD-10")
  ))
  expect_s3_class(children_df, "data.frame")
  expect_equal(nrow(children_df), 0)

  suppressMessages(suppressWarnings(
    children_vec <- CHILDREN(test_codes, type = "ICD-10")
  ))
  expect_type(children_vec$code, "character")
  expect_equal(length(children_vec$code), 0)

  suppressMessages(suppressWarnings(
    parents_df <- PARENTS(test_codes, type = "ICD-10")
  ))
  expect_s3_class(parents_df, "data.frame")
  expect_equal(nrow(parents_df), 0)

  suppressMessages(suppressWarnings(
    parents_vec <- PARENTS(test_codes, type = "ICD-10")
  ))
  expect_type(parents_vec$code, "character")
  expect_equal(length(parents_vec$code), 0)
})

test_that("CHILDREN() handles empty input", {
  expect_warning(result <- CHILDREN(character(0), type = "ICD-10"))
  expect_equal(nrow(result), 0)
})

# Read v3 hierarchy walk (#148) -------------------------------------------

test_that("CHILDREN() walks the full Read v3 hierarchy regardless of relationship_type", {
  # Real CTV3 V3hier.v3 uses `relationship_type` as a per-pair sequence
  # number, not a semantic "is a" label. The dummy fixture exercises this
  # with X40J8 hanging off X40J6 via type "05" — pre-#148 the "01" filter
  # would have dropped that edge entirely.
  dir <- create_dummy_read3_dir()
  local_build_temp_database()
  suppressMessages(add_read3_trud(path = dir, version = "test_v1"))

  # Use col_filters = NULL so retired codes (X40J7 status "R") show up
  # alongside the active ones — we're asserting on the walk, not on the
  # default lookup filter.
  result <- suppressMessages(suppressWarnings(
    CHILDREN("X40J5", type = "Read v3", col_filters = NULL)
  ))
  expect_setequal(
    result$code,
    c("X40J5", "X40J6", "X40J7", "X40J8")
  )
})

test_that("PARENTS() walks the full Read v3 hierarchy regardless of relationship_type", {
  dir <- create_dummy_read3_dir()
  local_build_temp_database()
  suppressMessages(add_read3_trud(path = dir, version = "test_v1"))

  result <- suppressMessages(suppressWarnings(
    PARENTS("X40J8", type = "Read v3")
  ))
  # X40J8 -> X40J6 (type 05) -> X40J5 (type 01) -> CHAP1/CHAP2 -> root.
  expect_true(all(
    c("X40J8", "X40J6", "X40J5", "CHAP1", "CHAP2", ".....") %in% result$code
  ))
})

test_that("get_relationship_tree() returns the same Read v3 walk for the same seed", {
  dir <- create_dummy_read3_dir()
  local_build_temp_database()
  suppressMessages(add_read3_trud(path = dir, version = "test_v1"))

  tree <- suppressMessages(suppressWarnings(
    get_relationship_tree("X40J5", type = "Read v3", col_filters = NULL)
  ))
  expect_setequal(
    tree$nodes$code,
    c("X40J5", "X40J6", "X40J7", "X40J8")
  )
})
