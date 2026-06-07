suppressMessages(create_dummy_database(.local = TRUE))

test_that("get_metadata_for_relationship() works correctly", {
  con <- connect_to_db()

  # Test valid code_type and version
  meta <- get_metadata_for_relationship(con, "ICD-10", "UKB v4")
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 1)
  expect_true(all(
    c("code_type", "relationship_version", "relationship_table_name") %in%
      names(meta)
  ))

  # Test latest version
  meta_latest <- get_metadata_for_relationship(con, "ICD-10", "latest")
  expect_s3_class(meta_latest, "data.frame")
  expect_equal(nrow(meta_latest), 1)
})

# get_relationship_tree() ---------------------------------------------------

test_that("get_relationship_tree() returns list(nodes, edges) with expected columns", {
  tree <- get_relationship_tree("E10", type = "ICD-10")
  expect_named(tree, c("nodes", "edges"))
  expect_s3_class(tree$nodes, "tbl_df")
  expect_s3_class(tree$edges, "tbl_df")
  expect_named(tree$nodes, c("code", "term", "category", "in_input_set"))
  expect_named(tree$edges, c("parent", "child"))
  expect_type(tree$nodes$in_input_set, "logical")
})

test_that("get_relationship_tree() expand_to_descendants = TRUE pulls in children", {
  tree <- get_relationship_tree("E10", type = "ICD-10")
  # Seed always present, and at least one descendant picked up
  expect_true("E10" %in% tree$nodes$code)
  expect_gt(nrow(tree$nodes), 1L)
  # Edge endpoints are codes in the node set (self-consistency)
  expect_true(all(tree$edges$parent %in% tree$nodes$code))
  expect_true(all(tree$edges$child %in% tree$nodes$code))
})

test_that("get_relationship_tree() expand_to_descendants = FALSE uses input as-is", {
  draft <- c("E10", "E11")
  tree <- get_relationship_tree(
    draft,
    type = "ICD-10",
    expand_to_descendants = FALSE
  )
  expect_setequal(tree$nodes$code, draft)
  # Edges (if any) only between the input pair
  expect_true(all(tree$edges$parent %in% draft))
  expect_true(all(tree$edges$child %in% draft))
})

test_that("get_relationship_tree() in_input_set flags only the seed", {
  tree <- get_relationship_tree("E10", type = "ICD-10")
  expect_true(tree$nodes$in_input_set[tree$nodes$code == "E10"])
  # Any descendant beyond the seed is not in the input set
  descendants <- tree$nodes$code[tree$nodes$code != "E10"]
  if (length(descendants) > 0) {
    expect_false(any(
      tree$nodes$in_input_set[tree$nodes$code %in% descendants]
    ))
  }
})

test_that("get_relationship_tree() includes orphan codes (in nodes, not in edges)", {
  # Use expand_to_descendants = FALSE with two unrelated codes — at least
  # one will be an orphan with respect to the other.
  draft <- c("E10", "I10")
  tree <- get_relationship_tree(
    draft,
    type = "ICD-10",
    expand_to_descendants = FALSE
  )
  expect_setequal(tree$nodes$code, draft)
  # No edges between unrelated codes
  expect_equal(nrow(tree$edges), 0)
})

test_that("get_relationship_tree() aborts when input exceeds max_codes (pre-expansion)", {
  big_input <- paste0("FAKE", seq_len(20))
  expect_error(
    get_relationship_tree(big_input, type = "ICD-10", max_codes = 5),
    class = "codeminer_max_tree_codes_exceeded"
  )
})

test_that("get_relationship_tree() aborts when expansion exceeds max_codes", {
  # Seed E10 expands to its descendants; cap at 1 so the post-expansion
  # check trips even though the input itself is fine.
  expect_error(
    get_relationship_tree("E10", type = "ICD-10", max_codes = 1),
    class = "codeminer_max_tree_codes_exceeded"
  )
})

test_that("get_relationship_tree() honours codeminer.max_tree_codes option", {
  withr::with_options(
    list(codeminer.max_tree_codes = 1),
    expect_error(
      get_relationship_tree("E10", type = "ICD-10"),
      class = "codeminer_max_tree_codes_exceeded"
    )
  )
})

test_that("get_relationship_tree() rejects empty codes input", {
  expect_error(
    get_relationship_tree(character(), type = "ICD-10"),
    "non-empty character vector"
  )
})

test_that("get_relationship_tree() rejects non-character codes input", {
  expect_error(
    get_relationship_tree(123, type = "ICD-10"),
    "non-empty character vector"
  )
})
