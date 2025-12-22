create_dummy_database()

test_that("RELATIONSHIP_TYPES_FROM() and RELATIONSHIP_TYPES_TO() return expected types", {
  test_type <- "dummy_rel_types"

  dummy_lookup <- data.frame(
    code = c("code1", "code2", "attr1", "attr2", "target1"),
    description = c("Code 1", "Code 2", "Attr 1", "Attr 2", "Target 1")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "code1", "code2", "attr1"),
    to = c("attr1", "target1", "attr2", "target1"),
    type = c("has attribute", "relates to", "has property", "relates to")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # RELATIONSHIP_TYPES_FROM
  types_from_code1 <- RELATIONSHIP_TYPES_FROM("code1", code_type = test_type)
  expect_type(types_from_code1, "character")
  expect_identical(sort(types_from_code1), c("has attribute", "relates to"))

  types_from_code2 <- RELATIONSHIP_TYPES_FROM("code2", code_type = test_type)
  expect_identical(types_from_code2, "has property")

  # RELATIONSHIP_TYPES_TO
  types_to_target1 <- RELATIONSHIP_TYPES_TO("target1", code_type = test_type)
  expect_identical(types_to_target1, "relates to")

  types_to_attr1 <- RELATIONSHIP_TYPES_TO("attr1", code_type = test_type)
  expect_identical(types_to_attr1, "has attribute")

  types_to_attr2 <- RELATIONSHIP_TYPES_TO("attr2", code_type = test_type)
  expect_identical(types_to_attr2, "has property")
})

test_that("RELATIONSHIP_TYPES_FROM() and RELATIONSHIP_TYPES_TO() handle multiple codes", {
  test_type <- "dummy_rel_types2"

  dummy_lookup <- data.frame(
    code = c("code1", "code2", "attr1", "attr2"),
    description = c("Code 1", "Code 2", "Attr 1", "Attr 2")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "code2", "code2"),
    to = c("attr1", "attr1", "attr2"),
    type = c("type_a", "type_b", "type_c")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # Multiple codes in FROM
  types_from <- RELATIONSHIP_TYPES_FROM(
    c("code1", "code2"),
    code_type = test_type
  )
  expect_identical(sort(types_from), c("type_a", "type_b", "type_c"))

  # Multiple codes in TO
  types_to <- RELATIONSHIP_TYPES_TO(c("attr1", "attr2"), code_type = test_type)
  expect_identical(sort(types_to), c("type_a", "type_b", "type_c"))
})

test_that("RELATIONSHIP_TYPES_FROM() and RELATIONSHIP_TYPES_TO() warn for missing codes", {
  test_type <- "dummy_rel_types3"

  dummy_lookup <- data.frame(
    code = c("code1", "attr1"),
    description = c("Code 1", "Attr 1")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = "code1",
    to = "attr1",
    type = "has attribute"
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # Missing codes should trigger warning
  expect_warning(
    RELATIONSHIP_TYPES_FROM(c("nonexistent", "code1"), code_type = test_type),
    class = "codeminer_missing_codes"
  )

  expect_warning(
    RELATIONSHIP_TYPES_TO(c("nonexistent", "attr1"), code_type = test_type),
    class = "codeminer_missing_codes"
  )
})
