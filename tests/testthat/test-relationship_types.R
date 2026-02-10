suppressMessages(create_dummy_database(.local = TRUE))

test_that("RELATIONSHIP_TYPES_FROM() and RELATIONSHIP_TYPES_TO() return expected types", {
  # Setup using example_ontology v2 (has both "is a" and "has attribute")
  test_type <- "capital_letters"

  add_lookup_table(
    example_ontology$lookup_tables$capital_letters_v2,
    example_ontology$lookup_metadata |>
      dplyr::filter(code_type == test_type, lookup_version == "v2")
  )

  add_relationship_table(
    example_ontology$relationship_tables$capital_letters_relationship_v2,
    example_ontology$relationship_metadata |>
      dplyr::filter(code_type == test_type, relationship_version == "v2")
  )

  # RELATIONSHIP_TYPES_FROM
  # B has "is a" relationship to A, and "has attribute" relationship to beta_code
  types_from_b <- RELATIONSHIP_TYPES_FROM("B", type = test_type)
  expect_type(types_from_b$code, "character")
  expect_identical(sort(types_from_b$code), c("has attribute", "is a"))

  # A only has "has attribute" relationship
  types_from_a <- RELATIONSHIP_TYPES_FROM("A", type = test_type)
  expect_identical(types_from_a$code, "has attribute")

  # RELATIONSHIP_TYPES_TO
  # A is the target of "is a" relationships from B, C, D
  types_to_a <- RELATIONSHIP_TYPES_TO("A", type = test_type)
  expect_identical(types_to_a$code, "is a")

  # alpha_code is target of "has attribute" from A
  types_to_alpha <- RELATIONSHIP_TYPES_TO("alpha_code", type = test_type)
  expect_identical(types_to_alpha$code, "has attribute")
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
    type = test_type
  )
  expect_identical(sort(types_from$code), c("type_a", "type_b", "type_c"))

  # Multiple codes in TO
  types_to <- RELATIONSHIP_TYPES_TO(c("attr1", "attr2"), type = test_type)
  expect_identical(sort(types_to$code), c("type_a", "type_b", "type_c"))
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
    RELATIONSHIP_TYPES_FROM(c("nonexistent", "code1"), type = test_type),
    class = "codeminer_missing_codes"
  )

  expect_warning(
    RELATIONSHIP_TYPES_TO(c("nonexistent", "attr1"), type = test_type),
    class = "codeminer_missing_codes"
  )
})
