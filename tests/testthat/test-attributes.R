suppressMessages(create_dummy_database(.local = TRUE))

test_that("ATTRIBUTES_FOR() and HAS_ATTRIBUTES() return the expected data format", {
  # Setup using example_ontology v2 (has both hierarchy and attributes)
  test_type <- "capital_letters"

  # Add lookup table for capital letters
  add_lookup_table(
    example_ontology$lookup_tables$capital_letters_v2,
    example_ontology$lookup_metadata |>
      dplyr::filter(code_type == test_type, lookup_version == "v2")
  )

  # Add relationship table (includes "has attribute" relationships)
  add_relationship_table(
    example_ontology$relationship_tables$capital_letters_relationship_v2,
    example_ontology$relationship_metadata |>
      dplyr::filter(code_type == test_type, relationship_version == "v2")
  )

  # ATTRIBUTES_FOR tests - A has attribute alpha_code, B has attribute beta_code
  result <- ATTRIBUTES_FOR("A", type = test_type)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_identical(unique(result$code_type), test_type)
  expect_identical(result$code, "alpha_code")

  # Test codes extraction
  result_codes <- ATTRIBUTES_FOR(
    "B",
    type = test_type,
    relationship_types = "has attribute"
  )
  expect_type(result_codes$code, "character")
  expect_identical(result_codes$code, "beta_code")
  expect_false("B" %in% result_codes$code)

  # HAS_ATTRIBUTES tests - alpha_code is an attribute of A
  has_result <- HAS_ATTRIBUTES("alpha_code", type = test_type)
  expect_s3_class(has_result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(has_result)))
  expect_identical(has_result$code, "A")

  has_result_codes <- HAS_ATTRIBUTES(
    "beta_code",
    type = test_type
  )
  expect_type(has_result_codes$code, "character")
  expect_identical(has_result_codes$code, "B")
  expect_false("beta_code" %in% has_result_codes$code)
})

test_that("ATTRIBUTES_FOR() and HAS_ATTRIBUTES() filter by relationship_types", {
  test_type <- "dummy_attr_types"

  dummy_lookup <- data.frame(
    code = c("code1", "attr1", "attr2", "attr3"),
    description = c("Code 1", "Attr 1", "Attr 2", "Attr 3")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "code1", "code1"),
    to = c("attr1", "attr2", "attr3"),
    type = c("has attribute", "has property", "has attribute")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # Filter for only "has attribute"
  result <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = "has attribute",
    type = test_type
  )
  expect_identical(sort(result$code), c("attr1", "attr3"))

  # Filter for only "has property"
  result2 <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = "has property",
    type = test_type
  )
  expect_identical(result2$code, "attr2")

  # HAS_ATTRIBUTES with filter
  has_result <- HAS_ATTRIBUTES(
    "attr2",
    relationship_types = "has property",
    type = test_type
  )
  expect_identical(has_result$code, "code1")
})

test_that("relationship_types accepts a codelist object", {
  test_type <- "dummy_attr_types"

  # Use a codelist (as returned by CODES()) for relationship_types
  rel_cl <- as_codelist(data.frame(
    code = "has attribute",
    description = "Has attribute",
    code_type = test_type
  ))

  result <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = rel_cl,
    type = test_type
  )
  expect_identical(sort(result$code), c("attr1", "attr3"))

  has_result <- HAS_ATTRIBUTES(
    "attr1",
    relationship_types = rel_cl,
    type = test_type
  )
  expect_identical(has_result$code, "code1")
})

test_that("relationship_types accepts a plain data.frame with code column", {
  test_type <- "dummy_attr_types"

  rel_df <- data.frame(code = "has property")
  result <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = rel_df,
    type = test_type
  )
  expect_identical(result$code, "attr2")
})

test_that("relationship_types with <<>> comments works the same as bare codes", {
  test_type <- "dummy_attr_types"

  bare <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = "has attribute",
    type = test_type
  )

  commented <- ATTRIBUTES_FOR(
    "code1",
    relationship_types = "has attribute <<Has attribute (attribute)>>",
    type = test_type
  )

  expect_identical(bare$code, commented$code)

  bare_has <- HAS_ATTRIBUTES(
    "attr2",
    relationship_types = "has property",
    type = test_type
  )
  commented_has <- HAS_ATTRIBUTES(
    "attr2",
    relationship_types = "has property <<Has property>>",
    type = test_type
  )
  expect_identical(bare_has$code, commented_has$code)
})

test_that("relationship_types errors for mismatched code_type", {
  test_type <- "dummy_attr_types"

  wrong_cl <- as_codelist(data.frame(
    code = "has attribute",
    description = "Attr",
    code_type = "ICD-10"
  ))

  expect_error(
    ATTRIBUTES_FOR("code1", relationship_types = wrong_cl, type = test_type),
    "Conflicting code types"
  )
})

test_that("relationship_types errors for invalid input type", {
  test_type <- "dummy_attr_types"

  expect_error(
    ATTRIBUTES_FOR("code1", relationship_types = 123, type = test_type),
    "must be NULL, a character vector, or a data frame.*code.*column"
  )
})

test_that("ATTRIBUTES_FOR() only returns immediate attributes (max_depth = 1)", {
  test_type <- "dummy_attr2"

  dummy_lookup <- data.frame(
    code = c("code1", "attr1", "attr2"),
    description = c("Code 1", "Attribute 1", "Attribute 2")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "attr1"),
    to = c("attr1", "attr2"),
    type = c("has attribute", "has attribute")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  result <- ATTRIBUTES_FOR("code1", type = test_type)

  # Should only return attr1, not attr2 (which is an attribute of attr1)
  expect_identical(result$code, "attr1")
})

test_that("ATTRIBUTES_FOR() and HAS_ATTRIBUTES() return empty for codes with no relationships", {
  test_type <- "dummy_attr3"

  dummy_lookup <- data.frame(
    code = c("code_no_attr", "attr_unused"),
    description = c("Code with no attributes", "Unused attribute")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = character(0),
    to = character(0),
    type = character(0)
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # ATTRIBUTES_FOR
  suppressWarnings(
    result_df <- ATTRIBUTES_FOR(
      "code_no_attr",
      type = test_type
    )
  )
  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 0)

  suppressWarnings(
    result_vec <- ATTRIBUTES_FOR(
      "code_no_attr",
      type = test_type
    )
  )
  expect_type(result_vec$code, "character")
  expect_equal(length(result_vec$code), 0)

  # HAS_ATTRIBUTES
  suppressWarnings(
    has_df <- HAS_ATTRIBUTES(
      "attr_unused",
      type = test_type
    )
  )
  expect_s3_class(has_df, "data.frame")
  expect_equal(nrow(has_df), 0)

  suppressWarnings(
    has_vec <- HAS_ATTRIBUTES(
      "attr_unused",
      type = test_type
    )
  )
  expect_type(has_vec$code, "character")
  expect_equal(length(has_vec$code), 0)
})

test_that("ATTRIBUTES_FOR() works with multiple codes", {
  test_type <- "dummy_attr4"

  dummy_lookup <- data.frame(
    code = c("code1", "code2", "attr1", "attr2", "attr3"),
    description = c("Code 1", "Code 2", "Attr 1", "Attr 2", "Attr 3")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "code1", "code2"),
    to = c("attr1", "attr2", "attr3"),
    type = c("has attribute", "has attribute", "has attribute")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  result <- ATTRIBUTES_FOR(
    c("code1", "code2"),
    type = test_type
  )

  expect_identical(sort(result$code), c("attr1", "attr2", "attr3"))
})
