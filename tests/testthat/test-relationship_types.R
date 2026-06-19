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
  add_relationship_table(
    dummy_relationships,
    relationship_metadata(
      test_type,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  )

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
  add_relationship_table(
    dummy_relationships,
    relationship_metadata(
      test_type,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  )

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

# A relationship table whose type values are themselves codes in the lookup
# table (as SNOMED CT relationship types are concept ids).
setup_typed_ontology <- function(test_type) {
  add_lookup_table(
    data.frame(
      code = c("disorder", "finding", "T1", "T2"),
      description = c(
        "Disorder X",
        "Finding Y",
        "Is a (attribute)",
        "Finding site (attribute)"
      )
    ),
    lookup_metadata(test_type)
  )
  add_relationship_table(
    data.frame(
      from = c("disorder", "disorder"),
      to = c("finding", "finding"),
      type = c("T1", "T2")
    ),
    relationship_metadata(
      test_type,
      type_col = "type",
      child_parent_relationship_code = "T1"
    )
  )
}

test_that("RELATIONSHIP_TYPES_FROM/TO describe types via the lookup table", {
  test_type <- "typed_onto_fromto"
  setup_typed_ontology(test_type)

  from <- RELATIONSHIP_TYPES_FROM("disorder", type = test_type)
  expect_s3_class(from, "codeminer_codelist")
  expect_setequal(from$code, c("T1", "T2"))
  expect_setequal(
    from$description,
    c("Is a (attribute)", "Finding site (attribute)")
  )
})

test_that("RELATIONSHIP_TYPES_* fall back to the code when no lookup entry", {
  # `is a` / `has attribute` are not codes in this lookup, so description
  # falls back to the type value itself.
  test_type <- "dummy_rel_fallback"
  add_lookup_table(
    data.frame(code = c("code1", "attr1"), description = c("Code 1", "Attr 1")),
    lookup_metadata(test_type)
  )
  add_relationship_table(
    data.frame(from = "code1", to = "attr1", type = "has attribute"),
    relationship_metadata(
      test_type,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  )

  res <- RELATIONSHIP_TYPES_FROM("code1", type = test_type)
  expect_identical(res$code, "has attribute")
  expect_identical(res$description, "has attribute")
})

test_that("RELATIONSHIP_TYPES() lists all types for a code type", {
  test_type <- "typed_onto_all"
  setup_typed_ontology(test_type)

  res <- RELATIONSHIP_TYPES(type = test_type)
  expect_s3_class(res, "codeminer_codelist")
  expect_setequal(res$code, c("T1", "T2"))
  expect_setequal(
    res$description,
    c("Is a (attribute)", "Finding site (attribute)")
  )
})

test_that("RELATIONSHIP_TYPES() filters by description pattern", {
  test_type <- "typed_onto_pattern"
  setup_typed_ontology(test_type)

  res <- RELATIONSHIP_TYPES("finding site", type = test_type)
  expect_identical(res$code, "T2")

  # Case-insensitive by default.
  expect_identical(
    RELATIONSHIP_TYPES("FINDING SITE", type = test_type)$code,
    "T2"
  )

  # No match returns an empty codelist.
  empty <- RELATIONSHIP_TYPES("no such type", type = test_type)
  expect_s3_class(empty, "codeminer_codelist")
  expect_equal(nrow(empty), 0)
})

test_that("RELATIONSHIP_TYPES() result feeds into ATTRIBUTES_FOR()", {
  test_type <- "typed_onto_feed"
  setup_typed_ontology(test_type)

  finding_site <- RELATIONSHIP_TYPES("finding site", type = test_type)
  res <- ATTRIBUTES_FOR(
    "disorder",
    relationship_types = finding_site,
    type = test_type
  )
  expect_identical(res$code, "finding")
})

test_that("RELATIONSHIP_TYPES() aborts on a purely hierarchical table", {
  test_type <- "pure_hier_rel_types"
  add_lookup_table(
    data.frame(code = c("a", "b"), description = c("A", "B")),
    lookup_metadata(test_type)
  )
  add_relationship_table(
    data.frame(from = "a", to = "b"),
    relationship_metadata(test_type)
  )

  expect_error(
    RELATIONSHIP_TYPES(type = test_type),
    class = "codeminer_no_relationship_types"
  )
})
