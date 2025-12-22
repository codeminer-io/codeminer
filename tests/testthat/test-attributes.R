create_dummy_database()

test_that("ATTRIBUTES_FOR() returns the expected data format", {
  # setup
  test_type <- "dummy_attr"

  dummy_lookup <- data.frame(
    code = c("code1", "attr1", "attr2"),
    description = c("Code 1", "Attribute 1", "Attribute 2")
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = c("code1", "code1"),
    to = c("attr1", "attr2"),
    type = c("has attribute", "has attribute")
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  # tests
  result <- ATTRIBUTES_FOR("code1", code_type = test_type)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_identical(unique(result$code_type), test_type)
  expect_identical(sort(result$code), c("attr1", "attr2"))

  # codes_only = TRUE
  result_codes_only <- ATTRIBUTES_FOR(
    "code1",
    code_type = test_type,
    codes_only = TRUE
  )

  expect_type(result_codes_only, "character")
  expect_identical(sort(result_codes_only), c("attr1", "attr2"))

  # Should not include self
  expect_false("code1" %in% result_codes_only)
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

  result <- ATTRIBUTES_FOR("code1", code_type = test_type, codes_only = TRUE)

  # Should only return attr1, not attr2 (which is an attribute of attr1)
  expect_identical(result, "attr1")
})

test_that("ATTRIBUTES_FOR() returns empty for codes with no attributes", {
  test_type <- "dummy_attr3"

  dummy_lookup <- data.frame(
    code = "code_no_attr",
    description = "Code with no attributes"
  )
  add_lookup_table(dummy_lookup, lookup_metadata(test_type))

  dummy_relationships <- data.frame(
    from = character(0),
    to = character(0),
    type = character(0)
  )
  add_relationship_table(dummy_relationships, relationship_metadata(test_type))

  suppressWarnings(
    result_df <- ATTRIBUTES_FOR(
      "code_no_attr",
      code_type = test_type,
      codes_only = FALSE
    )
  )
  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 0)

  suppressWarnings(
    result_vec <- ATTRIBUTES_FOR(
      "code_no_attr",
      code_type = test_type,
      codes_only = TRUE
    )
  )
  expect_type(result_vec, "character")
  expect_equal(length(result_vec), 0)
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
    code_type = test_type,
    codes_only = TRUE
  )

  expect_identical(sort(result), c("attr1", "attr2", "attr3"))
})
