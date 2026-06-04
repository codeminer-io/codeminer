test_that("split_double_pipe works with simple strings", {
  expect_equal(split_double_pipe("E10"), "E10")
  expect_equal(split_double_pipe("E10 || E11"), c("E10", "E11"))
  expect_equal(split_double_pipe("E10 || E11 || E12"), c("E10", "E11", "E12"))
})

test_that("split_double_pipe trims whitespace", {
  expect_equal(split_double_pipe("  E10  ||  E11  "), c("E10", "E11"))
})

test_that("parse_codes works with simple strings", {
  result <- parse_codes("E10")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, "E10")
  expect_equal(result$description, NA_character_)
})

test_that("parse_codes works with double-pipe strings", {
  result <- parse_codes("E10 || E11")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, c("E10", "E11"))
})

test_that("parse_codes works with comment syntax", {
  result <- parse_codes("E10 << Type 1 diabetes >>")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, "E10")
  expect_equal(result$description, "Type 1 diabetes")
})


test_that("parse_codes works with both double-pipe and comments", {
  result <- parse_codes("E10 << Type 1 >> || E11 << Type 2 >>")
  expect_s3_class(result, "codeminer_codelist")
  expect_equal(result$code, c("E10", "E11"))
  expect_equal(result$description, c("Type 1", "Type 2"))
})

test_that("collect_codes_input works with character vectors", {
  result <- collect_codes_input("E10", "E11")
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with double-pipe strings", {
  result <- collect_codes_input("E10 || E11")
  expect_equal(result$codes, c("E10", "E11"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with mixed inputs", {
  result <- collect_codes_input("E10", c("E11", "E12"), "E13 || E14")
  expect_equal(result$codes, c("E10", "E11", "E12", "E13", "E14"))
  expect_null(result$code_type)
})

test_that("collect_codes_input works with codelist data frame", {
  cl <- as_codelist(data.frame(
    code = c("E10", "E11"),
    description = c("T1", "T2"),
    code_type = "ICD-10"
  ))
  result <- collect_codes_input(cl)
  expect_equal(result$codes, c("E10", "E11"))
  expect_equal(result$code_type, "ICD-10")
})

test_that("collect_codes_input works with regular data frame with code_type", {
  df <- data.frame(code = c("E10", "E11"), code_type = "ICD-10")
  result <- collect_codes_input(df)
  expect_equal(result$codes, c("E10", "E11"))
  expect_equal(result$code_type, "ICD-10")
})

test_that("collect_codes_input validates type matches codelist", {
  cl <- as_codelist(data.frame(
    code = "E10",
    description = "T1",
    code_type = "ICD-10"
  ))

  # Should work with matching type
  expect_silent(collect_codes_input(cl, type = "ICD-10"))

  # Should error with conflicting type
  expect_error(
    collect_codes_input(cl, type = "Read v3"),
    "Conflicting.*type"
  )
})

test_that("collect_codes_input returns empty for no input", {
  result <- collect_codes_input()
  expect_equal(result$codes, character(0))
  expect_null(result$code_type)
})

test_that("collect_codes_input errors for empty input when not allowed", {
  expect_error(
    collect_codes_input(allow_empty = FALSE),
    "No codes provided"
  )
})

test_that("collect_codes_input errors with non-character non-df input", {
  expect_error(
    collect_codes_input("E10", 123),
    "All inputs must be character"
  )
})

test_that("collect_codes_input errors for named args in ...", {
  # Single named arg (simulates misspelled parameter like `relationship =`)
  expect_error(
    collect_codes_input(relationship = "has attribute"),
    "Unexpected named argument"
  )
  # Named arg alongside valid codes
  expect_error(
    collect_codes_input("E10", type_filter = "foo"),
    "Unexpected named argument"
  )
})

# strip_comments() -----------------------------------------------------------

test_that("strip_comments strips <<>> descriptions", {
  expect_equal(
    strip_comments("363698007 <<Finding site (attribute)>>"),
    "363698007"
  )
})

test_that("strip_comments is a no-op for bare codes", {
  expect_equal(strip_comments("363698007"), "363698007")
})

test_that("strip_comments works with strings containing spaces", {
  expect_equal(
    strip_comments("has attribute <<Has attribute (attribute)>>"),
    "has attribute"
  )
})

test_that("strip_comments works with a vector", {
  result <- strip_comments(c(
    "363698007 <<Finding site (attribute)>>",
    "116680003",
    "has attribute <<Has attribute>>",
    "364075005 <<Heart rate (observable entity)>>"
  ))
  expect_equal(
    result,
    c("363698007", "116680003", "has attribute", "364075005")
  )
})

# resolve_relationship_types() -----------------------------------------------

test_that("resolve_relationship_types returns NULL for NULL input", {
  expect_null(resolve_relationship_types(NULL))
})

test_that("resolve_relationship_types returns character vector unchanged", {
  expect_equal(
    resolve_relationship_types(c("has attribute", "has property")),
    c("has attribute", "has property")
  )
})

test_that("resolve_relationship_types strips <<>> comments", {
  expect_equal(
    resolve_relationship_types("has attribute <<Has attribute (attribute)>>"),
    "has attribute"
  )
})

test_that("resolve_relationship_types extracts codes from codelist", {
  cl <- as_codelist(data.frame(
    code = c("has attribute", "has property"),
    description = c("Attr", "Prop"),
    code_type = "test_type"
  ))
  expect_equal(
    resolve_relationship_types(cl),
    c("has attribute", "has property")
  )
})

test_that("resolve_relationship_types extracts codes from data.frame", {
  df <- data.frame(code = c("has attribute", "has property"))
  expect_equal(
    resolve_relationship_types(df),
    c("has attribute", "has property")
  )
})

test_that("resolve_relationship_types errors for data.frame without code column", {
  df <- data.frame(name = "x", value = 1)
  expect_error(
    resolve_relationship_types(df),
    "must have a.*code.*column"
  )
})

test_that("resolve_relationship_types errors for invalid type", {
  expect_error(
    resolve_relationship_types(123),
    "must be NULL, a character vector, or a data frame.*code.*column"
  )
})

test_that("resolve_relationship_types errors for mismatched code_type", {
  cl <- as_codelist(data.frame(
    code = "has attribute",
    description = "Attr",
    code_type = "ICD-10"
  ))
  expect_error(
    resolve_relationship_types(cl, expected_type = "SNOMED CT"),
    "Conflicting code types"
  )
})

test_that("resolve_relationship_types allows matching code_type", {
  cl <- as_codelist(data.frame(
    code = "has attribute",
    description = "Attr",
    code_type = "test_type"
  ))
  expect_equal(
    resolve_relationship_types(cl, expected_type = "test_type"),
    "has attribute"
  )
})

test_that("resolve_relationship_types skips validation for NA code_type", {
  df <- data.frame(code = "has attribute", code_type = NA_character_)
  expect_equal(
    resolve_relationship_types(df, expected_type = "SNOMED CT"),
    "has attribute"
  )
})

test_that("collect_codes_input errors with multiple data frames", {
  df1 <- data.frame(code = "E10", code_type = "ICD-10")
  df2 <- data.frame(code = "E11", code_type = "ICD-10")

  # Can only accept a single data frame
  expect_error(
    collect_codes_input(df1, df2),
    "All inputs must be character"
  )
})
