suppressMessages(create_dummy_database(.local = TRUE))

# fmt: skip
test_lookup_table <- tibble::tribble(
  ~code , ~description ,
  "f1"  , "apple"      ,
  "f2"  , "banana"     ,
  "f3"  , "pear"       ,
  "f4"  , "pineapple"  ,
  "f5"  , "cherry"
)
add_lookup_table(test_lookup_table, lookup_metadata("fruits"))

test_that("DESCRIPTION() works with regular expressions", {
  fruits_with_a <- c("apple", "banana", "pear", "pineapple")
  starts_with_a <- "apple"
  ends_with_a <- "banana"
  has_double_p <- c("apple", "pineapple")
  starts_with_p_ends_with_apple <- "pineapple"

  expect_identical(DESCRIPTION("a", "fruits")$description, fruits_with_a)
  expect_identical(DESCRIPTION("^a", "fruits")$description, starts_with_a)
  expect_identical(DESCRIPTION("a$", "fruits")$description, ends_with_a)
  expect_identical(DESCRIPTION("pp", "fruits")$description, has_double_p)
  expect_identical(
    DESCRIPTION("^p.*apple$", "fruits")$description,
    starts_with_p_ends_with_apple
  )
})

test_that("DESCRIPTION() can be configured to be case insenstive", {
  ignore_case <- DESCRIPTION("Apple", "fruits", ignore_case = TRUE)
  expect_identical(ignore_case$code, c("f1", "f4"))

  case_sensitive <- DESCRIPTION("Apple", "fruits", ignore_case = FALSE)
  expect_identical(case_sensitive$code, character(0))
})

test_that("DESCRIPTION() can return codes with a secondary description that matches the search string", {
  preferred_only <- DESCRIPTION(
    "IDDM",
    "Read v3",
    preferred_description_only = TRUE
  )
  expect_equal(nrow(preferred_only), 1)

  all_desc <- DESCRIPTION("IDDM", "Read v3", preferred_description_only = FALSE)
  expect_true(nrow(all_desc) > 1)
})
