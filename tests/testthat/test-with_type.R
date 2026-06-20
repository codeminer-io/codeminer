test_that("with_type() sets the code type options for the duration of code", {
  # options are unset outside the call
  withr::local_options(codeminer.code_type = NULL, codeminer.map_to = NULL)

  observed <- with_type("ICD-10", {
    list(
      code_type = getOption("codeminer.code_type"),
      map_to = getOption("codeminer.map_to")
    )
  })

  expect_equal(observed$code_type, "ICD-10")
  expect_equal(observed$map_to, "ICD-10")

  # options are restored afterwards
  expect_null(getOption("codeminer.code_type"))
  expect_null(getOption("codeminer.map_to"))
})

test_that("with_type() works with any user-supplied code type string", {
  withr::local_options(codeminer.code_type = NULL)

  expect_equal(
    with_type("my_custom_type", getOption("codeminer.code_type")),
    "my_custom_type"
  )
})

test_that("with_type() returns the value of code", {
  expect_equal(with_type("ICD-10", 1 + 1), 2)
})
