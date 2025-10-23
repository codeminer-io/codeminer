test_that("All code_types have corresponding functions", {
  for (code_type in CODE_TYPE_TO_LKP_TABLE_MAP$code) {
    expect_true(
      exists(code_type, where = asNamespace("codeminer")),
      info = paste("Function", code_type, "should exist in codeminer namespace")
    )
  }
})
