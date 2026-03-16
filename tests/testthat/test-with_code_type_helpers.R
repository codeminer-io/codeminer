test_that("All code type wrapper functions exist in namespace", {
  code_types <- c(
    "icd10",
    "icd9",
    "read3",
    "read2",
    "sct",
    "opcs4",
    "phecode",
    "read2_drugs",
    "bnf",
    "dmd",
    "data_coding_3",
    "data_coding_4",
    "data_coding_5",
    "data_coding_6"
  )

  for (code_type in code_types) {
    expect_true(
      exists(code_type, where = asNamespace("codeminer")),
      info = paste("Function", code_type, "should exist in codeminer namespace")
    )
  }
})
