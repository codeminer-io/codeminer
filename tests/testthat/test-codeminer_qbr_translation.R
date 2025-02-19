test_that("codeminer to qbr list translation works", {

  # Simple queries - DESCRIPTION, CODES, CHILDREN ------------------------------
  simple_query <- 'DESCRIPTION("diab") %AND% DESCRIPTION("retin|mac") %NOT%
     DESCRIPTION("absent|without") %OR% (DESCRIPTION("diab") %AND%
     DESCRIPTION("nephro|neuro") %NOT% DESCRIPTION("absent|without"))'

  qbr_from_simple_query <- translate_codeminer_query_to_qbr_list(simple_query)

  expect_equal(qbr_from_simple_query,
               list(
                 condition = "OR",
                 rules = list(
                   list(condition = "NOT", rules = list(
                     list(condition = "AND", rules = list(
                       list(
                         id = "description",
                         field = "description",
                         type = "string",
                         input = "text",
                         operator = "sct",
                         value = "diab"
                       ),
                       list(
                         id = "description",
                         field = "description",
                         type = "string",
                         input = "text",
                         operator = "sct",
                         value = "retin|mac"
                       )
                     )),
                     list(
                       id = "description",
                       field = "description",
                       type = "string",
                       input = "text",
                       operator = "sct",
                       value = "absent|without"
                     )
                   )),
                   list(condition = "NOT", rules = list(
                     list(condition = "AND", rules = list(
                       list(
                         id = "description",
                         field = "description",
                         type = "string",
                         input = "text",
                         operator = "sct",
                         value = "diab"
                       ),
                       list(
                         id = "description",
                         field = "description",
                         type = "string",
                         input = "text",
                         operator = "sct",
                         value = "nephro|neuro"
                       )
                     )),
                     list(
                       id = "description",
                       field = "description",
                       type = "string",
                       input = "text",
                       operator = "sct",
                       value = "absent|without"
                     )
                   ))
                 ),
                 valid = TRUE
               ))

  # Simple query, single statement -------------------------------------------

  simple_query_single <- 'DESCRIPTION("diab")'

  qbr_from_simple_query_single <- translate_codeminer_query_to_qbr_list(simple_query_single)

  expect_equal(qbr_from_simple_query_single, list(
    list(
      id = "description",
      field = "description",
      type = "string",
      input = "text",
      operator = "sct",
      value = "diab"
    )
  ))
})
