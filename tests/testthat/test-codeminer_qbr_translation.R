
# Example codeminer query-qbr pairs ---------------------------------------

egs <- list(
  simple_query = list(
    query = rlang::parse_expr('DESCRIPTION("diab") %AND% DESCRIPTION("retin|mac") %NOT%
     DESCRIPTION("absent|without") %OR% (DESCRIPTION("diab") %AND%
     DESCRIPTION("nephro|neuro") %NOT% DESCRIPTION("absent|without"))'),
    qbr = list(
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
    )
  ),
  simple_query2 = list(
    query = rlang::parse_expr('DESCRIPTION("diab") %AND% DESCRIPTION("type 1") %OR%
    (CHILDREN("E10") %OR% CODES("O240"))'),
    qbr = list(
      condition = "OR",
      rules = list(
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
            value = "type 1"
          )
        )),
        list(condition = "OR", rules = list(
          list(
            id = "children",
            field = "children",
            type = "string",
            input = "text",
            operator = "sct",
            value = "E10"
          ),
          list(
            id = "codes",
            field = "codes",
            type = "string",
            input = "text",
            operator = "sct",
            value = "O240"
          )
        ))
      ),
      valid = TRUE
    )
  ),
  simple_query_single = list(query = rlang::parse_expr('DESCRIPTION("diab")'), qbr = list(
    list(
      id = "description",
      field = "description",
      type = "string",
      input = "text",
      operator = "sct",
      value = "diab"
    )
  ))
)

# TESTS -------------------------------------------------------------------

test_that("Simple queries with AND/NOT/OR operators and DESCRIPTION/CHILDREN/CODES translate", {

  # DESCRIPTION() only
  expect_equal(translate_codeminer_query_to_qbr_list(egs$simple_query$query),
               egs$simple_query$qbr)

  expect_equal(egs$simple_query$query |>
                 deparse1(),
               egs$simple_query$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

  # DESCRIPTION(), CHILDREN() and CODES()
  expect_equal(translate_codeminer_query_to_qbr_list(egs$simple_query2$query),
               egs$simple_query2$qbr)

  expect_equal(egs$simple_query2$query |>
                 deparse1(),
               egs$simple_query2$qbr |>
                 custom_qbr_translation() |>
                 deparse1())
})

test_that("Single statement single queries translate", {
  expect_equal(translate_codeminer_query_to_qbr_list(egs$simple_query_single$query),
               egs$simple_query_single$qbr)

  expect_equal(egs$simple_query_single$query |>
                 deparse1(),
               egs$simple_query_single$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

})
