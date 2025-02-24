
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
  )),
  simple_saved_query = list(
    query = rlang::parse_expr('DIAB %OR% RETINOPATHY'),
    qbr = list(
      condition = "OR",
      rules = list(
        list(
          id = "saved_query",
          field = "saved_query",
          type = "string",
          input = "select",
          operator = "icd10",
          value = "DIAB"
        ),
        list(
          id = "saved_query",
          field = "saved_query",
          type = "string",
          input = "select",
          operator = "icd10",
          value = "RETINOPATHY"
        )
      ),
      valid = TRUE
    )
  ),
  simple_saved_query_single = list(query = rlang::parse_expr('DIAB'), qbr = list(
    list(
      id = "saved_query",
      field = "saved_query",
      type = "string",
      input = "select",
      operator = "icd10",
      value = "DIAB"
    )
  )),
  has_attributes_query = list(
    query = rlang::parse_expr(
      'HAS_ATTRIBUTES(CHILD_BETA_BLOCKER_SUBSTANCE, relationship = ATTRIBUTES_HAS_INGREDIENT)'
    ),
    qbr = list(
      list(
        id = "sct_has_attributes",
        field = "sct_has_attributes",
        type = "string",
        input = "select",
        operator = "sct_relationship",
        value = list("CHILD_BETA_BLOCKER_SUBSTANCE", "ATTRIBUTES_HAS_INGREDIENT")
      )
    )
  ),
  map_codes_query = list(query = rlang::parse_expr('MAP("E101 << Type 1 diabetes mellitus With ketoacidosis >>", from = "icd10")'), qbr = list(
    list(
      id = "map_codes",
      field = "map_codes",
      type = "string",
      input = "text",
      operator = "icd10",
      value = "E101 << Type 1 diabetes mellitus With ketoacidosis >>"
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

test_that("Simple saved query statements translate", {
  expect_equal(translate_codeminer_query_to_qbr_list(egs$simple_saved_query$query),
               egs$simple_saved_query$qbr)

  expect_equal(egs$simple_saved_query$query |>
                 deparse1(),
               egs$simple_saved_query$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

})

test_that("Simple single saved query statements translate", {
  expect_equal(translate_codeminer_query_to_qbr_list(egs$simple_saved_query_single$query),
               egs$simple_saved_query_single$qbr)

  expect_equal(egs$simple_saved_query_single$query |>
                 deparse1(),
               egs$simple_saved_query_single$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

})

test_that("Has attributes (sct) query statements translate", {
  expect_equal(translate_codeminer_query_to_qbr_list(egs$has_attributes_query$query),
               egs$has_attributes_query$qbr)

  expect_equal(egs$has_attributes_query$query |>
                 deparse1(),
               egs$has_attributes_query$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

})

test_that("Map codes query statements translate", {
  expect_equal(translate_codeminer_query_to_qbr_list(egs$map_codes_query$query),
               egs$map_codes_query$qbr)

  expect_equal(egs$map_codes_query$query |>
                 deparse1(),
               egs$map_codes_query$qbr |>
                 custom_qbr_translation() |>
                 deparse1())

})


