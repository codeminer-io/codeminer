test_that(
  "`validate_codeminer_calls()` raises error for query statements with non-codeminer functions",
  {
    expect_equal(
      validate_codeminer_calls(quote(
        DESCRIPTION("diab") %AND% DESCRIPTION("type 1") %OR%
          (CHILDREN("E10") %OR% CODES("O240"))
      )),
      c("%OR%", "%AND%", "DESCRIPTION", "(", "CHILDREN", "CODES")
    )

    expect_equal(
      validate_codeminer_calls(rlang::parse_expr(
        'RESULT = DESCRIPTION("diab") %AND% DESCRIPTION("type 1") %OR%
          (CHILDREN("E10") %OR% CODES("O240"))'
      )),
      c("=", "%OR%", "%AND%", "DESCRIPTION", "(", "CHILDREN", "CODES")
    )

    expect_equal(
      validate_codeminer_calls(rlang::parse_expr(
        'RESULT <- DESCRIPTION("diab") %AND% DESCRIPTION("type 1") %OR%
          (CHILDREN("E10") %OR% CODES("O240"))'
      )),
      c("<-", "%OR%", "%AND%", "DESCRIPTION", "(", "CHILDREN", "CODES")
    )

    expect_error(
      validate_codeminer_calls(quote(base::mean(1:3))),
      "Invalid function calls found: `mean`, `:`. These functions are not exported by codeminer"
    )

    expect_error(
      validate_codeminer_calls(quote(mean(1:3))),
      "Invalid function calls found: `mean`, `:`. These functions are not exported by codeminer"
    )

    expect_error(
      validate_codeminer_calls(quote(base:::abs(-1))),
      "Invalid function calls found: `abs`, `-`. These functions are not exported by codeminer"
    )
  }
)
