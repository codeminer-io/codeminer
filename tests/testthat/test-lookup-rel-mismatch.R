suppressMessages(create_dummy_database(.local = TRUE))

# Collect every codeminer_missing_codes warning raised while evaluating `expr`,
# muffling them so they don't surface as test noise.
collect_missing_code_warnings <- function(expr) {
  state <- new.env(parent = emptyenv())
  state$warns <- list()
  withCallingHandlers(
    force(expr),
    codeminer_missing_codes = function(w) {
      state$warns <- c(state$warns, list(w))
      invokeRestart("muffleWarning")
    }
  )
  state$warns
}

test_that("relationship traversal enriches a lookup miss and does not double-warn", {
  tt <- "mismatch_rel_lookup"
  suppressMessages(add_lookup_table(
    data.frame(code = "root", description = "Root"),
    lookup_metadata(tt)
  ))
  # `orphan` is in the relationship table but absent from the lookup.
  suppressMessages(add_relationship_table(
    data.frame(from = "orphan", to = "root", type = "is a"),
    relationship_metadata(
      tt,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  ))

  warns <- collect_missing_code_warnings(
    suppressMessages(CHILDREN("root", type = tt))
  )
  lookup_warns <- Filter(function(w) identical(w$table_type, "lookup"), warns)

  # Exactly one lookup warning (the generic one is muffled, not duplicated).
  expect_length(lookup_warns, 1)
  expect_match(
    conditionMessage(lookup_warns[[1]]),
    "absent from its lookup table"
  )
  expect_match(conditionMessage(lookup_warns[[1]]), "built together")
  # Structured fields survive the re-raise.
  expect_setequal(lookup_warns[[1]]$missing_codes, "orphan")
  expect_s3_class(lookup_warns[[1]], "codeminer_missing_codes")
})

test_that("input codes absent from the relationship table are classified", {
  tt <- "mismatch_input"
  suppressMessages(add_lookup_table(
    data.frame(
      code = c("root", "lonely"),
      description = c("Root", "Lonely")
    ),
    lookup_metadata(tt)
  ))
  suppressMessages(add_relationship_table(
    data.frame(from = "orphan", to = "root", type = "is a"),
    relationship_metadata(
      tt,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  ))

  # `lonely` is in the lookup but not the relationship table; `ghost` is in
  # neither.
  warns <- collect_missing_code_warnings(
    suppressMessages(CHILDREN(c("lonely", "ghost"), type = tt))
  )
  rel_warns <- Filter(
    function(w) identical(w$table_type, "relationship"),
    warns
  )

  expect_length(rel_warns, 1)
  msg <- conditionMessage(rel_warns[[1]])
  # `lonely` is in the lookup but missing from the relationship table.
  expect_match(msg, "lookup table but absent from its relationship table")
  # `ghost` is in neither.
  expect_match(msg, "not in the .* lookup table either")
})

test_that("MAP enriches a lookup miss in the mapped target", {
  suppressMessages(add_lookup_table(
    data.frame(code = c("s1", "s2"), description = c("Src 1", "Src 2")),
    lookup_metadata("mm_src")
  ))
  suppressMessages(add_lookup_table(
    data.frame(code = "t1", description = "Tgt 1"),
    lookup_metadata("mm_tgt")
  ))
  # s2 maps to a target absent from the mm_tgt lookup.
  suppressMessages(add_mapping_table(
    data.frame(from = c("s1", "s2"), to = c("t1", "t_missing")),
    mapping_metadata("mm_src", "mm_tgt", map_version = "v0")
  ))

  warns <- collect_missing_code_warnings(
    suppressMessages(MAP(c("s1", "s2"), from = "mm_src", to = "mm_tgt"))
  )
  lookup_warns <- Filter(function(w) identical(w$table_type, "lookup"), warns)

  expect_length(lookup_warns, 1)
  expect_match(
    conditionMessage(lookup_warns[[1]]),
    "mapping table but absent from the mm_tgt lookup table"
  )
  expect_setequal(lookup_warns[[1]]$missing_codes, "t_missing")
})

test_that("no enrichment fires when lookup and relationship are consistent", {
  tt <- "mismatch_clean"
  suppressMessages(add_lookup_table(
    data.frame(
      code = c("parent", "child"),
      description = c("Parent", "Child")
    ),
    lookup_metadata(tt)
  ))
  suppressMessages(add_relationship_table(
    data.frame(from = "child", to = "parent", type = "is a"),
    relationship_metadata(
      tt,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  ))

  warns <- collect_missing_code_warnings(
    suppressMessages(CHILDREN("parent", type = tt))
  )
  expect_length(warns, 0)
})
