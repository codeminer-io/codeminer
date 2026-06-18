suppressMessages(create_dummy_database(.local = TRUE))

test_that("a purely hierarchical table traverses every edge (no type filter)", {
  test_type <- "pure_hier"

  add_lookup_table(
    data.frame(
      code = c("root", "p1", "p2", "child", "leaf"),
      description = c("Root", "Parent 1", "Parent 2", "Child", "Leaf")
    ),
    lookup_metadata(test_type)
  )

  # `child` has two parents (p1, p2). The table is purely hierarchical: no type
  # column, so `relationship_metadata()` leaves the type fields NA and traversal
  # must use every edge (this is the Read-v3 multi-parent case).
  add_relationship_table(
    data.frame(
      from = c("p1", "p2", "child", "child", "leaf"),
      to = c("root", "root", "p1", "p2", "child")
    ),
    relationship_metadata(test_type)
  )

  # Full ancestry of `child` is returned, including BOTH parents (not just the
  # first edge) and their shared grandparent.
  parents <- PARENTS("child", type = test_type)
  expect_setequal(parents$code, c("child", "p1", "p2", "root"))

  # Full descendant closure is returned for each parent.
  expect_setequal(
    CHILDREN("p1", type = test_type)$code,
    c("p1", "child", "leaf")
  )
  expect_setequal(
    CHILDREN("p2", type = test_type)$code,
    c("p2", "child", "leaf")
  )

  # And from the root, the whole tree is reachable.
  expect_setequal(
    CHILDREN("root", type = test_type)$code,
    c("root", "p1", "p2", "child", "leaf")
  )
})

test_that("type-dimension functions abort on a purely hierarchical table", {
  test_type <- "pure_hier_guard"

  add_lookup_table(
    data.frame(code = c("a", "b"), description = c("A", "B")),
    lookup_metadata(test_type)
  )
  add_relationship_table(
    data.frame(from = "a", to = "b"),
    relationship_metadata(test_type)
  )

  expect_error(
    RELATIONSHIP_TYPES_FROM("a", type = test_type),
    class = "codeminer_no_relationship_types"
  )
  expect_error(
    RELATIONSHIP_TYPES_TO("b", type = test_type),
    class = "codeminer_no_relationship_types"
  )
  expect_error(
    ATTRIBUTES_FOR("a", type = test_type),
    class = "codeminer_no_relationship_types"
  )
  expect_error(
    HAS_ATTRIBUTES("b", type = test_type),
    class = "codeminer_no_relationship_types"
  )

  # Hierarchy traversal remains valid on the same table.
  expect_no_error(CHILDREN("b", type = test_type))
})

test_that("a multi-type table still filters traversal to the hierarchical type", {
  test_type <- "multi_type"

  add_lookup_table(
    data.frame(
      code = c("parent", "child", "attr"),
      description = c("Parent", "Child", "Attribute")
    ),
    lookup_metadata(test_type)
  )

  # `child` is a hierarchical child of `parent`; `child` also has an attribute
  # edge to `attr` that must NOT be followed by CHILDREN/PARENTS.
  add_relationship_table(
    data.frame(
      from = c("child", "child"),
      to = c("parent", "attr"),
      type = c("is a", "has attribute")
    ),
    relationship_metadata(
      test_type,
      type_col = "type",
      child_parent_relationship_code = "is a"
    )
  )

  expect_setequal(
    CHILDREN("parent", type = test_type)$code,
    c("parent", "child")
  )
  # `attr` is reached only via a non-hierarchical edge, so it is not a parent.
  expect_setequal(PARENTS("child", type = test_type)$code, c("child", "parent"))
})
