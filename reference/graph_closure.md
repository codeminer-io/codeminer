# Perform transitive closure graph traversal

Returns all nodes reachable from the input nodes by recursively
following edges until no new nodes are discovered.

## Usage

``` r
graph_closure(
  nodes,
  relationship_tbl,
  from_colname = "from_col",
  to_colname = "to_col",
  type_colname = "type_col",
  direction = c("out", "in"),
  rel_type = NULL,
  include_self = FALSE,
  max_depth = Inf,
  max_nodes = getOption("codeminer.max_traversal_nodes", default = 20000L),
  call = rlang::caller_env()
)
```

## Arguments

- nodes:

  Character vector of node IDs to start from.

- relationship_tbl:

  A `dbplyr` table containing relationship data.

- from_colname:

  Name of column containing 'from' nodes (e.g. child).

- to_colname:

  Name of column containing 'to' nodes (e.g. parent).

- type_colname:

  Name of column containing relationship type.

- direction:

  Either `"out"` (follow edges from nodes) or `"in"` (follow edges to
  nodes).

- rel_type:

  Character vector of relationship types to filter by. If `NULL`, all
  types are included.

- include_self:

  Logical. If `TRUE`, include the starting nodes in the result.

- max_depth:

  Integer. Maximum number of steps to traverse. Default is `Inf`
  (complete traversal).

## Value

Character vector of all reachable node IDs.
