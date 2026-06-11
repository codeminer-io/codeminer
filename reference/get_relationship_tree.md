# Build a flat nodes/edges tree view for a set of codes

Given a set of seed `codes`, returns a `list(nodes, edges)` suitable for
hierarchy analysis or tree rendering (e.g. `data.tree`, `ggraph`,
`visNetwork`). By default the seed is first expanded into its full
descendant set, then the parent/child edges *among* that set are
returned alongside a node table carrying term and category.

## Usage

``` r
get_relationship_tree(
  codes,
  type = getOption("codeminer.code_type"),
  expand_to_descendants = TRUE,
  max_codes = getOption("codeminer.max_tree_codes", default = 10000),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  col_filters = "default",
  preferred_description_only = TRUE,
  con = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- codes:

  Character vector of seed codes.

- type:

  The code type (character). Can also be configured via the
  `codeminer.code_type` option.

- expand_to_descendants:

  Logical. If `TRUE` (default), `codes` are expanded to include all
  descendants via
  [`graph_closure()`](https://codeminer-io.github.io/codeminer/reference/graph_closure.md)
  before collecting edges and nodes. If `FALSE`, `codes` is used as-is.

- max_codes:

  Integer. Maximum size of the (expanded) code set. Aborts with class
  `codeminer_max_tree_codes_exceeded` if exceeded. Defaults to
  `getOption("codeminer.max_tree_codes", default = 10000)` — a guardrail
  against accidentally materialising a SNOMED-sized subgraph in R
  memory.

- relationship_version:

  Relationship table version. Defaults to `"latest"`. Can be configured
  via the `codeminer.relationship_version` option.

- lookup_version:

  Lookup table version. Defaults to `"latest"`. Can be configured via
  the `codeminer.lookup_version` option.

- col_filters:

  Column filters to apply to both the relationship and lookup tables.
  See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- preferred_description_only:

  Logical. If `TRUE` (default), node `term` is the preferred description
  only (one row per code).

- con:

  Optional DBI connection. If `NULL` (default), uses the workbench
  connection.

- call:

  The calling environment. Passed to
  [codeminer_abort](https://codeminer-io.github.io/codeminer/reference/conditions.md).

## Value

A plain `list` with two tibbles:

- `nodes`:

  `code`, `term`, `category`, `in_input_set` (logical: `TRUE` for codes
  in the original input, `FALSE` for codes added by descendant
  expansion).

- `edges`:

  `parent`, `child` — one row per hierarchical edge among the (expanded)
  code set.

Orphan codes (in the set but with no hierarchical edges) appear in
`nodes` with no rows in `edges`.

## Details

Internally composes
[`graph_closure()`](https://codeminer-io.github.io/codeminer/reference/graph_closure.md)
(for descendant expansion) and the filtered
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)
/
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md)
getters. Edges are restricted to the hierarchical relationship type
defined by `child_parent_relationship_code` in the relationship metadata
— non hierarchical edges (e.g. SNOMED "has finding site") are excluded.

## Options

- `codeminer.max_tree_codes`: default size cap for the expanded code
  set. Overridden by the `max_codes` argument.

- `codeminer.code_type`, `codeminer.lookup_version`,
  `codeminer.relationship_version`: shared with
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  /
  [`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  /
  [`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
  used as defaults for the corresponding arguments.

## See also

[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)
for raw edges,
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md)
for terms,
[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
/
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
for flat codelist traversal.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmprBSQ1f/file1ad3252af011.duckdb")`
#>   `codeminer_connect()`

tree <- get_relationship_tree("E10", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
tree$nodes
#> # A tibble: 11 × 4
#>    code  term                                              category in_input_set
#>    <chr> <chr>                                             <chr>    <lgl>       
#>  1 E10   Type 1 diabetes mellitus                          NA       TRUE        
#>  2 E100  Type 1 diabetes mellitus With coma                NA       FALSE       
#>  3 E101  Type 1 diabetes mellitus With ketoacidosis        NA       FALSE       
#>  4 E102  Type 1 diabetes mellitus With renal complications NA       FALSE       
#>  5 E103  Type 1 diabetes mellitus With ophthalmic complic… NA       FALSE       
#>  6 E104  Type 1 diabetes mellitus With neurological compl… NA       FALSE       
#>  7 E105  Type 1 diabetes mellitus With peripheral circula… NA       FALSE       
#>  8 E106  Type 1 diabetes mellitus With other specified co… NA       FALSE       
#>  9 E107  Type 1 diabetes mellitus With multiple complicat… NA       FALSE       
#> 10 E108  Type 1 diabetes mellitus With unspecified compli… NA       FALSE       
#> 11 E109  Type 1 diabetes mellitus Without complications    NA       FALSE       
tree$edges
#> # A tibble: 10 × 2
#>    parent child
#>    <chr>  <chr>
#>  1 E10    E100 
#>  2 E10    E101 
#>  3 E10    E102 
#>  4 E10    E103 
#>  5 E10    E104 
#>  6 E10    E105 
#>  7 E10    E106 
#>  8 E10    E107 
#>  9 E10    E108 
#> 10 E10    E109 

# `in_input_set` flags the seed apart from expanded descendants
subset(tree$nodes, in_input_set)
#> # A tibble: 1 × 4
#>   code  term                     category in_input_set
#>   <chr> <chr>                    <chr>    <lgl>       
#> 1 E10   Type 1 diabetes mellitus NA       TRUE        
```
