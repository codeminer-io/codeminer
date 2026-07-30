# Retrieve parent or child codes

Returns immediate or transitive parent or child codes for the given
codes by traversing the relationship graph.

## Usage

``` r
CHILDREN(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  max_nodes = getOption("codeminer.max_traversal_nodes", default = 20000L),
  col_filters = "default"
)

PARENTS(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  max_nodes = getOption("codeminer.max_traversal_nodes", default = 20000L),
  col_filters = "default"
)

N_CHILDREN(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  max_nodes = getOption("codeminer.max_traversal_nodes", default = 20000L),
  col_filters = "default",
  call = rlang::current_env()
)

N_PARENTS(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  max_nodes = getOption("codeminer.max_traversal_nodes", default = 20000L),
  col_filters = "default",
  call = rlang::current_env()
)
```

## Arguments

- ...:

  Codes to start from. Supports flexible input like
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).

- type:

  Code type (character).

- lookup_version:

  Lookup table version (character).

- relationship_version:

  Relationship table version (character).

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- max_nodes:

  Integer. Ceiling on the number of codes the traversal can accumulate
  before aborting (checked after each generation of the traversal,
  before the next one runs). Defaults to
  `getOption("codeminer.max_traversal_nodes", default = 20000)`.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- depth:

  Integer. Maximum number of steps to traverse. Use `Inf` for transitive
  closure (all ancestors/descendants). Only used by `N_PARENTS()` and
  `N_CHILDREN()`.

- call:

  **For internal use only.** The execution environment of a currently
  running function. Used for error reporting. Users should not need to
  set this parameter.

## Value

A data frame of codes and descriptions.

## Details

Use `N_PARENTS()`/`N_CHILDREN()` for immediate relationships (one step),
and `PARENTS()`/`CHILDREN()` for transitive closure (all reachable
ancestors/descendants).

## See also

Other Code relationships:
[`attributes`](https://codeminer-io.github.io/codeminer/reference/attributes.md),
[`relationship_types`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpOTuqjN/file1a0b4d9aa414.duckdb")`
#>   `codeminer_connect()`
PARENTS("E10", "E11", type = "ICD-10")
#> ℹ Using "UKB v4" as the latest relationship version for "ICD-10".
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   
CHILDREN("E10", "E11", type = "ICD-10")
#> <codeminer_codelist>: 22 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 22 × 3
#>    code  description                                                   code_type
#>    <chr> <chr>                                                         <chr>    
#>  1 E10   Type 1 diabetes mellitus                                      ICD-10   
#>  2 E100  Type 1 diabetes mellitus With coma                            ICD-10   
#>  3 E101  Type 1 diabetes mellitus With ketoacidosis                    ICD-10   
#>  4 E102  Type 1 diabetes mellitus With renal complications             ICD-10   
#>  5 E103  Type 1 diabetes mellitus With ophthalmic complications        ICD-10   
#>  6 E104  Type 1 diabetes mellitus With neurological complications      ICD-10   
#>  7 E105  Type 1 diabetes mellitus With peripheral circulatory complic… ICD-10   
#>  8 E106  Type 1 diabetes mellitus With other specified complications   ICD-10   
#>  9 E107  Type 1 diabetes mellitus With multiple complications          ICD-10   
#> 10 E108  Type 1 diabetes mellitus With unspecified complications       ICD-10   
#> # ℹ 12 more rows
N_PARENTS("E10", "E11", type = "ICD-10")
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   
N_CHILDREN("E10", "E11", type = "ICD-10")
#> <codeminer_codelist>: 22 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 22 × 3
#>    code  description                                                   code_type
#>    <chr> <chr>                                                         <chr>    
#>  1 E10   Type 1 diabetes mellitus                                      ICD-10   
#>  2 E100  Type 1 diabetes mellitus With coma                            ICD-10   
#>  3 E101  Type 1 diabetes mellitus With ketoacidosis                    ICD-10   
#>  4 E102  Type 1 diabetes mellitus With renal complications             ICD-10   
#>  5 E103  Type 1 diabetes mellitus With ophthalmic complications        ICD-10   
#>  6 E104  Type 1 diabetes mellitus With neurological complications      ICD-10   
#>  7 E105  Type 1 diabetes mellitus With peripheral circulatory complic… ICD-10   
#>  8 E106  Type 1 diabetes mellitus With other specified complications   ICD-10   
#>  9 E107  Type 1 diabetes mellitus With multiple complications          ICD-10   
#> 10 E108  Type 1 diabetes mellitus With unspecified complications       ICD-10   
#> # ℹ 12 more rows
```
