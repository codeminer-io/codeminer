# Map clinical codes from one coding system to another

Map clinical codes from one coding system to another

## Usage

``` r
MAP(
  ...,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  map_version = getOption("codeminer.map_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  col_filters = "default"
)
```

## Arguments

- ...:

  Codes to map. Supports flexible input like
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).
  Special value: `"all"` returns all mapped codes.

- from:

  Coding system that `...` codes belong to. Optional if input is a
  codelist with code_type.

- to:

  Coding system to map codes to.

- map_version:

  Version of the mapping table to use.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- col_filters:

  Column filters for the tables this query touches. One of:

  - `"default"` (default): apply session-pinned filters
    ([`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md))
    where set, else the metadata-defined default filters.

  - `NULL` (or `NA`): no filtering for any table this query touches.

  - A table-keyed list — the shape returned by
    [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
    — with top-level names `lookup` / `relationship` / `mapping`, keyed
    by code type (or `"from > to"` pair for mappings), e.g.
    `list(lookup = list("SNOMED CT" = list(active_concept = "1")))`.
    Each table entry *replaces* that table's pinned/default filters
    wholesale; tables the list does not name keep their pins/defaults.
    `NA` as a table entry un-filters that one table. The filters reach
    every table the query touches (e.g. both the mapping table and the
    target lookup in `MAP()`).

  To tweak one column while keeping a table's other default filters,
  amend
  [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
  output and pass it back. Entries that match no registered table or
  column trigger a warning.

## Value

A `codeminer_codelist` of the mapped codes with their descriptions.

If using `codes = "all"`, returns the mapping table as a `data.frame`
with columns:

- `from`: the codes from the source code system

- `to`: the mapped codes from the destination system

## Details

If no mapping table matching the `from -> to` direction is found, but
there is a table for `to -> from`, `MAP()` will return the reverse
mapping with a warning. Note that this is not guaranteed to be correct,
as most mapping tables only work one way.

`MAP()` touches two tables: the mapping table (keyed `"from > to"` under
`mapping`) and the target lookup (keyed by `to` under `lookup`). The
`col_filters` argument reaches both, so the target lookup can be
restricted alongside the mapping table in one call; `col_filters = NULL`
un-filters both.

## See also

[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
for adding new mapping tables to the codeminer database.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md),
[`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpGGV5Wj/file1a4e6cd134cd.duckdb")`
#>   `codeminer_connect()`

# Single code
MAP("X40J4", from = "Read v3", to = "ICD-10")
#> ℹ Using "UKB v4" as the latest mapping version for "Read v3 > ICD-10".
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# Multiple codes
MAP("X40J4", "X40J5", from = "Read v3", to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
#> ℹ Active column filters may exclude codes - see `codeminer_status()` and the
#>   `col_filters` argument.
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# || separated
MAP("X40J4 || X40J5", from = "Read v3", to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
#> ℹ Active column filters may exclude codes - see `codeminer_status()` and the
#>   `col_filters` argument.
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# Data frame input (from is optional)
df <- data.frame(
  code = c("X40J4", "X40J5"),
  description = c("Desc 1", "Desc 2"),
  code_type = c("Read v3", "Read v3")
)
MAP(df, to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
#> ℹ Active column filters may exclude codes - see `codeminer_status()` and the
#>   `col_filters` argument.
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# Return the mapping table itself
MAP("all", from = "Read v3", to = "ICD-10")
#> # A tibble: 5 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 X40J4 E109  D              C           P             0           0        
#> 2 C10.. E149  D              C           C             0           0        
#> 3 XaIP9 L721  D              C           C             0           0        
#> 4 XE0e0 N390  D              C           P             0           0        
#> 5 XE0Uc I10   D              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```
