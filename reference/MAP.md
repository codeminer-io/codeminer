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

  Column filters to apply to the **mapping table**. One of:

  - `"default"` (default): apply session-pinned or metadata-defined
    default filters

  - `NULL`: no filtering (return all rows)

  - A named list of `column_name = c(values)` pairs for explicit
    filtering

  Note: this controls filtering of the mapping table, not the target
  lookup table (which uses its own default col_filters).

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

## See also

[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
for adding new mapping tables to the codeminer database.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpGZ1UM7/file1cd73dd08343.duckdb")`
#>   `codeminer_connect()`

# Single code
MAP("X40J4", from = "Read 3", to = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# Multiple codes
MAP("X40J4", "X40J5", from = "Read 3", to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# || separated
MAP("X40J4 || X40J5", from = "Read 3", to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
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
  code_type = c("Read 3", "Read 3")
)
MAP(df, to = "ICD-10")
#> Warning: ! The following codes were not found in the mapping table:
#> • `X40J5`
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10   

# Return the mapping table itself
MAP("all", from = "Read 3", to = "ICD-10")
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
