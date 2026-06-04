# Get the full lookup table for a code type

Returns a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
containing the lookup table with standardised column names (`code`,
`description`, `code_type`, `preferred_description`) plus all additional
columns from the underlying database table. Call
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to materialise the result.

## Usage

``` r
get_lookup_table(
  type,
  lookup_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- type:

  The code type for which to retrieve the lookup table.

- lookup_version:

  The version to retrieve. Defaults to `"latest"`.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- con:

  Optional DBI connection. If `NULL` (default), uses the workbench
  connection.

- call:

  The calling environment. Passed to
  [codeminer_abort](https://codeminer-io.github.io/codeminer/reference/conditions.md).

## Value

A lazy [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
with standardised columns (`code`, `description`, `code_type`,
`preferred_description`) plus all other columns from the underlying
table.

## Details

This is useful for inspecting columns beyond the standard codelist
output returned by
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
and
[`DESCRIPTION()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION.md).

## See also

[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
for standardised codelist output,
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
for discovering available tables.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/Rtmpc15fMD/file1b4d32cd80fd.duckdb")`
#>   `codeminer_connect()`

# Get the full ICD-10 lookup table
get_lookup_table("ICD-10") |> dplyr::collect()
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 197 × 14
#>    code  description  ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>    <chr> <chr>        <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#>  1 A00   Cholera      A00        DEFA… 3        NA         NA         NA        
#>  2 A000  Cholera due… A00.0      DEFA… 3        NA         NA         NA        
#>  3 A001  Cholera due… A00.1      DEFA… 3        NA         NA         NA        
#>  4 A009  Cholera, un… A00.9      DEFA… 3        NA         NA         NA        
#>  5 A010  Typhoid fev… A01.0      DEFA… 3        NA         NA         NA        
#>  6 A02   Other salmo… A02        DEFA… 3        NA         NA         NA        
#>  7 A020  Salmonella … A02.0      DEFA… 3        NA         NA         NA        
#>  8 A021  Salmonella … A02.1      DEFA… 3        NA         NA         NA        
#>  9 A022  Localized s… A02.2      DEFA… 3        NA         NA         NA        
#> 10 A028  Other speci… A02.8      DEFA… 3        NA         NA         NA        
#> # ℹ 187 more rows
#> # ℹ 6 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>

# Inspect raw columns for specific codes
result <- CODES("E10", "E11", type = "ICD-10")
get_lookup_table("ICD-10") |>
  dplyr::filter(code %in% .env$result$code) |>
  dplyr::collect()
#> # A tibble: 2 × 14
#>   code  description   ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>   <chr> <chr>         <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#> 1 E10   Type 1 diabe… E10        DEFA… 3        NA         NA         NA        
#> 2 E11   Type 2 diabe… E11        DEFA… 3        NA         NA         NA        
#> # ℹ 6 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>
```
