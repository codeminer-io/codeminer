# Look up descriptions for clinical codes

Returns a codelist with descriptions for the codes of interest. Supports
flexible input: character vectors, `||` separated strings, or data
frames.

## Usage

``` r
CODES(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)

CODES_LIKE(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)
```

## Arguments

- ...:

  Codes to look up. Can be:

  - Character vectors: `CODES("E10", "E11", type = "ICD-10")`

  - `||` separated strings: `CODES("E10 || E11", type = "ICD-10")`

  - Data frame with code/description/code_type columns: `CODES(my_df)`

  - Mixed: `CODES("E10", my_vector, "E13 || E14", type = "ICD-10")`

  Special values: `"all"` returns all codes; empty input returns empty
  codelist.

  Comments can be added with `<< >>` syntax:
  `"E10 << Type 1 diabetes >>"`.

- type:

  character. Type of clinical code system to be searched. Optional if
  input is a data frame with code_type column. Depends on what is
  available in the lookup tables. See
  [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  on how to add new lookup tables. This can also be configured through
  the `codeminer.code_type` option.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

- col_filters:

  Column filters to apply. One of:

  - `"default"` (default): apply session-pinned or metadata-defined
    default filters

  - `NULL`: no filtering (return all rows)

  - A named list of `column_name = c(values)` pairs for explicit
    filtering

- pattern:

  a regular expression to search for

## Value

A `codeminer_codelist` object (tibble) containing the codes and their
descriptions

## Details

`CODES_LIKE` searches for codes that match a given regular expression.
The matching is case-insensitive.

## See also

[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
for adding new lookup tables to the database.

Other Clinical code lookups and mappings:
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ✔ Dummy database ready to use!

# Multiple arguments
CODES("E10", "E11", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   

# With comments
CODES("E10 << Type 1 diabetes >>", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   

# || separated string
CODES("E10 || E11", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   

# Splice operator
my_codes <- c("E10", "E11")
CODES(!!!my_codes, type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   

# Data frame input
df <- data.frame(
  code = c("E10", "E11"),
  description = c("Type 1", "Type 2"),
  code_type = c("ICD-10", "ICD-10")
)
CODES(df)
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10   
CODES_LIKE("^E1", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 55 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 55 × 3
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
#> # ℹ 45 more rows
```
