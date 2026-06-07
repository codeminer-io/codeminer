# Search for codes that match a description

Returns a codelist with clinical codes that match the provided
description pattern.

## Usage

``` r
DESCRIPTION(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  preferred_description_only = TRUE,
  col_filters = "default"
)
```

## Arguments

- pattern:

  The description to search for. See
  [`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
  for details.

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

- ignore_case:

  If `TRUE` (default), ignore case in `description`.

- preferred_description_only:

  `logical`. If `TRUE` (default), return only preferred descriptions.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

## Value

A `codeminer_codelist` with codes that match the description.

## Examples

``` r
# build dummy database
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpcKcAeV/file1b0b67e4c83f.duckdb")`
#>   `codeminer_connect()`

# lookup ICD10 code descriptions matching 'cyst'
DESCRIPTION("cyst", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description          code_type
#>   <chr> <chr>                <chr>    
#> 1 L721  Trichilemmal cyst    ICD-10   
#> 2 N330  Tuberculous cystitis ICD-10   
```
