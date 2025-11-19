# Search for codes that match a description

Returns a data frame with clinical codes that match the provided
description pattern.

## Usage

``` r
DESCRIPTION(
  pattern,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  codes_only = FALSE,
  preferred_description_only = TRUE
)
```

## Arguments

- pattern:

  The description to search for. See
  [`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
  for details.

- code_type:

  character. Type of clinical code system to be searched. Depends on
  what is available in the lookup tables. See
  [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  on how to add new lookup tables. This can also be configured through
  the `codeminer.code_type` option.

- version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- ignore_case:

  If `TRUE` (default), ignore case in `description`.

- codes_only:

  `logical`. If `TRUE`, return a character vector of *unique* codes. If
  `FALSE` (default), return a data frame of all results including code
  descriptions (useful for manual validation).

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

The result of
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
for codes that match the description, or a character vector of codes if
`codes_only` is `TRUE`.

## Examples

``` r
# build dummy database
create_dummy_database()
#> ℹ Creating new database at /tmp/RtmpY524IB/file192d25a23f87.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!

# lookup ICD10 code descriptions matching 'cyst'
DESCRIPTION("cyst", code_type = "icd10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
