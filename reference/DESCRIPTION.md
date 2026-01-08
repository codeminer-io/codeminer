# Search for codes that match a description

Returns a data frame with clinical codes that match the provided
description pattern.

## Usage

``` r
DESCRIPTION(
  pattern,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
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

- lookup_version:

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
#> Creating new database at /tmp/RtmpRK6Py7/file1bd37e537787.duckdb
#> Reading 17 selected tables from UKB Resource 592
#> 
#> Extending read_v2_drugs_bnf with BNF hierarchy and descriptions
#> Extending read_v2_icd10 by expanding ICD-10 code ranges
#> Adding tables to database
#> ✔ Lookup table BNF_UKB v4 added successfully.
#> ✔ Relationship table BNF_relationship_UKB v4 added successfully.
#> ✔ Lookup table DM+D_UKB v4 added successfully.
#> ✔ Lookup table ICD-9_UKB v4 added successfully.
#> ✔ Relationship table ICD-9_relationship_UKB v4 added successfully.
#> ✔ Lookup table ICD-10_UKB v4 added successfully.
#> ✔ Relationship table ICD-10_relationship_UKB v4 added successfully.
#> ✔ Mapping table ICD-9_ICD-10_UKB v4 added successfully.
#> ✔ Lookup table Read 2_UKB v4 added successfully.
#> ✔ Relationship table Read 2_relationship_UKB v4 added successfully.
#> ✔ Lookup table Read 2, drugs_UKB v4 added successfully.
#> ✔ Mapping table Read 2, drugs_BNF_UKB v4 added successfully.
#> ✔ Mapping table Read 2_ICD-9_UKB v4 added successfully.
#> ✔ Mapping table Read 2_ICD-10_UKB v4 added successfully.
#> ✔ Mapping table Read 2_OPCS4_UKB v4 added successfully.
#> ✔ Mapping table Read 2_Read 3_UKB v4 added successfully.
#> ✔ Lookup table Read 3_UKB v4 added successfully.
#> ✔ Mapping table Read 3_ICD-9_UKB v4 added successfully.
#> ✔ Mapping table Read 3_ICD-10_UKB v4 added successfully.
#> ✔ Mapping table Read 3_OPCS4_UKB v4 added successfully.
#> ✔ Mapping table Read 3_Read 2_UKB v4 added successfully.
#> ✔ Dummy database ready to use!

# lookup ICD10 code descriptions matching 'cyst'
DESCRIPTION("cyst", code_type = "ICD-10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
