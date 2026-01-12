# Look up descriptions for clinical codes

Returns a data frame including descriptions for the codes of interest

## Usage

``` r
CODES(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
)

CODES_LIKE(
  pattern,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
)
```

## Arguments

- codes:

  character. Vector of codes to lookup. If passing `"all"`, returns all
  codes.

- code_type:

  character. Type of clinical code system to be searched. Depends on
  what is available in the lookup tables. See
  [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  on how to add new lookup tables. This can also be configured through
  the `codeminer.code_type` option.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

- pattern:

  a regular expression to search for

## Value

A `data.frame` containing the codes and their descriptions

## Details

`CODES_LIKE` searches for codes that match a given regular expression.
The matching is case-insensitive.

## See also

[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
for adding new lookup tables to the database.

Other Clinical code lookups and mappings:
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> Creating new database at /tmp/RtmpLBgdnU/file1c653da2465f.duckdb
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

# look up ICD10 codes
CODES(
  codes = c("E10", "E11"),
  code_type = "ICD-10"
)
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
CODES_LIKE("^E1", code_type = "ICD-10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
