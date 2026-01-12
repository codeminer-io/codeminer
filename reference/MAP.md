# Map clinical codes from one coding system to another

Map clinical codes from one coding system to another

## Usage

``` r
MAP(
  codes,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  map_version = getOption("codeminer.map_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest")
)
```

## Arguments

- codes:

  A character vector of codes to be mapped. If passing `"all"`, all
  mapped codes will be returned.

- from:

  Coding system that `codes` belong to.

- to:

  Coding system to map `codes` to.

- map_version:

  Version of the mapping table to use.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

## Value

A `data.frame` of the mapped codes with their descriptions, as returned
by
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).

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
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> Creating new database at /tmp/RtmpC7qugX/file1bbec1b067.duckdb
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

MAP("X40J4", from = "Read 3", to = "ICD-10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection

# Return the mapping table itself
MAP("all", from = "Read 3", to = "ICD-10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
