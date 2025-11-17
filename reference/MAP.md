# Map clinical codes from one coding system to another

Map clinical codes from one coding system to another

## Usage

``` r
MAP(
  codes,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  version = getOption("codeminer.map_version", default = "latest")
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

- version:

  Version of the mapping table to use.

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
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ℹ Creating new database at /tmp/RtmprDVbPc/file190157a83ca8.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!

MAP("X40J4", from = "read3", to = "icd10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection

# Return the mapping table itself
MAP("all", from = "read3", to = "icd10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
