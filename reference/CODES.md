# Look up descriptions for clinical codes

Returns a data frame including descriptions for the codes of interest

## Usage

``` r
CODES(
  codes,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
)

CODES_LIKE(
  pattern,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.lookup_version", default = "latest"),
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

- version:

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
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ℹ Creating new database at /tmp/Rtmpv7eqly/file18f93f56fe4e.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!

# look up ICD10 codes
CODES(
  codes = c("E10", "E11"),
  code_type = "icd10"
)
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
CODES_LIKE("^E1", code_type = "icd10")
#> Warning: cannot open file '/home/runner/.local/share/codeminer/ontology.duckdb': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
