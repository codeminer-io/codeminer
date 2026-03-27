# Add a lookup table to the database

Add a lookup table to the database together with its metadata. Note that
it is not possible to overwrite an existing lookup table.

## Usage

``` r
add_lookup_table(table, metadata)
```

## Arguments

- table:

  The lookup table to add, should be coercible to a `data.frame`

- metadata:

  The lookup metadata, as specified by
  [`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md).

## Value

`TRUE` invisibly if successful, `FALSE` invisibly if the lookup table
already exists.

## Details

Lookup tables are indexed by their `code_type` and `version`, specified
in
[`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md).
This index needs to be unique and is used to identify the lookup table
in the database. If a lookup table with the same `code_type` and
`version` already exists, the function will emit a warning and return
`FALSE` (invisibly) without any effect. Use a different `version` to add
a new version of the lookup table for the given `code_type`.

## See also

[`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
for the specification of the metadata.

## Examples

``` r
# Using the example ontology data included in codeminer
lookup_table <- example_ontology$lookup_tables$capital_letters_v3
lookup_table
#> # A tibble: 9 × 2
#>   code       description    
#>   <chr>      <chr>          
#> 1 A          ALPHA          
#> 2 B          BRAVO          
#> 3 C          CHARLIE        
#> 4 D          DELTA          
#> 5 E          ECHO           
#> 6 alpha_code Alpha Attribute
#> 7 beta_code  Beta Attribute 
#> 8 gamma_code Gamma Attribute
#> 9 delta_code Delta Attribute

# Using a temporary database
Sys.setenv(CODEMINER_DB_PATH = tempfile())
build_database()
#> Creating new database at /tmp/Rtmpn29v7A/file1c8b423ad9eb
add_lookup_table(
  lookup_table,
  lookup_metadata("capital_letters", lookup_version = "v3")
)
#> ✔ Lookup table capital_letters_v3 added successfully.
```
