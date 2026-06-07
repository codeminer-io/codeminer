# Add a mapping table to the database

Add a mapping table to the database together with its metadata. Note
that it is not possible to overwrite an existing mapping table.

## Usage

``` r
add_mapping_table(table, metadata)
```

## Arguments

- table:

  The mapping table to add, should be coercible to a `data.frame`

- metadata:

  The mapping metadata, as specified by
  [`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md).

## Value

`TRUE` invisibly if successful, `FALSE` invisibly if the mapping table
already exists.

## Details

Mapping tables are indexed by the combination of `from_code_type`,
`to_code_type`, and `map_version`, specified in
[`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md).
This index needs to be unique and is used to identify the mapping table
in the database. If a mapping table with the same index already exists,
the function will emit a warning and return `FALSE` (invisibly) without
any effect. Use a different `map_version` to add a new version of the
mapping table.

## See also

[`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md)
for the specification of the metadata.

## Examples

``` r
# Using the example ontology data included in codeminer
mapping_table <- example_ontology$mapping_tables$capital_to_lowercase_v3
mapping_table
#> # A tibble: 5 × 2
#>   capital lowercase
#>   <chr>   <chr>    
#> 1 A       a        
#> 2 B       b        
#> 3 C       c        
#> 4 D       d        
#> 5 E       e        

# Using a temporary database
Sys.setenv(CODEMINER_DB_PATH = tempfile())
build_database()
#> Creating new database at /tmp/RtmpElJchn/file1add86bd022
add_mapping_table(mapping_table, mapping_metadata("capital", "lowercase", map_version = "v3"))
#> ✔ Mapping table capital_lowercase_v3 added successfully.
```
