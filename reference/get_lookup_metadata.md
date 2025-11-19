# Return the lookup metadata table as a data frame

Return the lookup metadata table as a data frame

## Usage

``` r
get_lookup_metadata(con = connect_to_db())
```

## Arguments

- con:

  A database connection object. Uses the default connection if not
  provided.

## Value

A data frame containing the lookup metadata.

## Examples

``` r
create_dummy_database()
#> ℹ Creating new database at /tmp/RtmpY524IB/file192d43e4dc19.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
get_lookup_metadata()
#> [1] lookup_table_name               code_type                      
#> [3] lookup_version                  lookup_code_col                
#> [5] lookup_description_col          lookup_source                  
#> [7] preferred_description_col       preferred_description_indicator
#> <0 rows> (or 0-length row.names)
```
