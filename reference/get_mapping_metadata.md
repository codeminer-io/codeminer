# Return the mapping metadata table as a data frame

Return the mapping metadata table as a data frame

## Usage

``` r
get_mapping_metadata(con = connect_to_db())
```

## Arguments

- con:

  A database connection object. Uses the default connection if not
  provided.

## Value

A data frame containing the mapping metadata.

## Examples

``` r
create_dummy_database()
#> ℹ Creating new database at /tmp/RtmpZPw1IG/file192a4dd6f145.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
get_mapping_metadata()
#> [1] mapping_table_name from_code_type     to_code_type       mapping_version   
#> [5] from_col           to_col            
#> <0 rows> (or 0-length row.names)
```
