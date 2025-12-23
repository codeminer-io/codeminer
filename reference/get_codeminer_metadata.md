# Get codeminer metadata

Returns metadata about the lookup, mapping, and relationship tables in
the codeminer database.

## Usage

``` r
get_codeminer_metadata(type = c("lookup", "mapping", "relationship"))
```

## Arguments

- type:

  The type of metadata to return. By default returns a list containing
  all metadata types. Otherwise, returns a data frame for the specified
  type. Must be one of "lookup", "mapping" or "relationship".

## Value

If a single type is requested, a data frame. If multiple types are
requested, a named list of data frames.

## Examples

``` r
create_dummy_database()
#> Creating new database at /tmp/RtmpE2JCfn/file1c241a6c0edb.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Relationship table icd10_relationship_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
get_codeminer_metadata()
#> $lookup
#> [1] lookup_table_name               code_type                      
#> [3] lookup_version                  lookup_code_col                
#> [5] lookup_description_col          lookup_source                  
#> [7] preferred_description_col       preferred_description_indicator
#> <0 rows> (or 0-length row.names)
#> 
#> $mapping
#> [1] mapping_table_name from_code_type     to_code_type       map_version       
#> [5] from_col           to_col            
#> <0 rows> (or 0-length row.names)
#> 
#> $relationship
#> [1] relationship_table_name        code_type                     
#> [3] relationship_version           from_col                      
#> [5] to_col                         type_col                      
#> [7] child_parent_relationship_code relationship_source           
#> <0 rows> (or 0-length row.names)
#> 
get_codeminer_metadata("lookup")
#> [1] lookup_table_name               code_type                      
#> [3] lookup_version                  lookup_code_col                
#> [5] lookup_description_col          lookup_source                  
#> [7] preferred_description_col       preferred_description_indicator
#> <0 rows> (or 0-length row.names)
get_codeminer_metadata(c("lookup", "mapping"))
#> $lookup
#> [1] lookup_table_name               code_type                      
#> [3] lookup_version                  lookup_code_col                
#> [5] lookup_description_col          lookup_source                  
#> [7] preferred_description_col       preferred_description_indicator
#> <0 rows> (or 0-length row.names)
#> 
#> $mapping
#> [1] mapping_table_name from_code_type     to_code_type       map_version       
#> [5] from_col           to_col            
#> <0 rows> (or 0-length row.names)
#> 
```
