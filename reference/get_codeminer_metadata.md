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
#> Creating new database at /tmp/RtmpC7qugX/file1bbe2e8948b8.duckdb
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
#> [5] from_col           to_col             map_source        
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
#> [5] from_col           to_col             map_source        
#> <0 rows> (or 0-length row.names)
#> 
```
