# Get codeminer metadata

Returns metadata about the lookup, mapping, and relationship tables in
the codeminer database.

## Usage

``` r
get_codeminer_metadata(
  type = c("lookup", "mapping", "relationship"),
  con = NULL
)
```

## Arguments

- type:

  The type of metadata to return. By default returns a list containing
  all metadata types. Otherwise, returns a data frame for the specified
  type. Must be one of "lookup", "mapping" or "relationship".

- con:

  Optional DBI connection. If `NULL` (default), uses the workbench
  connection.

## Value

If a single type is requested, a data frame. If multiple types are
requested, a named list of data frames.

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpPIcf1N/file1b3316adcf85.duckdb")`
#>   `codeminer_connect()`
get_codeminer_metadata()
#> $lookup
#>      lookup_table_name     code_type lookup_version lookup_code_col
#> 1           BNF_UKB v4           BNF         UKB v4        BNF_Code
#> 2          DM+D_UKB v4          DM+D         UKB v4      concept_id
#> 3         ICD-9_UKB v4         ICD-9         UKB v4            ICD9
#> 4        ICD-10_UKB v4        ICD-10         UKB v4        ALT_CODE
#> 5       Read v2_UKB v4       Read v2         UKB v4       read_code
#> 6 Read v2 drugs_UKB v4 Read v2 drugs         UKB v4       read_code
#> 7       Read v3_UKB v4       Read v3         UKB v4       read_code
#>   lookup_description_col lookup_category_col
#> 1            Description         BNF_Chapter
#> 2                   term                <NA>
#> 3       DESCRIPTION_ICD9                <NA>
#> 4            DESCRIPTION                <NA>
#> 5       term_description                <NA>
#> 6       term_description                <NA>
#> 7       term_description                <NA>
#>                                        lookup_source preferred_description_col
#> 1 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 2 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 3 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 4 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 5 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                 term_code
#> 6 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 7 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592          description_type
#>   preferred_description_indicator col_filters
#> 1                            <NA>        <NA>
#> 2                            <NA>        <NA>
#> 3                            <NA>        <NA>
#> 4                            <NA>        <NA>
#> 5                              00        <NA>
#> 6                            <NA>        <NA>
#> 7                               P        <NA>
#> 
#> $mapping
#>          mapping_table_name from_code_type to_code_type map_version    from_col
#> 1       ICD-9_ICD-10_UKB v4          ICD-9       ICD-10      UKB v4        ICD9
#> 2  Read v2 drugs_BNF_UKB v4  Read v2 drugs          BNF      UKB v4   read_code
#> 3      Read v2_ICD-9_UKB v4        Read v2        ICD-9      UKB v4   read_code
#> 4     Read v2_ICD-10_UKB v4        Read v2       ICD-10      UKB v4   read_code
#> 5     Read v2_OPCS-4_UKB v4        Read v2       OPCS-4      UKB v4   read_code
#> 6    Read v2_Read v3_UKB v4        Read v2      Read v3      UKB v4 READV2_CODE
#> 7      Read v3_ICD-9_UKB v4        Read v3        ICD-9      UKB v4   read_code
#> 8     Read v3_ICD-10_UKB v4        Read v3       ICD-10      UKB v4   read_code
#> 9     Read v3_OPCS-4_UKB v4        Read v3       OPCS-4      UKB v4   read_code
#> 10   Read v3_Read v2_UKB v4        Read v3      Read v2      UKB v4 READV3_CODE
#>           to_col                                         map_source
#> 1          ICD10 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 2       bnf_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 3      icd9_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 4     icd10_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 5  opcs_4.2_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 6    READV3_CODE https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 7      icd9_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 8     icd10_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 9     opcs4_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 10   READV2_CODE https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#>                                                                                                                                                                                                                                                                                                            col_filters
#> 1                                                                                                                                                                                                                                                                                                                 <NA>
#> 2                                                                                                                                                                                                                                                                                                                 <NA>
#> 3                                                                                                                                                                                                                                                                                                                 <NA>
#> 4                                                                                                                                                                                                                   {"icd10_code_def":{"values":["1","15","3","5","7","8","2"],"defaults":["1","15","3","5","7","8"]}}
#> 5                                                                                                                                                                                                                                                                                                                 <NA>
#> 6                                                                                                                                                                                                                                                                     {"IS_ASSURED":{"values":["1"],"defaults":["1"]}}
#> 7                                                                                                                                                                                                                                                                                                                 <NA>
#> 8  {"mapping_status":{"values":["E","G","D","R","A","U"],"defaults":["E","G","D"]},"refine_flag":{"values":["C","P","M"],"defaults":["C","P"]},"element_num":{"values":["0","1","2","3"],"defaults":["0"]},"block_num":{"values":["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14"],"defaults":["0"]}}
#> 9                                                                                                                                                                                                                                                                                                                 <NA>
#> 10                                                                                                                                                                                                                                                                    {"IS_ASSURED":{"values":["1"],"defaults":["1"]}}
#> 
#> $relationship
#>       relationship_table_name code_type relationship_version from_col to_col
#> 1     BNF_relationship_UKB v4       BNF               UKB v4     from     to
#> 2   ICD-9_relationship_UKB v4     ICD-9               UKB v4     from     to
#> 3  ICD-10_relationship_UKB v4    ICD-10               UKB v4     from     to
#> 4 Read v2_relationship_UKB v4   Read v2               UKB v4     from     to
#>   type_col child_parent_relationship_code
#> 1     type                           is a
#> 2     type                           is a
#> 3     type                           is a
#> 4     type                           is a
#>                                  relationship_source col_filters
#> 1 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592        <NA>
#> 2 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592        <NA>
#> 3 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592        <NA>
#> 4 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592        <NA>
#> 
get_codeminer_metadata("lookup")
#>      lookup_table_name     code_type lookup_version lookup_code_col
#> 1           BNF_UKB v4           BNF         UKB v4        BNF_Code
#> 2          DM+D_UKB v4          DM+D         UKB v4      concept_id
#> 3         ICD-9_UKB v4         ICD-9         UKB v4            ICD9
#> 4        ICD-10_UKB v4        ICD-10         UKB v4        ALT_CODE
#> 5       Read v2_UKB v4       Read v2         UKB v4       read_code
#> 6 Read v2 drugs_UKB v4 Read v2 drugs         UKB v4       read_code
#> 7       Read v3_UKB v4       Read v3         UKB v4       read_code
#>   lookup_description_col lookup_category_col
#> 1            Description         BNF_Chapter
#> 2                   term                <NA>
#> 3       DESCRIPTION_ICD9                <NA>
#> 4            DESCRIPTION                <NA>
#> 5       term_description                <NA>
#> 6       term_description                <NA>
#> 7       term_description                <NA>
#>                                        lookup_source preferred_description_col
#> 1 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 2 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 3 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 4 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 5 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                 term_code
#> 6 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 7 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592          description_type
#>   preferred_description_indicator col_filters
#> 1                            <NA>        <NA>
#> 2                            <NA>        <NA>
#> 3                            <NA>        <NA>
#> 4                            <NA>        <NA>
#> 5                              00        <NA>
#> 6                            <NA>        <NA>
#> 7                               P        <NA>
get_codeminer_metadata(c("lookup", "mapping"))
#> $lookup
#>      lookup_table_name     code_type lookup_version lookup_code_col
#> 1           BNF_UKB v4           BNF         UKB v4        BNF_Code
#> 2          DM+D_UKB v4          DM+D         UKB v4      concept_id
#> 3         ICD-9_UKB v4         ICD-9         UKB v4            ICD9
#> 4        ICD-10_UKB v4        ICD-10         UKB v4        ALT_CODE
#> 5       Read v2_UKB v4       Read v2         UKB v4       read_code
#> 6 Read v2 drugs_UKB v4 Read v2 drugs         UKB v4       read_code
#> 7       Read v3_UKB v4       Read v3         UKB v4       read_code
#>   lookup_description_col lookup_category_col
#> 1            Description         BNF_Chapter
#> 2                   term                <NA>
#> 3       DESCRIPTION_ICD9                <NA>
#> 4            DESCRIPTION                <NA>
#> 5       term_description                <NA>
#> 6       term_description                <NA>
#> 7       term_description                <NA>
#>                                        lookup_source preferred_description_col
#> 1 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 2 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 3 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 4 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 5 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                 term_code
#> 6 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 7 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592          description_type
#>   preferred_description_indicator col_filters
#> 1                            <NA>        <NA>
#> 2                            <NA>        <NA>
#> 3                            <NA>        <NA>
#> 4                            <NA>        <NA>
#> 5                              00        <NA>
#> 6                            <NA>        <NA>
#> 7                               P        <NA>
#> 
#> $mapping
#>          mapping_table_name from_code_type to_code_type map_version    from_col
#> 1       ICD-9_ICD-10_UKB v4          ICD-9       ICD-10      UKB v4        ICD9
#> 2  Read v2 drugs_BNF_UKB v4  Read v2 drugs          BNF      UKB v4   read_code
#> 3      Read v2_ICD-9_UKB v4        Read v2        ICD-9      UKB v4   read_code
#> 4     Read v2_ICD-10_UKB v4        Read v2       ICD-10      UKB v4   read_code
#> 5     Read v2_OPCS-4_UKB v4        Read v2       OPCS-4      UKB v4   read_code
#> 6    Read v2_Read v3_UKB v4        Read v2      Read v3      UKB v4 READV2_CODE
#> 7      Read v3_ICD-9_UKB v4        Read v3        ICD-9      UKB v4   read_code
#> 8     Read v3_ICD-10_UKB v4        Read v3       ICD-10      UKB v4   read_code
#> 9     Read v3_OPCS-4_UKB v4        Read v3       OPCS-4      UKB v4   read_code
#> 10   Read v3_Read v2_UKB v4        Read v3      Read v2      UKB v4 READV3_CODE
#>           to_col                                         map_source
#> 1          ICD10 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 2       bnf_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 3      icd9_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 4     icd10_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 5  opcs_4.2_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 6    READV3_CODE https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 7      icd9_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 8     icd10_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 9     opcs4_code https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 10   READV2_CODE https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#>                                                                                                                                                                                                                                                                                                            col_filters
#> 1                                                                                                                                                                                                                                                                                                                 <NA>
#> 2                                                                                                                                                                                                                                                                                                                 <NA>
#> 3                                                                                                                                                                                                                                                                                                                 <NA>
#> 4                                                                                                                                                                                                                   {"icd10_code_def":{"values":["1","15","3","5","7","8","2"],"defaults":["1","15","3","5","7","8"]}}
#> 5                                                                                                                                                                                                                                                                                                                 <NA>
#> 6                                                                                                                                                                                                                                                                     {"IS_ASSURED":{"values":["1"],"defaults":["1"]}}
#> 7                                                                                                                                                                                                                                                                                                                 <NA>
#> 8  {"mapping_status":{"values":["E","G","D","R","A","U"],"defaults":["E","G","D"]},"refine_flag":{"values":["C","P","M"],"defaults":["C","P"]},"element_num":{"values":["0","1","2","3"],"defaults":["0"]},"block_num":{"values":["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14"],"defaults":["0"]}}
#> 9                                                                                                                                                                                                                                                                                                                 <NA>
#> 10                                                                                                                                                                                                                                                                    {"IS_ASSURED":{"values":["1"],"defaults":["1"]}}
#> 
```
