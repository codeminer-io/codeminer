# Metadata

Returns a named list of tibbles which show the metadata used for (i)
lookup tables and (ii) mapping tables.

## Usage

``` r
codeminer_metadata()
```

## Value

A named list.

## Examples

``` r
codeminer_metadata()
#> $lookup_tables
#> # A tibble: 16 × 9
#>    code      code_label lkp_table code_col description_col preferred_synonym_col
#>    <chr>     <chr>      <chr>     <chr>    <chr>           <chr>                
#>  1 icd10     ICD-10     icd10_lkp ALT_CODE DESCRIPTION     NA                   
#>  2 icd9      ICD-9      icd9_lkp  ICD9     DESCRIPTION_IC… NA                   
#>  3 read3     Read 3     read_ctv… read_co… term_descripti… description_type     
#>  4 read2     Read 2     read_v2_… read_co… term_descripti… term_code            
#>  5 sct       SNOMED CT  sct_desc… concept… term_descripti… typeId_description   
#>  6 opcs4     OPCS4      opcs4_lkp opcs4_c… description     NA                   
#>  7 phecode   Phecode    phecode_… phecode  phenotype       NA                   
#>  8 read2_dr… Read 2, d… read_v2_… read_co… term_descripti… NA                   
#>  9 bnf       BNF        bnf_lkp   BNF_Code Description     NA                   
#> 10 dmd       DMD        bnf_dmd   snomed_… dm_d_product_d… NA                   
#> 11 data_cod… Self-repo… self_rep… data_co… description     NA                   
#> 12 data_cod… Self-repo… self_rep… data_co… description     NA                   
#> 13 data_cod… Self-repo… self_rep… data_co… description     NA                   
#> 14 data_cod… Self-repo… self_rep… data_co… description     NA                   
#> 15 medcode_… MedCode (… medcode_… MedCode… Term            NA                   
#> 16 prodcode… ProdCode … prodcode… ProdCod… Term_from_EMIS  NA                   
#> # ℹ 3 more variables: preferred_code <chr>, grouping_col <chr>,
#> #   filter_cols <list>
#> 
#> $mapping_tables
#> # A tibble: 19 × 8
#>    from           to      mapping_table    from_col to_col preferred_synonym_col
#>    <chr>          <chr>   <chr>            <chr>    <chr>  <chr>                
#>  1 icd9           icd10   icd9_icd10       ICD9     ICD10  NA                   
#>  2 read2_drugs    bnf     read_v2_drugs_b… read_co… bnf_c… NA                   
#>  3 read2          icd9    read_v2_icd9     read_co… icd9_… NA                   
#>  4 read2          icd10   read_v2_icd10    read_co… icd10… NA                   
#>  5 read2          opcs4   read_v2_opcs4    read_co… opcs_… NA                   
#>  6 read2          read3   read_v2_read_ct… READV2_… READV… TERMV3_TYPE          
#>  7 read3          icd9    read_ctv3_icd9   read_co… icd9_… NA                   
#>  8 read3          icd10   read_ctv3_icd10  read_co… icd10… NA                   
#>  9 read3          opcs4   read_ctv3_opcs4  read_co… opcs4… NA                   
#> 10 read3          read2   read_ctv3_read_… READV3_… READV… TERMV2_TYPE          
#> 11 bnf            sct     bnf_dmd          bnf_code snome… NA                   
#> 12 icd10          phecode icd10_phecode    ALT_CODE PHECO… NA                   
#> 13 icd9           phecode icd9_phecode     icd9     pheco… NA                   
#> 14 sct            icd10   sct_icd10        referen… mapTa… NA                   
#> 15 sct            opcs4   sct_opcs4        referen… mapTa… NA                   
#> 16 read2          sct     rcsctmap2        ReadCode Conce… NA                   
#> 17 read3          sct     ctv3sctmap2      CTV3_CO… SCT_C… NA                   
#> 18 medcode_aurum  sct     medcode_aurum    MedCode… Snome… NA                   
#> 19 prodcode_aurum sct     prodcode_aurum   ProdCod… dmdid  NA                   
#> # ℹ 2 more variables: preferred_code <chr>, filter_cols <list>
#> 
```
