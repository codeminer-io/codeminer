# Build named list of clinical code look up and mapping tables

Downloads the lookup and mapping tables from [UK Biobank resource
592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) as well as the
NHSBSA BNF-SNOMED mapping table (available
[here](https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping))
and OPCS4, self-reported medical conditions/medications/operations from
the UK Biobank codings file (available
[here](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)).

## Usage

``` r
build_all_lkps_maps(
  all_lkps_maps = read_all_lkps_maps(),
  ukb_codings = ukbwranglr::get_ukb_codings(),
  bnf_lkp = get_bnf_from_open_prescribing(),
  bnf_dmd = get_nhsbsa_snomed_bnf(),
  self_report_med_to_atc_map = get_ukb_self_report_med_to_atc_map(),
  phecode_1_2_lkp = get_phecode_definitions(),
  icd10_phecode_1_2 = get_phecode_icd10_map(),
  icd9_phecode_1_2 = get_phecode_icd9_map(),
  snomed_ct_uk_monolith = NULL,
  snomed_ct_nhs_data_migration = NULL,
  cprd_aurum_codebrowser_dir = NULL
)
```

## Arguments

- all_lkps_maps:

  UK Biobank resource 592, as returned by
  [`get_ukb_all_lkps_maps`](https://codeminer-io.github.io/codeminer/reference/get_ukb_all_lkps_maps.md).

- ukb_codings:

  The UK Biobank codings file, as returned by
  [`get_ukb_codings`](https://rmgpanw.github.io/ukbwranglr/reference/get_ukb_codings.html).

- bnf_lkp:

  Optional: default is to obtain a BNF lookup from the OpenPrescribing
  API. If `NULL`, the BNF look up table from UK Biobank resource 592
  will be used instead.

- bnf_dmd:

  Optional: path to the NHSBSA BNF-SNOMED mapping table (see
  [`get_nhsbsa_snomed_bnf()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_snomed_bnf.md)).

- self_report_med_to_atc_map:

  Optional: path to a UK Biobank self-reported medication to ATC map
  (see
  [`get_ukb_self_report_med_to_atc_map()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_self_report_med_to_atc_map.md)).

- phecode_1_2_lkp:

  Optional: path to the phecode v1.2 lookup file (see
  [`get_phecode_definitions()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_definitions.md)).

- icd10_phecode_1_2:

  Optional: path to the phecode v1.2 to ICD10 mapping file (see
  [`get_phecode_icd10_map()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_icd10_map.md)).

- icd9_phecode_1_2:

  Optional: path to the phecode v1.2 to ICD10 mapping file (see
  [`get_phecode_icd9_map()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_icd9_map.md)).

- snomed_ct_uk_monolith:

  Optional: path to the unzipped [SNOMED CT UK Monolith
  Edition](https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases).

- snomed_ct_nhs_data_migration:

  Optional: path to the unzipped [NHS Data
  Migration](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases).

- cprd_aurum_codebrowser_dir:

  Optional: path to the unzipped CPRD Aurum CodeBrowser folder. The
  folder name should contain the release date e.g.
  "CPRD_CodeBrowser_202412_Aurum".

## Value

Returns a named list of data frames.

## See also

[`all_lkps_maps_to_db()`](https://codeminer-io.github.io/codeminer/reference/all_lkps_maps_to_db.md)

## Examples

``` r
# build dummy all_lkps_maps using just UKB resource 592 and UKB codings file
build_all_lkps_maps(
  all_lkps_maps = read_all_lkps_maps_dummy(),
  ukb_codings = read_ukb_codings_dummy(),
  bnf_lkp = NULL,
  bnf_dmd = NULL,
  self_report_med_to_atc_map = NULL,
  snomed_ct_uk_monolith = NULL,
  phecode_1_2_lkp = NULL,
  icd10_phecode_1_2 = NULL,
  icd9_phecode_1_2 = NULL
)
#> Extending tables in UKB resource 592
#> Reformatting ICD9-Phecode map
#> Success!
#> $metadata_all_lkps_maps
#> # A tibble: 18 × 1
#>    metadata                                                      
#>    <chr>                                                         
#>  1 BNF lookup: version 76                                        
#>  2 dm+d lookup: May 2019 release                                 
#>  3 ICD-9 lookup: April 1995 release                              
#>  4 ICD-10 lookup: 5th Edition                                    
#>  5 ICD-9 to ICD-10 mapping: April 1995 release                   
#>  6 Read V2 lookup: April 2016 release                            
#>  7 Read V2 drugs lookup: April 2016 release                      
#>  8 Read V2 (April 2016 release) to BNF (version 68) mapping      
#>  9 Read V2 (April 2016 release) to ICD-9 mapping                 
#> 10 Read V2 (April 2016 release) to ICD-10 (5th Edition) mapping  
#> 11 Read V2 (April 2016 release) to OPCS-4.7 mapping              
#> 12 Read V2 (April 2016 release) to Read CTV3 (April 2018 release)
#> 13 Read CTV3 lookup (April 2018 release)                         
#> 14 Read CTV3 (April 2018 release) to ICD-9 mapping               
#> 15 Read CTV3 (April 2018 release) to ICD-10 (5th Edition) mapping
#> 16 Read CTV3 (April 2018 release) to OPCS-4.8 mapping            
#> 17 Read CTV3 (April 2018 release) to Read V2 (April 2016 release)
#> 18 Version 3 (May 2021)                                          
#> 
#> $bnf_lkp
#> # A tibble: 25 × 10
#>    BNF_Code        BNF_Code_Level     BNF_Chapter BNF_Section      BNF_Paragraph
#>    <chr>           <chr>              <chr>       <chr>            <chr>        
#>  1 1106000ABAAAAAA full               Eye         Treatment Of Gl… Treatment Of…
#>  2 11              chapter            Eye         NA               NA           
#>  3 1106            section            Eye         Treatment Of Gl… NA           
#>  4 110600          paragraph          Eye         Treatment Of Gl… Treatment Of…
#>  5 1106000         subparagraph       Eye         Treatment Of Gl… Treatment Of…
#>  6 1106000AB       chemical_substance Eye         Treatment Of Gl… Treatment Of…
#>  7 1106000ABAA     product_name       Eye         Treatment Of Gl… Treatment Of…
#>  8 1106000ABAAAA   further_info       Eye         Treatment Of Gl… Treatment Of…
#>  9 1106000ABAAABAB full               Eye         Treatment Of Gl… Treatment Of…
#> 10 1106000ABAAAB   further_info       Eye         Treatment Of Gl… Treatment Of…
#> # ℹ 15 more rows
#> # ℹ 5 more variables: BNF_Subparagraph <chr>, BNF_Chemical_Substance <chr>,
#> #   BNF_Product <chr>, BNF_Presentation <chr>, Description <chr>
#> 
#> $dmd_lkp
#> # A tibble: 33 × 3
#>    .rowid concept_id        term                                                
#>     <int> <chr>             <chr>                                               
#>  1      1 9248101000001107  Cosopt (product)                                    
#>  2      2 13283401000001102 Cosopt (qualifier value)                            
#>  3      3 3479501000001107  Cosopt (qualifier value)                            
#>  4      4 3479301000001100  Cosopt 2%/0.5% eye drops                            
#>  5      5 3479401000001108  Cosopt 2%/0.5% eye drops 5mL                        
#>  6      6 9007501000001101  COSOPT 2%/0.5% single-use eye drops 0.2mL (product) 
#>  7      7 19873411000001103 Cosopt eye drops (Doncaster Pharmaceuticals Ltd) (p…
#>  8      8 19873511000001104 Cosopt eye drops (Doncaster Pharmaceuticals Ltd) 5 …
#>  9      9 19869611000001107 Cosopt eye drops (Lexon (UK) Ltd) (product)         
#> 10     10 19869711000001103 Cosopt eye drops (Lexon (UK) Ltd) 5 ml (product)    
#> # ℹ 23 more rows
#> 
#> $icd9_lkp
#> # A tibble: 24 × 3
#>    .rowid ICD9  DESCRIPTION_ICD9                                 
#>     <int> <chr> <chr>                                            
#>  1      1 0020  TYPHOID FEVER                                    
#>  2      2 0341  SCARLATINA                                       
#>  3      3 250   DIABETES MELLITUS                                
#>  4      4 2500  DIABETES MELLITUS WITHOUT MENTION OF COMPLICATION
#>  5      5 2501  DIABETES WITH KETOACIDOSIS                       
#>  6      6 2502  DIABETES WITH COMA                               
#>  7      7 2503  DIABETES WITH RENAL MANIFESTATIONS               
#>  8      8 2504  DIABETES WITH OPHTHALMIC MANIFESTATIONS          
#>  9      9 2505  DIABETES WITH NEUROLOGICAL MANIFESTATIONS        
#> 10     10 2506  DIABETES WITH PERIPHERAL CIRCULATORY DISORDERS   
#> # ℹ 14 more rows
#> 
#> $icd10_lkp
#> # A tibble: 197 × 13
#>    .rowid ICD10_CODE ALT_CODE USAGE   USAGE_UK DESCRIPTION MODIFIER_4 MODIFIER_5
#>     <int> <chr>      <chr>    <chr>   <chr>    <chr>       <chr>      <chr>     
#>  1      1 A00        A00      DEFAULT 3        Cholera     NA         NA        
#>  2      2 A00.0      A000     DEFAULT 3        Cholera du… NA         NA        
#>  3      3 A00.1      A001     DEFAULT 3        Cholera du… NA         NA        
#>  4      4 A00.9      A009     DEFAULT 3        Cholera, u… NA         NA        
#>  5      5 A01.0      A010     DEFAULT 3        Typhoid fe… NA         NA        
#>  6      6 A02        A02      DEFAULT 3        Other salm… NA         NA        
#>  7      7 A02.0      A020     DEFAULT 3        Salmonella… NA         NA        
#>  8      8 A02.1      A021     DEFAULT 3        Salmonella… NA         NA        
#>  9      9 A02.2      A022     DEFAULT 3        Localized … NA         NA        
#> 10     10 A02.8      A028     DEFAULT 3        Other spec… NA         NA        
#> # ℹ 187 more rows
#> # ℹ 5 more variables: QUALIFIERS <chr>, GENDER_MASK <chr>, MIN_AGE <chr>,
#> #   MAX_AGE <chr>, TREE_DESCRIPTION <chr>
#> 
#> $icd9_icd10
#> # A tibble: 47 × 5
#>    .rowid ICD9  DESCRIPTION_ICD9        ICD10 DESCRIPTION_ICD10                 
#>     <int> <chr> <chr>                   <chr> <chr>                             
#>  1      1 0020  TYPHOID FEVER           A010  Typhoid fever                     
#>  2      2 0341  SCARLATINA              A38X  Scarlet fever                     
#>  3      3 218   UTERINE LEIOMYOMA       D250  Submucous leiomyoma of uterus     
#>  4      4 218   UTERINE LEIOMYOMA       D251  Intramural leiomyoma of uterus    
#>  5      5 218   UTERINE LEIOMYOMA       D252  Subserosal leiomyoma of uterus    
#>  6      6 218   UTERINE LEIOMYOMA       D259  Leiomyoma of uterus, unspecified  
#>  7      7 2890  SECONDARY POLYCYTHAEMIA D751  Secondary polycythaemia           
#>  8      8 UNDEF NA                      E100  Insulin-dependent diabetes mellit…
#>  9      9 UNDEF NA                      E101  Insulin-dependent diabetes mellit…
#> 10     10 UNDEF NA                      E102  Insulin-dependent diabetes mellit…
#> # ℹ 37 more rows
#> 
#> $read_v2_lkp
#> # A tibble: 23 × 4
#>    .rowid read_code term_code term_description             
#>     <int> <chr>     <chr>     <chr>                        
#>  1      1 65V7.     00        Notification of scarlet fever
#>  2      2 A341.     00        Scarlet fever - scarlatina   
#>  3      3 A341.     11        Scarlet fever                
#>  4      4 B78..     00        Uterine leiomyoma - fibroids 
#>  5      5 B78..     11        Fibroids                     
#>  6      6 B780.     00        Submucous uterine leiomyoma  
#>  7      7 B781.     00        Intramural uterine leiomyoma 
#>  8      8 B781.     11        Mural fibroids               
#>  9      9 B782.     00        Subserous uterine leiomyoma  
#> 10     10 B78z.     00        Uterine leiomyoma NOS        
#> # ℹ 13 more rows
#> 
#> $read_v2_drugs_lkp
#> # A tibble: 5 × 4
#>   .rowid read_code term_description                                 status_flag
#>    <int> <chr>     <chr>                                            <chr>      
#> 1      1 k8k..     DORZOLAMIDE+TIMOLOL                              0          
#> 2      2 k8k1.     DORZOLAMIDE+TIMOLOL 2%/0.5% eye drops            0          
#> 3      3 k8k2.     COSOPT 2%/0.5% eye drops 5mL                     0          
#> 4      4 k8k3.     COSOPT 2%/0.5% single-use eye drops 0.2mL        0          
#> 5      5 k8k4.     DORZOLAMIDE+TIMOLOL 2%/0.5% single-use eye drops 0          
#> 
#> $read_v2_drugs_bnf
#> # A tibble: 3 × 14
#>   .rowid.x read_code bnf_code    .rowid.y term_description           status_flag
#>      <int> <chr>     <chr>          <int> <chr>                      <chr>      
#> 1        1 k8k..     11.06.00.00        1 DORZOLAMIDE+TIMOLOL        0          
#> 2        2 k8k1.     11.06.00.00        2 DORZOLAMIDE+TIMOLOL 2%/0.… 0          
#> 3        3 k8k2.     11.06.00.00        3 COSOPT 2%/0.5% eye drops … 0          
#> # ℹ 8 more variables: bnf_chapter_code <chr>, bnf_section_code <chr>,
#> #   bnf_paragraph_code <chr>, bnf_subparagraph_code <chr>, BNF_Chapter <chr>,
#> #   BNF_Section <chr>, BNF_Paragraph <chr>, BNF_Subparagraph <chr>
#> 
#> $read_v2_icd9
#> # A tibble: 118 × 4
#>    .rowid read_code icd9_code icd9_code_def
#>     <int> <chr>     <chr>     <chr>        
#>  1      1 A341.     0341      1            
#>  2      2 B78..     218       1            
#>  3      3 B780.     218       1            
#>  4      4 B781.     218       1            
#>  5      5 B782.     218       1            
#>  6      6 B78z.     218       1            
#>  7      7 B79..     219       1            
#>  8      8 C1...     250-259   2            
#>  9      9 C10..     250       1            
#> 10     10 C100.     2500      1            
#> # ℹ 108 more rows
#> 
#> $read_v2_icd10
#> # A tibble: 104 × 4
#>    .rowid read_code icd10_code  icd10_code_def
#>     <int> <chr>     <chr>       <chr>         
#>  1      1 A153.     A180D M900A 7             
#>  2      2 A15x.     A180D M900A 7             
#>  3      3 A34..     J020,A38X   3             
#>  4      4 A341.     A38X        1             
#>  5      5 B78..     D259        1             
#>  6      6 B780.     D250        1             
#>  7      7 B781.     D251        1             
#>  8      8 B782.     D252        1             
#>  9      9 B78z.     D259        1             
#> 10     10 C10..     E10-E14     2             
#> # ℹ 94 more rows
#> 
#> $read_v2_opcs4
#> # A tibble: 4 × 4
#>   .rowid read_code opcs_4.2_code opcs_code_def
#>    <int> <chr>     <chr>         <chr>        
#> 1      1 77010     H021          1            
#> 2      2 77011     H023          1            
#> 3      3 77012     H024          1            
#> 4      4 77013     H022          1            
#> 
#> $read_v2_read_ctv3
#> # A tibble: 26 × 12
#>    .rowid CHAPTER READV2_CODE READV2_DESC   TERMV2_DESC TERMV2_ORDER TERMV2_TYPE
#>     <int> <chr>   <chr>       <chr>         <chr>       <chr>        <chr>      
#>  1      1 C       C106.       Diabetes mel… Diabetes m… 00           P          
#>  2      2 C       C106.       Diabetes mel… Diabetes m… 12           S          
#>  3      3 C       C106.       Diabetes mel… Diabetes m… 13           S          
#>  4      4 C       C106.       Diabetes mel… Diabetic a… 11           S          
#>  5      5 C       C106.       Diabetes mel… Diabetic a… 11           S          
#>  6      6 C       C108.       Insulin depe… Insulin de… 00           P          
#>  7      7 C       C108.       Insulin depe… Insulin de… 00           P          
#>  8      8 C       C108.       Insulin depe… IDDM-Insul… 11           S          
#>  9      9 C       C108.       Insulin depe… IDDM-Insul… 11           S          
#> 10     10 C       C108.       Insulin depe… Type 1 dia… 12           S          
#> # ℹ 16 more rows
#> # ℹ 5 more variables: READV3_CODE <chr>, TERMV3_CODE <chr>, TERMV3_TYPE <chr>,
#> #   TERMV3_DESC <chr>, IS_ASSURED <chr>
#> 
#> $read_ctv3_lkp
#> # A tibble: 31 × 5
#>    .rowid read_code term_description                     description_type status
#>     <int> <chr>     <chr>                                <chr>            <chr> 
#>  1      1 C10..     Diabetes mellitus                    P                C     
#>  2      2 C10..     DM - Diabetes mellitus               S                C     
#>  3      3 X40J4     Type I diabetes mellitus             P                C     
#>  4      4 X40J4     Type 1 diabetes mellitus             S                C     
#>  5      5 X40J4     IDDM - Insulin-dependent diabetes m… S                C     
#>  6      6 X40J4     Juvenile onset diabetes mellitus     S                C     
#>  7      7 X40J4     Insulin-dependent diabetes mellitus  S                C     
#>  8      8 X30J0     End stage renal failure              P                C     
#>  9      9 X30J0     ESRF - End stage renal failure       S                C     
#> 10     10 X30J0     End stage renal disease              S                C     
#> # ℹ 21 more rows
#> 
#> $read_ctv3_icd9
#> # A tibble: 4 × 8
#>   .rowid read_code icd9_code mapping_status refine_flag add_code_flag
#>    <int> <chr>     <chr>     <chr>          <chr>       <chr>        
#> 1      1 X40J4     2500      D              P           C            
#> 2      2 X40J4     250       A              M           P            
#> 3      3 X40J4     6480      R              C           C            
#> 4      4 X40J4     7902      R              C           C            
#> # ℹ 2 more variables: element_num <chr>, block_num <chr>
#> 
#> $read_ctv3_icd10
#> # A tibble: 36 × 8
#>    .rowid read_code icd10_code mapping_status refine_flag add_code_flag
#>     <int> <chr>     <chr>      <chr>          <chr>       <chr>        
#>  1      1 X40J4     E109       D              C           P            
#>  2      2 X40J4     E10        A              M           P            
#>  3      3 X40J4     O240       R              C           C            
#>  4      4 C10..     E149       D              C           C            
#>  5      5 C10..     E14        A              M           P            
#>  6      6 C10..     E109       R              C           C            
#>  7      7 C10..     E119       R              C           C            
#>  8      8 C10..     E129       R              C           C            
#>  9      9 C10..     E139       R              C           C            
#> 10     10 C10..     O249       R              C           C            
#> # ℹ 26 more rows
#> # ℹ 2 more variables: element_num <chr>, block_num <chr>
#> 
#> $read_ctv3_opcs4
#> # A tibble: 5 × 8
#>   .rowid read_code opcs4_code mapping_status refine_flag add_code_flag
#>    <int> <chr>     <chr>      <chr>          <chr>       <chr>        
#> 1      1 77010     H021       G              C           P            
#> 2      2 77011     H023       G              C           P            
#> 3      3 77012     H024       G              C           P            
#> 4      4 77013     H022       G              C           P            
#> 5      5 XaBAT     H021       D              C           P            
#> # ℹ 2 more variables: element_num <chr>, block_num <chr>
#> 
#> $read_ctv3_read_v2
#> # A tibble: 10 × 11
#>    .rowid READV3_CODE TERMV3_CODE TERMV3_TYPE TERMV3_DESC            READV2_CODE
#>     <int> <chr>       <chr>       <chr>       <chr>                  <chr>      
#>  1      1 X40J4       Y41PV       S           Type 1 diabetes melli… C10E.      
#>  2      2 X40J4       Y41PW       S           IDDM - Insulin-depend… C108.      
#>  3      3 X40J4       Y41PW       S           IDDM - Insulin-depend… C10E.      
#>  4      4 X40J4       Y41PW       S           IDDM - Insulin-depend… C10E.      
#>  5      5 X40J4       Y41PX       S           Juvenile onset diabet… C108.      
#>  6      6 X40J4       Y41PX       S           Juvenile onset diabet… C10E.      
#>  7      7 X40J4       Y41PX       S           Juvenile onset diabet… C10E.      
#>  8      8 X40J4       Yadjz       S           Insulin-dependent dia… C10E.      
#>  9      9 X40J4       Yadjz       S           Insulin-dependent dia… C10E.      
#> 10     10 X40J4       Yagv5       P           Type I diabetes melli… C10E.      
#> # ℹ 5 more variables: READV2_DESC <chr>, TERMV2_ORDER <chr>, TERMV2_TYPE <chr>,
#> #   TERMV2_DESC <chr>, IS_ASSURED <chr>
#> 
#> $opcs4_lkp
#> # A tibble: 6 × 2
#>   opcs4_code description                                                   
#>   <chr>      <chr>                                                         
#> 1 H01        H01 Emergency excision of appendix                            
#> 2 H011       H01.1 Emergency excision of abnormal appendix and drainage HFQ
#> 3 H012       H01.2 Emergency excision of abnormal appendix NEC             
#> 4 H013       H01.3 Emergency excision of normal appendix                   
#> 5 H018       H01.8 Other specified emergency excision of appendix          
#> 6 H019       H01.9 Unspecified emergency excision of appendix              
#> 
#> $self_report_cancer
#> # A tibble: 0 × 2
#> # ℹ 2 variables: data_coding_3 <chr>, description <chr>
#> 
#> $self_report_medication
#> # A tibble: 0 × 2
#> # ℹ 2 variables: data_coding_4 <chr>, description <chr>
#> 
#> $self_report_operation
#> # A tibble: 0 × 2
#> # ℹ 2 variables: data_coding_5 <chr>, description <chr>
#> 
#> $self_report_non_cancer
#> # A tibble: 0 × 2
#> # ℹ 2 variables: data_coding_6 <chr>, description <chr>
#> 
#> $metadata_codeminer
#> # A tibble: 1 × 1
#>   metadata                  
#>   <chr>                     
#> 1 codeminer|0.0.0.9004|4.5.2
#> 
```
