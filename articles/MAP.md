# Mapping between clinical coding systems

``` r
library(codeminer)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

create_dummy_database()
#> ✔ Dummy database ready to use!
```

## Basic usage

Use [`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md)
to map a vector of codes from one clinical coding system to another:

``` r
MAP(codes = "X40J4", from = "Read 3", to = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> Warning: ! The following codes were not found in the lookup table:
#> • `O240`
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> # A tibble: 2 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E10   Type 1 diabetes mellitus                       ICD-10   
#> 2 E109  Type 1 diabetes mellitus Without complications ICD-10
```

Use `MAP("all")` to return the entire mapping table:

``` r
MAP("all", from = "Read 3", to = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 36 × 8
#>    from  to    mapping_status refine_flag add_code_flag element_num block_num
#>    <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#>  1 X40J4 E109  D              C           P             0           0        
#>  2 X40J4 E10   A              M           P             0           0        
#>  3 X40J4 O240  R              C           C             0           0        
#>  4 C10.. E149  D              C           C             0           0        
#>  5 C10.. E14   A              M           P             0           0        
#>  6 C10.. E109  R              C           C             0           0        
#>  7 C10.. E119  R              C           C             0           0        
#>  8 C10.. E129  R              C           C             0           0        
#>  9 C10.. E139  R              C           C             0           0        
#> 10 C10.. O249  R              C           C             0           0        
#> # ℹ 26 more rows
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```

## Filters

Mapping between different clinical coding systems is not perfect and
should be manually reviewed. Some mapping tables list multiple possible
code mappings, with a separate column indicating the type of
mapping.[¹](#fn1) For example, the Read 3 to ICD10 mapping table
includes `mapping_status` and `refine_flag` columns:

``` r
MAP("all", from = "Read 3", to = "ICD-10") |>
  filter(from == "XaIP9")
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 5 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 XaIP9 L721  D              C           C             0           0        
#> 2 XaIP9 H028  R              C           C             0           0        
#> 3 XaIP9 N508  R              C           C             0           0        
#> 4 XaIP9 N608  R              C           C             0           0        
#> 5 XaIP9 N948  R              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```

It is important to decide which mappings to include. For example, if no
filters are applied then the Read 3 code for sebaceous cyst ‘XaIP9’ will
map to a number of ICD10 codes, some of which are sex-specific (‘N508’
and ‘N948’):

``` r
MAP("XaIP9", from = "Read 3", to = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 5 codes
#> 
#> Code type: "ICD-10"
#> # A tibble: 5 × 3
#>   code  description                                                    code_type
#>   <chr> <chr>                                                          <chr>    
#> 1 H028  Other specified disorders of eyelid                            ICD-10   
#> 2 L721  Trichilemmal cyst                                              ICD-10   
#> 3 N508  Other specified disorders of male genital organs               ICD-10   
#> 4 N608  Other benign mammary dysplasias                                ICD-10   
#> 5 N948  Other specified conditions associated with female genital org… ICD-10
```

------------------------------------------------------------------------

1.  Refer to the accompanying documentation for [UKB resource
    592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) for further
    details.
