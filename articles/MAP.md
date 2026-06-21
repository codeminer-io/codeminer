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

MAP("X40J4", from = "Read v3", to = "ICD-10")
#> ℹ Using "UKB v4" as the latest mapping version for "Read v3 > ICD-10".
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
#> <codeminer_codelist>: 1 code
#> 
#> Code type: "ICD-10"
#> # A tibble: 1 × 3
#>   code  description                                    code_type
#>   <chr> <chr>                                          <chr>    
#> 1 E109  Type 1 diabetes mellitus Without complications ICD-10
```

Use `MAP("all")` to return the entire mapping table:

``` r

MAP("all", from = "Read v3", to = "ICD-10")
#> # A tibble: 5 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 X40J4 E109  D              C           P             0           0        
#> 2 C10.. E149  D              C           C             0           0        
#> 3 XaIP9 L721  D              C           C             0           0        
#> 4 XE0e0 N390  D              C           P             0           0        
#> 5 XE0Uc I10   D              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```

## Filters

Mapping between different clinical coding systems is not perfect and
should be manually reviewed. Some mapping tables list multiple possible
code mappings, with a separate column indicating the type of
mapping.[^1] For example, the Read v3 to ICD10 mapping table includes
`mapping_status` and `refine_flag` columns:

``` r

MAP("all", from = "Read v3", to = "ICD-10") |>
  filter(from == "XaIP9")
#> # A tibble: 1 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 XaIP9 L721  D              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```

It is important to decide which mappings to include. For example, if no
filters are applied then the Read v3 code for sebaceous cyst ‘XaIP9’
will map to a number of ICD10 codes, some of which are sex-specific
(‘N508’ and ‘N948’):

``` r

MAP("XaIP9", from = "Read v3", to = "ICD-10")
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description       code_type
#>   <chr> <chr>             <chr>    
#> 1 L721  Trichilemmal cyst ICD-10
```

[^1]: Refer to the accompanying documentation for [UKB resource
    592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) for further
    details.
