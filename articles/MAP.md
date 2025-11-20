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
#> ℹ Creating new database at /tmp/Rtmpkk30XB/file1e2521072b90.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
```

## Basic usage

Use [`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md)
to map a vector of codes from one clinical coding system to another:

``` r
MAP(codes = "X40J4", from = "read3", to = "icd10", version = "v0")
#> # A tibble: 2 × 12
#>   code  description     ICD10_CODE USAGE USAGE_UK QUALIFIERS GENDER_MASK MIN_AGE
#>   <chr> <chr>           <chr>      <chr>    <dbl> <chr>            <dbl>   <dbl>
#> 1 E10   Type 1 diabete… E10        DEFA…        3 NA                  NA      NA
#> 2 E109  Type 1 diabete… E10.9      DEFA…        3 NA                  NA      NA
#> # ℹ 4 more variables: MAX_AGE <dbl>, TREE_DESCRIPTION <lgl>, code_type <chr>,
#> #   preferred_description <lgl>
```

Use `MAP("all")` to return the entire mapping table:

``` r
MAP("all", from = "read3", to = "icd10")
#> ℹ Using 'v0' as latest version
#> # A tibble: 24 × 7
#>    from  to    mapping_status refine_flag add_code_flag element_num block_num
#>    <chr> <chr> <chr>          <chr>       <chr>               <dbl>     <dbl>
#>  1 X40J4 E109  D              C           P                       0         0
#>  2 X40J4 E10   A              M           P                       0         0
#>  3 C10.. E149  D              C           C                       0         0
#>  4 C10.. E14   A              M           P                       0         0
#>  5 C10.. E109  R              C           C                       0         0
#>  6 C10.. E119  R              C           C                       0         0
#>  7 C10.. E129  R              C           C                       0         0
#>  8 C10.. E139  R              C           C                       0         0
#>  9 C10.. O249  R              C           C                       0         0
#> 10 C10.. P700  R              C           C                       0         0
#> # ℹ 14 more rows
```

## Filters

Mapping between different clinical coding systems is not perfect and
should be manually reviewed. Some mapping tables list multiple possible
code mappings, with a separate column indicating the type of
mapping.[¹](#fn1) For example, the Read 3 to ICD10 mapping table
includes `mapping_status` and `refine_flag` columns:

``` r
MAP("all", from = "read3", to = "icd10") |> 
  filter(from == "XaIP9")
#> ℹ Using 'v0' as latest version
#> # A tibble: 5 × 7
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>               <dbl>     <dbl>
#> 1 XaIP9 L721  D              C           C                       0         0
#> 2 XaIP9 H028  R              C           C                       0         0
#> 3 XaIP9 N508  R              C           C                       0         0
#> 4 XaIP9 N608  R              C           C                       0         0
#> 5 XaIP9 N948  R              C           C                       0         0
```

It is important to decide which mappings to include. For example, if no
filters are applied then the Read 3 code for sebaceous cyst ‘XaIP9’ will
map to a number of ICD10 codes, some of which are sex-specific (‘N508’
and ‘N948’):

``` r
MAP("XaIP9", from = "read3", to = "icd10") |> 
  filter(!is.na(GENDER_MASK))
#> ℹ Using 'v0' as latest version
#> ℹ Using 'v0' as latest version
#> # A tibble: 2 × 12
#>   code  description     ICD10_CODE USAGE USAGE_UK QUALIFIERS GENDER_MASK MIN_AGE
#>   <chr> <chr>           <chr>      <chr>    <dbl> <chr>            <dbl>   <dbl>
#> 1 N508  Other specifie… N50.8      DEFA…        3 NA                   2      NA
#> 2 N948  Other specifie… N94.8      DEFA…        3 NA                   1      NA
#> # ℹ 4 more variables: MAX_AGE <dbl>, TREE_DESCRIPTION <lgl>, code_type <chr>,
#> #   preferred_description <lgl>
```

------------------------------------------------------------------------

1.  Refer to the accompanying documentation for [UKB resource
    592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) for further
    details.
