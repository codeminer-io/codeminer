
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codemapper

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmgpanw/codemapper/workflows/R-CMD-check/badge.svg)](https://github.com/rmgpanw/codemapper/actions)
[![Codecov test
coverage](https://codecov.io/gh/rmgpanw/codemapper/branch/master/graph/badge.svg)](https://codecov.io/gh/rmgpanw/codemapper?branch=master)
[![pkgdown](https://github.com/rmgpanw/codemapper/workflows/pkgdown/badge.svg)](https://github.com/rmgpanw/codemapper/actions)

<!-- badges: end -->

The goal of codemapper is to facilitate working with clinical codes used
in electronic health records. This package relies primarily on UK
Biobank resource 592 ([Clinical coding classification systems and
maps](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) and the UK
Biobank [data codings
file](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide),
both of which are publicly available.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/codemapper")
```

## Basic example

A data analyst using electronic health records for research into
hypertension may wish to build a list of clinical codes that capture
this condition.

First, build a local resource containing lookup and mapping tables for
various clinical codings systems. A dummy dataset is used here:

``` r
library(codemapper)

all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#> Extending tables in UKB resource 592
#> Success!
```

Look up Read 2 codes for hypertension:

``` r
htn_read2 <- code_descriptions_like("Hypertension",
                       code_type = "read2",
                       all_lkps_maps = all_lkps_maps_dummy)

htn_read2
#> # A tibble: 1 × 3
#>   code  description            code_type
#>   <chr> <chr>                  <chr>    
#> 1 G20.. Essential hypertension read2
```

Map these to ICD10:

``` r
htn_icd10 <- map_codes(
  codes = htn_read2$code,
  from = "read2",
  to = "icd10",
  all_lkps_maps = all_lkps_maps_dummy
)

htn_icd10
#> # A tibble: 1 × 3
#>   code  description                      code_type
#>   <chr> <chr>                            <chr>    
#> 1 I10X  Essential (primary) hypertension icd10
```
