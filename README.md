
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codeminer

<!-- badges: start -->

[![R-CMD-check](https://github.com/codeminer-io/codeminer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/codeminer-io/codeminer/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/codeminer-io/codeminer/graph/badge.svg?token=AO69EQDLKI)](https://codecov.io/gh/codeminer-io/codeminer)
[![pkgdown](https://github.com/codeminer-io/codeminer/workflows/pkgdown/badge.svg)](https://github.com/codeminer-io/codeminer/actions)
[![Launch RStudio
Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/4007004)
[![DOI](https://zenodo.org/badge/485945478.svg)](https://zenodo.org/badge/latestdoi/485945478)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of codeminer is to facilitate working with clinical codes used
in electronic health records.

This package relies primarily on UK Biobank resource 592 ([Clinical
coding classification systems and
maps](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) and the UK
Biobank [data codings
file](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide),
both of which are publicly available.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("codeminer-io/codeminer")
```

## Basic example

A data analyst using electronic health records for research into
hypertension may wish to build a list of clinical codes that capture
this condition.

First, build a local resource containing lookup and mapping tables for
various clinical codings systems. A dummy dataset is used here:

``` r
library(codeminer)

all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
```

Look up Read 2 codes for hypertension:

``` r
htn_read2 <- code_descriptions_like("Hypertension",
  code_type = "read2",
  all_lkps_maps = all_lkps_maps_dummy
)

htn_read2
#> # A tibble: 1 × 3
#>   code  description            code_type
#>   <chr> <chr>                  <chr>    
#> 1 G20.. Essential hypertension read2
```

Map these to ICD10:

``` r
htn_icd10 <- MAP(
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

See `vignette('codeminer')` for further details, including how to build
a clinical codelist with R Shiny using `RunCodelistBuilder()`.

## UK Biobank

Also included are functions for using CALIBER code lists
(`vignette('caliber')`) and Phecodes (`vignette('phecodes')`) with UK
Biobank data.

## 🏗️ Development

Contributions are welcome! Please follow the [Contributing
Guidelines](https://github.com/codeminer-io/codeminer/blob/main/CONTRIBUTING.md).
To suggest a change, please follow the instructions below.

Install a [recent version of R](https://www.r-project.org/) (`>= 4.5.0`)
and install [`pak`](https://pak.r-lib.org/) for package management:

``` r
install.packages("pak")
```

Though not required,
[RStudio](https://posit.co/download/rstudio-desktop/) is recommended as
an IDE, as it has good support for R package development and Shiny. We
also use [air](https://posit-dev.github.io/air/) for code formatting.
This can be set up as a standalone command line tool or be integrated
into your IDE.

1.  Clone this repository:
    `git clone https://github.com/codeminer-io/codeminer.git`

2.  Install development dependencies from a fresh R session in your
    local `codeminer` directory:

    ``` r
    pak::local_install_dev_deps()
    ```

3.  Create a new branch for your changes, following the naming
    convention `git checkout -b <username>/<my-feature-branch>`

4.  Modify the code as needed

5.  Format the code with `air`, either with your IDE or from the command
    line: `air format <path>`

6.  Run `R CMD check` locally and fix any errors and warnings, e.g. from
    an R session:

    ``` r
    devtools::check()
    ```

7.  Open a [Pull
    request](https://github.com/codeminer-io/codeminer/pulls) against
    the `main` branch

For more details about R package development and good practices, see the
[R Packages](https://r-pkgs.org/) book.
