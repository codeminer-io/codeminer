# codeminer

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
create_dummy_database()
#> Creating new database at
#> '/var/folders/zt/jltqykf54y3588fczjzp63ym0000gn/T//RtmpscvWXm/file105fb1aa82952.duckdb'
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
```

Look up Read 3 codes for hypertension:

``` r

htn_read3 <- DESCRIPTION("Hypertension", code_type = "Read v3")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
htn_read3
#> # A tibble: 1 × 5
#>   code  description            preferred_description status code_type
#>   <chr> <chr>                  <lgl>                 <chr>  <chr>    
#> 1 XE0Uc Essential hypertension TRUE                  C      Read 3
```

Map these to ICD10:

``` r

htn_icd10 <- MAP(
  codes = htn_read3$code,
  from = "Read v3",
  to = "ICD-10"
)
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version

htn_icd10
#> # A tibble: 7 × 14
#>   code  description   ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>   <chr> <chr>         <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#> 1 I10   Essential (p… I10        DEFA… 3        <NA>       <NA>       <NA>      
#> 2 I11   Hypertensive… I11        DEFA… 3        <NA>       <NA>       <NA>      
#> 3 I12   Hypertensive… I12        DEFA… 3        <NA>       <NA>       <NA>      
#> 4 I13   Hypertensive… I13        DEFA… 3        <NA>       <NA>       <NA>      
#> 5 O10   Pre-existing… O10        DEFA… 3        <NA>       <NA>       <NA>      
#> 6 O11   Pre-eclampsi… O11        DEFA… 3        <NA>       <NA>       <NA>      
#> 7 P000  Fetus and ne… P00.0      DEFA… 3        <NA>       <NA>       <NA>      
#> # ℹ 6 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>
```

See the main [codeminer
vignette](https://codeminer-io.github.io/codeminer/articles/codeminer.html)
for further details.

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

7.  Install and set up [lintr](https://lintr.r-lib.org/) [for your
    IDE](https://lintr.r-lib.org/articles/editors.html) and fix any
    issues

8.  Open a [Pull
    request](https://github.com/codeminer-io/codeminer/pulls) against
    the `main` branch

For more details about R package development and good practices, see the
[R Packages](https://r-pkgs.org/) book.
