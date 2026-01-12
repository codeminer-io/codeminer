# Introduction to codeminer

``` r
library(codeminer)
```

## Introduction

The goal of codeminer is to simplify working with clinical codes for
research using electronic health records. The workflow is as follows:

1.  Create a local resource containing lookup and mapping tables for
    various clinical codings systems (e.g. ICD10 and Read codes)
2.  Build clinical code lists for conditions of interest by querying
    this resource

This vignette demonstrates the above using dummy data included with the
package. You can try out the steps either locally by installing
codeminer on your own machine, or online by clicking on the following
link to RStudio Cloud[¹](#fn1) and navigating to this Rmd file in the
‘vignettes’ directory: [![Launch RStudio
Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/4007004)

Also included are functions for mapping between different clinical
coding systems, and using Phecodes(Denny, Bastarache, and Roden 2016; Wu
et al. 2019) with UK Biobank data. See vignettes
[`vignette('MAP')`](https://codeminer-io.github.io/codeminer/articles/MAP.md)
`vignette('caliber')` and `vignette('phecodes')` for further
information.

## Build a local clinical codes lookup and mappings resource

The first step is to create a local database containing lookup and
mapping tables for various clinical coding systems using
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md).

By default this will download the following resources:

- UK Biobank resource 592 ([Clinical coding classification systems and
  maps](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592))

- UK Biobank [data codings
  file](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)

- [Phecode lookup and mapping files](https://phewascatalog.org/) (for
  ICD9 and ICD10 to phecode)

The tables are imported into R, reformatted, and stored as a named list
of data frames:

``` r
# Create a temporary database with dummy data
(db_path <- create_dummy_database())
#> Creating new database at /tmp/RtmpgnDlIa/file24b677398309.duckdb
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
#> [1] "/tmp/RtmpgnDlIa/file24b677398309.duckdb"
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmpgnDlIa/file24b677398309.duckdb"
```

Setting the `CODEMINER_DB_PATH` environment variable ensures that all
subsequent `codeminer` calls will use this database.

To persist the database across sessions, set the `CODEMINER_DB_PATH`
environment variable to a path on your system, e.g. using
[`usethis::edit_r_environ(scope = "project")`](https://usethis.r-lib.org/reference/edit.html?q=edit_r_environ#ref-usage):

    # ./.Renviron
    CODEMINER_DB_PATH=/path/to/codeminer-database.duckdb

Alternatively, if the environment variable is not set, `codeminer` will
store the database in a default location, determined by
[`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

The database is a [duckdb](https://r.duckdb.org/index.html) database and
can be inspected using the [DBI](https://dbi.r-dbi.org/) package.

``` r
# connect to Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
DBI::dbListTables(con)
#>  [1] "BNF_UKB v4"                 "BNF_relationship_UKB v4"   
#>  [3] "DM+D_UKB v4"                "ICD-10_UKB v4"             
#>  [5] "ICD-10_relationship_UKB v4" "ICD-9_ICD-10_UKB v4"       
#>  [7] "ICD-9_UKB v4"               "ICD-9_relationship_UKB v4" 
#>  [9] "Read 2, drugs_BNF_UKB v4"   "Read 2, drugs_UKB v4"      
#> [11] "Read 2_ICD-10_UKB v4"       "Read 2_ICD-9_UKB v4"       
#> [13] "Read 2_OPCS4_UKB v4"        "Read 2_Read 3_UKB v4"      
#> [15] "Read 2_UKB v4"              "Read 2_relationship_UKB v4"
#> [17] "Read 3_ICD-10_UKB v4"       "Read 3_ICD-9_UKB v4"       
#> [19] "Read 3_OPCS4_UKB v4"        "Read 3_Read 2_UKB v4"      
#> [21] "Read 3_UKB v4"              "_lookup_metadata"          
#> [23] "_mapping_metadata"          "_relationship_metadata"

# Close the connection when you're done
DBI::dbDisconnect(con)
```

Note that manual interaction with the database should not be necessary,
`codeminer` will take care of this for you.

## Build a clinical code list

### Explore codes

Codes may be explored with:

- [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md):
  look up descriptions for a set of code in the given code system type

``` r
CODES(
  codes = c("E10", "E11"),
  code_type = "ICD-10"
)
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 2 × 14
#>   code  description   ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>   <chr> <chr>         <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#> 1 E10   Type 1 diabe… E10        DEFA… 3        NA         NA         NA        
#> 2 E11   Type 2 diabe… E11        DEFA… 3        NA         NA         NA        
#> # ℹ 6 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>
```

- `DESCRIPTION():` search for codes that match a description

``` r
DESCRIPTION(pattern = "cyst", code_type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 2 × 14
#>   code  description   ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>   <chr> <chr>         <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#> 1 L721  Trichilemmal… L72.1      DEFA… 3        NA         NA         NA        
#> 2 N330  Tuberculous … N33.0      ASTE… 2        NA         NA         NA        
#> # ℹ 6 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>
```

Denny, Joshua C., Lisa Bastarache, and Dan M. Roden. 2016. “Phenome-Wide
Association Studies as a Tool to Advance Precision Medicine.” *Annual
Review of Genomics and Human Genetics* 17 (August): 353–73.
<https://doi.org/10.1146/annurev-genom-090314-024956>.

Wu, Patrick, Aliya Gifford, Xiangrui Meng, Xue Li, Harry Campbell, Tim
Varley, Juan Zhao, et al. 2019. “Mapping ICD-10 and ICD-10-CM Codes to
Phecodes: Workflow Development and Initial Evaluation.” *JMIR medical
informatics* 7 (4): e14325. <https://doi.org/10.2196/14325>.

------------------------------------------------------------------------

1.  You will be asked to sign up for a free account if you do not have
    one already.
