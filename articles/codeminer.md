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
package.

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
#> Creating new database at /tmp/RtmpKqskMH/file260d10f7e97b.duckdb
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
#> [1] "/tmp/RtmpKqskMH/file260d10f7e97b.duckdb"
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmpKqskMH/file260d10f7e97b.duckdb"
```

`codeminer` resolves the database location using the following
precedence:

1.  The `CODEMINER_DB_PATH` environment variable, if set
2.  A default location determined by
    [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html)

To persist the database location across sessions, set
`CODEMINER_DB_PATH` in your `.Renviron`, e.g. using
[`usethis::edit_r_environ(scope = "project")`](https://usethis.r-lib.org/reference/edit.html?q=edit_r_environ#ref-usage):

    # ./.Renviron
    CODEMINER_DB_PATH=/path/to/codeminer-database.duckdb

Alternatively, you can point `codeminer` at a specific database file
with
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md):

``` r
codeminer_connect(main = "/path/to/codeminer-database.duckdb")
```

The database is a [duckdb](https://r.duckdb.org/index.html) database.
`codeminer` manages the database connection automatically — you don’t
need to connect or disconnect manually. You can check the current
connection status with
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md):

``` r
codeminer_status()
#> No active workbench connection.
```

## Build a clinical code list

### Explore codes

Codes may be explored with:

- [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md):
  look up descriptions for a set of code in the given code system type

``` r
CODES(
  codes = c("E10", "E11"),
  type = "ICD-10"
)
#> ℹ Using database at /tmp/RtmpKqskMH/file260d10f7e97b.duckdb
#> ℹ Set `CODEMINER_DB_PATH` or use `codeminer_connect()` to change this.
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> 
#> Code type: "ICD-10"
#> # A tibble: 2 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   
#> 2 E11   Type 2 diabetes mellitus ICD-10
```

- `DESCRIPTION():` search for codes that match a description

``` r
DESCRIPTION(pattern = "cyst", type = "ICD-10")
#> ℹ Using 'UKB v4' as latest version
#> ℹ Using 'UKB v4' as latest version
#> <codeminer_codelist>: 2 codes
#> 
#> Code type: "ICD-10"
#> # A tibble: 2 × 3
#>   code  description          code_type
#>   <chr> <chr>                <chr>    
#> 1 L721  Trichilemmal cyst    ICD-10   
#> 2 N330  Tuberculous cystitis ICD-10
```

## Managing tables

### Adding tables

You can add custom lookup, mapping, and relationship tables to the
database with
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md).
Each requires a data frame and a metadata object created with the
corresponding `*_metadata()` constructor:

``` r
custom_lookup <- data.frame(
  code = c("CUSTOM1", "CUSTOM2"),
  description = c("Custom code 1", "Custom code 2")
)

add_lookup_table(
  custom_lookup,
  lookup_metadata("custom_codes", lookup_version = "v1")
)
#> ✔ Lookup table custom_codes_v1 added successfully.

CODES("all", type = "custom_codes")
#> ℹ Using 'v1' as latest version
#> <codeminer_codelist>: 2 codes
#> 
#> Code type: "custom_codes"
#> # A tibble: 2 × 3
#>   code    description   code_type   
#>   <chr>   <chr>         <chr>       
#> 1 CUSTOM1 Custom code 1 custom_codes
#> 2 CUSTOM2 Custom code 2 custom_codes
```

### Removing tables

To remove a table, use the corresponding `remove_*_table()` function
with the same identifying keys:

``` r
remove_lookup_table("custom_codes", "v1")
#> ✔ Lookup table custom_codes_v1 removed.
```

Removing a table deletes both the data table and its metadata entry.
After removal, the same code type and version can be re-added.

### Viewing metadata

Use
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
to inspect the tables currently in the database:

``` r
get_codeminer_metadata("lookup")
#>      lookup_table_name     code_type lookup_version lookup_code_col
#> 1           BNF_UKB v4           BNF         UKB v4        BNF_Code
#> 2          DM+D_UKB v4          DM+D         UKB v4      concept_id
#> 3         ICD-9_UKB v4         ICD-9         UKB v4            ICD9
#> 4        ICD-10_UKB v4        ICD-10         UKB v4        ALT_CODE
#> 5        Read 2_UKB v4        Read 2         UKB v4       read_code
#> 6 Read 2, drugs_UKB v4 Read 2, drugs         UKB v4       read_code
#> 7        Read 3_UKB v4        Read 3         UKB v4       read_code
#>   lookup_description_col                                      lookup_source
#> 1            Description https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 2                   term https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 3       DESCRIPTION_ICD9 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 4            DESCRIPTION https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 5       term_description https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 6       term_description https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#> 7       term_description https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#>   preferred_description_col preferred_description_indicator
#> 1                      <NA>                            <NA>
#> 2                      <NA>                            <NA>
#> 3                      <NA>                            <NA>
#> 4                      <NA>                            <NA>
#> 5                 term_code                              00
#> 6                      <NA>                            <NA>
#> 7          description_type                               P
```

## Version pinning

When multiple versions of a lookup, mapping, or relationship table are
available, `codeminer` resolves `"latest"` automatically. You can
override this for the current session with
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md):

``` r
# Pin lookup and relationship versions for a code type
codeminer_set_version(
  lookup       = c("ICD-10" = "UKB v4"),
  relationship = c("ICD-10" = "UKB v4")
)

# Pin a mapping version (use "from > to" format for the key)
codeminer_set_version(
  mapping = c("Read 3 > ICD-10" = "UKB v4")
)
```

Pins only affect the default `"latest"` resolution. Explicit version
arguments always take precedence:

``` r
# This uses the pinned version for ICD-10:
CODES("E10", type = "ICD-10")
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10

# This ignores the pin and uses "UKB v4" directly:
CODES("E10", type = "ICD-10", lookup_version = "UKB v4")
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10
```

To clear all pins and return to automatic `"latest"` resolution:

``` r
codeminer_clear_versions()
```

[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
shows any active pins alongside the connection info.

### Storing version settings

For reproducible analysis, you can store your version pins in a
configuration file and load them at the start of a session.

**CSV format** (one row per code type, columns for each table type):

    code_type,lookup,relationship
    ICD-10,UKB v4,UKB v4
    Read 3,UKB v4,UKB v4
    SNOMED CT,GPS v1,GPS v1

``` r
cfg <- read.csv("codeminer_versions.csv")
codeminer_set_version(
  lookup       = setNames(cfg$lookup, cfg$code_type),
  relationship = setNames(cfg$relationship, cfg$code_type)
)
```

Mapping pins use a `"from > to"` key format and are best stored in a
separate file or in JSON:

**JSON format:**

``` json
{
  "lookup": {"ICD-10": "UKB v4", "Read 3": "UKB v4"},
  "relationship": {"ICD-10": "UKB v4"},
  "mapping": {"Read 3 > ICD-10": "UKB v4"}
}
```

``` r
cfg <- jsonlite::fromJSON("codeminer_versions.json")
codeminer_set_version(
  lookup       = unlist(cfg$lookup),
  relationship = unlist(cfg$relationship),
  mapping      = unlist(cfg$mapping)
)
```

Denny, Joshua C., Lisa Bastarache, and Dan M. Roden. 2016. “Phenome-Wide
Association Studies as a Tool to Advance Precision Medicine.” *Annual
Review of Genomics and Human Genetics* 17 (August): 353–73.
<https://doi.org/10.1146/annurev-genom-090314-024956>.

Wu, Patrick, Aliya Gifford, Xiangrui Meng, Xue Li, Harry Campbell, Tim
Varley, Juan Zhao, et al. 2019. “Mapping ICD-10 and ICD-10-CM Codes to
Phecodes: Workflow Development and Initial Evaluation.” *JMIR medical
informatics* 7 (4): e14325. <https://doi.org/10.2196/14325>.
