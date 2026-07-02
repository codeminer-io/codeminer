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
coding systems, and using Phecodes(Denny et al. 2016; Wu et al. 2019)
with UK Biobank data. See vignettes
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
#> ✔ Dummy database ready to use!
#> [1] "/tmp/Rtmpd4PC8B/file3a565f1983eb.duckdb"
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/Rtmpd4PC8B/file3a565f1983eb.duckdb"
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
#> ℹ Workbench active
#>   Database: /tmp/Rtmpd4PC8B/file3a565f1983eb.duckdb
```

### Single file vs folder

`CODEMINER_DB_PATH` can point at either a single `.duckdb` file (the
default) **or** an existing directory. The directory form holds the
per-table data files separately at the folder root, which makes the
database easier to inspect, distribute, or sync to remote storage.
Choose the layout when you call
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md):

``` r

# Single .duckdb file (default)
Sys.setenv(CODEMINER_DB_PATH = "/path/to/codeminer.duckdb")
build_database()

# Folder, data tables as per-table .duckdb files (matches single-file
# query performance; recommended folder layout)
dir.create("/path/to/codeminer-folder")
Sys.setenv(CODEMINER_DB_PATH = "/path/to/codeminer-folder")
build_database(format = "duckdb")

# Folder, data tables as parquet (~35% smaller on disk; slower
# recursive queries like CHILDREN())
dir.create("/path/to/codeminer-parquet")
Sys.setenv(CODEMINER_DB_PATH = "/path/to/codeminer-parquet")
build_database(format = "parquet")
```

[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md)
figures out which layout you have automatically — no flags to remember
at query time.

## Build a clinical code list

### Explore codes

Codes may be explored with:

- [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md):
  look up descriptions for a set of code in the given code system type

``` r

CODES(
  c("E10", "E11"),
  type = "ICD-10"
)
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
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
#> <codeminer_codelist>: 2 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 2 × 3
#>   code  description          code_type
#>   <chr> <chr>                <chr>    
#> 1 L721  Trichilemmal cyst    ICD-10   
#> 2 N330  Tuberculous cystitis ICD-10
```

### Reviewing a draft codelist as a tree

A common QA workflow is to draft a broad codelist with parent codes only
— e.g. for diabetes mellitus — and then verify the descendant coverage,
spotting any leaf codes worth including explicitly.

[`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)
returns a flat `list(nodes, edges)` suitable for downstream hierarchy
analysis or tree rendering (e.g. `data.tree`, `ggraph`, `visNetwork`).

``` r

draft <- c("E10", "E11", "E13", "E14")
tree <- get_relationship_tree(draft, type = "ICD-10")
#> ℹ Using "UKB v4" as the latest relationship version for
#> "ICD-10".

tree$nodes
#> # A tibble: 44 × 4
#>    code  term                                              category in_input_set
#>    <chr> <chr>                                             <chr>    <lgl>       
#>  1 E10   Type 1 diabetes mellitus                          NA       TRUE        
#>  2 E100  Type 1 diabetes mellitus With coma                NA       FALSE       
#>  3 E101  Type 1 diabetes mellitus With ketoacidosis        NA       FALSE       
#>  4 E102  Type 1 diabetes mellitus With renal complications NA       FALSE       
#>  5 E103  Type 1 diabetes mellitus With ophthalmic complic… NA       FALSE       
#>  6 E104  Type 1 diabetes mellitus With neurological compl… NA       FALSE       
#>  7 E105  Type 1 diabetes mellitus With peripheral circula… NA       FALSE       
#>  8 E106  Type 1 diabetes mellitus With other specified co… NA       FALSE       
#>  9 E107  Type 1 diabetes mellitus With multiple complicat… NA       FALSE       
#> 10 E108  Type 1 diabetes mellitus With unspecified compli… NA       FALSE       
#> # ℹ 34 more rows
tree$edges
#> # A tibble: 40 × 2
#>    parent child
#>    <chr>  <chr>
#>  1 E10    E100 
#>  2 E10    E101 
#>  3 E10    E102 
#>  4 E10    E103 
#>  5 E10    E104 
#>  6 E10    E105 
#>  7 E10    E106 
#>  8 E10    E107 
#>  9 E10    E108 
#> 10 E10    E109 
#> # ℹ 30 more rows
```

The `in_input_set` column on `nodes` flags codes that were in the
original draft (`TRUE`) versus codes that were picked up by descendant
expansion (`FALSE`). To inspect the subcodes the expansion added:

``` r

subset(tree$nodes, !in_input_set)
#> # A tibble: 40 × 4
#>    code  term                                              category in_input_set
#>    <chr> <chr>                                             <chr>    <lgl>       
#>  1 E100  Type 1 diabetes mellitus With coma                NA       FALSE       
#>  2 E101  Type 1 diabetes mellitus With ketoacidosis        NA       FALSE       
#>  3 E102  Type 1 diabetes mellitus With renal complications NA       FALSE       
#>  4 E103  Type 1 diabetes mellitus With ophthalmic complic… NA       FALSE       
#>  5 E104  Type 1 diabetes mellitus With neurological compl… NA       FALSE       
#>  6 E105  Type 1 diabetes mellitus With peripheral circula… NA       FALSE       
#>  7 E106  Type 1 diabetes mellitus With other specified co… NA       FALSE       
#>  8 E107  Type 1 diabetes mellitus With multiple complicat… NA       FALSE       
#>  9 E108  Type 1 diabetes mellitus With unspecified compli… NA       FALSE       
#> 10 E109  Type 1 diabetes mellitus Without complications    NA       FALSE       
#> # ℹ 30 more rows
```

The helper restricts edges to the hierarchical parent/child relationship
type only, and applies `endpoints = "both"` internally — so every edge
endpoint is guaranteed to appear in `nodes` (no dangling edges).

For a defined codelist, the new `codes` argument on
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md)
is the most direct way to pull the matching terms + categories:

``` r

get_lookup_table("ICD-10", codes = draft) |> dplyr::collect()
#> # A tibble: 4 × 15
#>   code  description   ICD10_CODE USAGE USAGE_UK MODIFIER_4 MODIFIER_5 QUALIFIERS
#>   <chr> <chr>         <chr>      <chr> <chr>    <chr>      <chr>      <chr>     
#> 1 E10   Type 1 diabe… E10        DEFA… 3        NA         NA         NA        
#> 2 E11   Type 2 diabe… E11        DEFA… 3        NA         NA         NA        
#> 3 E13   Other specif… E13        DEFA… 3        NA         NA         NA        
#> 4 E14   Unspecified … E14        DEFA… 3        NA         NA         NA        
#> # ℹ 7 more variables: GENDER_MASK <chr>, MIN_AGE <chr>, MAX_AGE <chr>,
#> #   TREE_DESCRIPTION <chr>, code_type <chr>, preferred_description <lgl>,
#> #   category <chr>
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
#> ℹ Using "v1" as the latest lookup version for "custom_codes".
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
#> 5       Read v2_UKB v4       Read v2         UKB v4       read_code
#> 6 Read v2 drugs_UKB v4 Read v2 drugs         UKB v4       read_code
#> 7       Read v3_UKB v4       Read v3         UKB v4       read_code
#>   lookup_description_col lookup_category_col
#> 1            Description         BNF_Chapter
#> 2                   term                <NA>
#> 3       DESCRIPTION_ICD9                <NA>
#> 4            DESCRIPTION                <NA>
#> 5       term_description                <NA>
#> 6       term_description                <NA>
#> 7       term_description                <NA>
#>                                        lookup_source preferred_description_col
#> 1 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 2 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 3 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 4 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 5 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                 term_code
#> 6 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592                      <NA>
#> 7 https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592          description_type
#>   preferred_description_indicator col_filters
#> 1                            <NA>        <NA>
#> 2                            <NA>        <NA>
#> 3                            <NA>        <NA>
#> 4                            <NA>        <NA>
#> 5                              00        <NA>
#> 6                            <NA>        <NA>
#> 7                               P        <NA>
```

## Version pinning

When multiple versions of a lookup, mapping, or relationship table are
available, `codeminer` resolves `"latest"` automatically. The first time
a query function resolves `"latest"` for a given code type, the resolved
version is cached for the remainder of the session. This avoids repeated
informational messages and ensures consistent version usage across a
workflow.

You can override this for the current session with
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md):

``` r

# Pin lookup and relationship versions for a code type
codeminer_set_version(
  lookup       = c("ICD-10" = "UKB v4"),
  relationship = c("ICD-10" = "UKB v4")
)

# Pin a mapping version (use "from > to" format for the key)
codeminer_set_version(
  mapping = c("Read v3 > ICD-10" = "UKB v4")
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

To clear all version selections and return to automatic `"latest"`
resolution:

``` r

codeminer_clear_versions()
```

You can also clear versions selectively by code type:

``` r

# Clear only the ICD-10 lookup version
codeminer_clear_versions(lookup = "ICD-10")

# Clear lookup and relationship for SNOMED CT
codeminer_clear_versions(
  lookup = "SNOMED CT",
  relationship = "SNOMED CT"
)
```

[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
shows any active versions alongside the connection info.

### Storing version settings

For reproducible analysis, you can store your version pins in a
configuration file and load them at the start of a session.

**CSV format** (one row per code type, columns for each table type):

    code_type,lookup,relationship
    ICD-10,UKB v4,UKB v4
    Read v3,UKB v4,UKB v4
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
  "lookup": {"ICD-10": "UKB v4", "Read v3": "UKB v4"},
  "relationship": {"ICD-10": "UKB v4"},
  "mapping": {"Read v3 > ICD-10": "UKB v4"}
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

## Column filters

Some tables contain rows that should be excluded by default — for
example, inactive SNOMED CT concepts or approximate code mappings.
Column filters (`col_filters`) let table authors declare which columns
are filterable, what values are available, and which values should be
selected by default. You can override them per call, per session, or per
scope; the model in one sentence:

> For each table a query touches: the call’s `col_filters` entry, else
> the session pin, else the metadata defaults — first match wins,
> replacing the whole set; `NA` means unfiltered at whatever level you
> apply it.

``` r

# Defaults (from metadata / session pins)
CODES("all", type = "SNOMED CT")

# No filtering for this call
CODES("all", type = "SNOMED CT", col_filters = NULL)

# Table-keyed override — reaches every table the query touches,
# e.g. both the mapping table and the target lookup in MAP()
MAP(
  bnf_codes,
  from = "BNF",
  to = "SNOMED CT",
  col_filters = list(
    mapping = list("BNF > SNOMED CT" = list(assured = "Y")),
    lookup  = list("SNOMED CT" = list(moduleId_concept = "999000011000001104"))
  )
)
```

Session pinning works like version pinning
([`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md)
/
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)),
scoped overrides use
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md),
and
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
returns the registered filters in the same shape for amend-and-pass-back
workflows. See
[`vignette("col_filters")`](https://codeminer-io.github.io/codeminer/articles/col_filters.md)
for the full guide.

Denny, Joshua C., Lisa Bastarache, and Dan M. Roden. 2016. “Phenome-Wide
Association Studies as a Tool to Advance Precision Medicine.” *Annual
Review of Genomics and Human Genetics* 17 (August): 353–73.
<https://doi.org/10.1146/annurev-genom-090314-024956>.

Wu, Patrick, Aliya Gifford, Xiangrui Meng, et al. 2019. “Mapping ICD-10
and ICD-10-CM Codes to Phecodes: Workflow Development and Initial
Evaluation.” *JMIR medical informatics* 7 (4): e14325.
<https://doi.org/10.2196/14325>.
