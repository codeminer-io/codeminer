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
#> [1] "/tmp/RtmpUM4vMd/file28505c8a7af6.duckdb"
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmpUM4vMd/file28505c8a7af6.duckdb"
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
#>   Main: /tmp/RtmpUM4vMd/file28505c8a7af6.duckdb
```

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
#> ℹ Using 'UKB v4' as latest version

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
selected by default.

### How filters are defined

Filters are stored in table metadata as a JSON specification. Each
filterable column has an entry with `values` (all valid options) and
`defaults` (applied when no override is given):

``` r

# When adding a lookup table with filters
add_lookup_table(
  my_snomed_lookup,
  lookup_metadata(
    "SNOMED CT",
    lookup_version = "v1",
    col_filters = list(
      active_concept = list(
        values = c("0", "1"),
        defaults = c("1")
      )
    )
  )
)
```

### Query-time behaviour

All query functions accept a `col_filters` parameter with three options:

- **`"default"`** (the default): apply filters from session pin or
  metadata defaults
- **`NULL`**: no filtering — return all rows regardless of column values
- **A named list**: apply explicit filters for this call only

``` r

# Default: only active concepts (from metadata defaults)
CODES("all", type = "SNOMED CT")

# Override: return all rows including inactive
CODES("all", type = "SNOMED CT", col_filters = NULL)

# Custom: only inactive concepts
CODES("all", type = "SNOMED CT", col_filters = list(active_concept = c("0")))
```

### Filters are per-table-type

An important design point: **each table type has its own independent
col_filters**. This matters most for
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
which touches two table types:

1.  The **mapping table** (e.g., Read v3 → ICD-10) may have filters like
    `mapping_status`
2.  The **target lookup table** (e.g., ICD-10) may have filters like
    `active_concept`

When you call `MAP(col_filters = ...)`, this controls only the mapping
table. The target lookup table uses its own default filters when looking
up descriptions. This is intentional — the two tables have different
filterable columns and different semantics.

To override filters on the target lookup as well, use session pinning:

``` r

# Pin lookup filters for SNOMED CT
codeminer_set_col_filters(
  lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))
)

# Now MAP() will use the pinned lookup filters for the target table
MAP("24700007", from = "SNOMED CT", to = "ICD-10")
```

### Session pinning

Like version pinning, you can pin col_filters for the entire session:

``` r

# Pin: include inactive SNOMED concepts
codeminer_set_col_filters(
  lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))
)

# Clear all filter pins
codeminer_clear_col_filters()
```

### Temporary overrides

For a scoped override, use
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md):

``` r

# Temporarily include inactive concepts for this block only
result <- with_col_filters(
  {
    CODES("all", type = "SNOMED CT")
  },
  lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))
)
# Outside the block, default filters apply again
```

### Updating filters after table creation

If you need to add or change filters on an existing table without
re-adding the data:

``` r

update_lookup_metadata(
  "SNOMED CT",
  col_filters = list(
    active_concept = list(values = c("0", "1"), defaults = c("1"))
  )
)
```

### Discovering available filters

[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
returns all registered filters, useful for building UIs:

``` r

# Just defaults (for applying)
get_col_filters(defaults_only = TRUE)

# Full spec with all available values (for checkboxes in a Shiny app)
get_col_filters(defaults_only = FALSE)
```

Denny, Joshua C., Lisa Bastarache, and Dan M. Roden. 2016. “Phenome-Wide
Association Studies as a Tool to Advance Precision Medicine.” *Annual
Review of Genomics and Human Genetics* 17 (August): 353–73.
<https://doi.org/10.1146/annurev-genom-090314-024956>.

Wu, Patrick, Aliya Gifford, Xiangrui Meng, et al. 2019. “Mapping ICD-10
and ICD-10-CM Codes to Phecodes: Workflow Development and Initial
Evaluation.” *JMIR medical informatics* 7 (4): e14325.
<https://doi.org/10.2196/14325>.
