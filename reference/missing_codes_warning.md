# Warn about missing codes in a table

Emits a truncated warning listing codes that were not found in a given
table. The warning carries the missing codes and table context as
structured data and can be handled programmatically.

## Usage

``` r
missing_codes_warning(
  missing_codes,
  table_meta,
  table_type = c("lookup", "mapping", "relationship"),
  max_show = 10
)
```

## Arguments

- missing_codes:

  Character vector of all missing codes (untruncated).

- table_meta:

  Data frame of metadata for the table the codes were expected to be
  found in.

- table_type:

  Type of table the codes were expected to be found in. Must be one of
  'relationship', 'lookup', or 'mapping'.

- max_show:

  Maximum number of missing codes to display. Defaults to 10.

## Value

Invisibly returns `missing_codes`.

## Details

### Handling missing-code warnings

This function signals a warning of class `"codeminer_missing_codes"`.
The warning object contains the following fields:

- `missing_codes`: a character vector of all missing codes (untruncated)

- `table_type`: the type of table the codes were expected to be found in
  (e.g. `"lookup"`, `"mapping"`, `"relationship"`)

- `table_meta`: metadata for the table that was queried (including table
  name, version, code type, and source)

You can intercept and act on these warnings using
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) (see
examples). This allows you to collect missing inputs without printing a
warning, while still preserving full context about where the lookup
failed.

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> Creating new database at /tmp/RtmpLBgdnU/file1c6539a101b0.duckdb
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
missing_codes <- table_type <- table_meta <- NULL

# Capture missing codes from waning using `withCallingHandlers()`
withCallingHandlers(
  {
    codes <- CODES(c("foo", "bar", "E10"), code_type = "ICD-10")
  },
  codeminer_missing_codes = function(w) {
    missing_codes <<- w$missing_codes
    table_type <<- w$table_type
    table_meta <<- w$table_meta
    invokeRestart("muffleWarning")
  }
)
#> Error in CODES(c("foo", "bar", "E10"), code_type = "ICD-10"): Code type 'ICD-10' not found in lookup metadata.
#> ℹ Did you add the lookup table with `codeminer::add_lookup_table()`?

# Recognised codes
codes
#> Error: object 'codes' not found

# Unrecognised codes and related context
missing_codes
#> NULL
table_type
#> NULL
table_meta
#> NULL
```
