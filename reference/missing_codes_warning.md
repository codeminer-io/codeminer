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
#> Creating new database at /tmp/RtmpE2JCfn/file1c246c0bd69e.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Relationship table icd10_relationship_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
missing_codes <- table_type <- table_meta <- NULL

# Capture missing codes from waning using `withCallingHandlers()`
withCallingHandlers(
  {
    codes <- CODES(c("foo", "bar", "E10"), code_type = "icd10")
  },
  codeminer_missing_codes = function(w) {
    missing_codes <<- w$missing_codes
    table_type <<- w$table_type
    table_meta <<- w$table_meta
    invokeRestart("muffleWarning")
  }
)
#> Error in CODES(c("foo", "bar", "E10"), code_type = "icd10"): Code type 'icd10' not found in lookup metadata.
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
