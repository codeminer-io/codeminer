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
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/Rtmp7fg0Bd/file1c7dd0fd369.duckdb")`
#>   `codeminer_connect()`
missing_codes <- table_type <- table_meta <- NULL

# Capture missing codes from waning using `withCallingHandlers()`
withCallingHandlers(
  {
    codes <- CODES(c("foo", "bar", "E10"), type = "ICD-10")
  },
  codeminer_missing_codes = function(w) {
    missing_codes <<- w$missing_codes
    table_type <<- w$table_type
    table_meta <<- w$table_meta
    invokeRestart("muffleWarning")
  }
)
#> ℹ Using 'UKB v4' as latest version

# Recognised codes
codes
#> <codeminer_codelist>: 1 code
#> Code type: "ICD-10"
#> 
#> # A tibble: 1 × 3
#>   code  description              code_type
#>   <chr> <chr>                    <chr>    
#> 1 E10   Type 1 diabetes mellitus ICD-10   

# Unrecognised codes and related context
missing_codes
#> [1] "bar" "foo"
table_type
#> [1] "lookup"
table_meta
#>   lookup_table_name code_type lookup_version lookup_code_col
#> 4     ICD-10_UKB v4    ICD-10         UKB v4        ALT_CODE
#>   lookup_description_col                                      lookup_source
#> 4            DESCRIPTION https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592
#>   preferred_description_col preferred_description_indicator col_filters
#> 4                      <NA>                            <NA>        <NA>
```
