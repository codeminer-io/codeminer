# Set up a dummy all_lkps_maps database

Set up a dummy all_lkps_maps database

## Usage

``` r
dummy_all_lkps_maps_db(db_path = tempfile(fileext = ".db"))
```

## Arguments

- db_path:

  Path to the database file. Defaults to a temporary file.

## Value

Returns the `db_path` invisibly

## See also

Other Dummy data:
[`build_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps_dummy.md),
[`dummy_all_lkps_maps_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_path.md),
[`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md),
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps_dummy.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
dummy_all_lkps_maps_db()
#> Writing lookup and mapping tables to Duckdb database at /tmp/RtmpF9QZHg/file1f4e2fce0fa1.db
#> metadata_all_lkps_maps
#> bnf_lkp
#> dmd_lkp
#> icd9_lkp
#> icd10_lkp
#> icd9_icd10
#> read_v2_lkp
#> read_v2_drugs_lkp
#> read_v2_drugs_bnf
#> read_v2_icd9
#> read_v2_icd10
#> read_v2_opcs4
#> read_v2_read_ctv3
#> read_ctv3_lkp
#> read_ctv3_icd9
#> read_ctv3_icd10
#> read_ctv3_opcs4
#> read_ctv3_read_v2
#> opcs4_lkp
#> self_report_cancer
#> self_report_medication
#> self_report_operation
#> self_report_non_cancer
#> phecode_lkp
#> icd10_phecode
#> metadata_codeminer
#> Success! Connect to database with `con <- DBI::dbConnect(duckdb::duckdb(), '/tmp/RtmpF9QZHg/file1f4e2fce0fa1.db', read_only = TRUE)`, then access all tables with `all_lkps_maps <- ukbwranglr::db_tables_to_list(con)`
```
