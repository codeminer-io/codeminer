# Read CPRD Aurum code browser files into R

Reads the medical and product dictionaries from a CPRD Aurum code
browser release (for example `CPRD_CodeBrowser_YYYYMM_Aurum/`,
containing `CPRDAurumMedical.txt` and `CPRDAurumProduct.txt`) and
returns lookup tables for medcodes and prodcodes, plus mapping tables to
SNOMED CT and Read v2.

## Usage

``` r
read_cprd_aurum(
  path,
  tables = cprd_aurum_tables(),
  version = NULL,
  source = "CPRD Aurum code browser"
)
```

## Arguments

- path:

  Path to the CPRD Aurum code browser release directory, containing
  `CPRDAurumMedical.txt` and `CPRDAurumProduct.txt`. Only the files
  needed for `tables` must be present.

- tables:

  Character vector of tables to read. Any of `"cprd_aurum_medcode_lkp"`,
  `"cprd_aurum_prodcode_lkp"`, `"cprd_aurum_medcode_snomed"`,
  `"cprd_aurum_medcode_read2"` and `"cprd_aurum_prodcode_snomed"`.
  Defaults to all of them. The `medcode` tables come from
  `CPRDAurumMedical.txt` and the `prodcode` tables from
  `CPRDAurumProduct.txt`.

- version:

  Character string for the version label. If `NULL` (default), the
  release date (`yyyymm`) is taken from a directory name like
  `CPRD_CodeBrowser_YYYYMM_Aurum`, with a message saying which version
  is used. If the name doesn't include a release date, an error asks for
  `version` to be supplied.

- source:

  Character string for the data source.

## Value

A named list with one element per requested table. Each holds `lookup`
or `mapping`, a list with `table` (data.table) and `metadata`.

## Details

CPRD supplies the code browser to researchers with access to its data,
and it can't be downloaded publicly, so there is no `get_*()` function:
supply the path to your local copy.

## Columns

The lookups keep every column of the dictionary files. Column names are
converted to lower case, with spaces replaced by underscores
(`MedCodeId` becomes `medcodeid`, `Term from EMIS` becomes
`term_from_emis`).

Column types are declared rather than guessed: `observations` and
`drugissues` are integer counts, and every other column, including the
numeric identifiers, is character. An error is raised if an expected
column is missing. A column the reader doesn't know about is read as
character, with a warning.

## Column filters

The medcode lookup's `emiscodecategoryid`, and the prodcode lookup's
`routeofadministration` and `formulation`, are column filters (see
[`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)).
Their values are taken from the release, and every value is included by
default, so no codes are hidden by default. The EMIS code category IDs
have no labels.

## Blank values

Empty fields are read as `NA`, except in the column filter columns,
which keep them as empty strings (`""`). Filters match exact values and
drop `NA`, so the empty string is kept as a filter value in its own
right.

## Mappings

- **medcode to SNOMED CT** (`cprd_aurum_medcode_snomed`): from
  `medcodeid` to `snomedctconceptid`, keeping `snomedctdescriptionid`.

- **medcode to Read v2** (`cprd_aurum_medcode_read2`): from `medcodeid`
  to `read2_code`, the first 5 characters of `CleansedReadCode`, keeping
  the 2-character `read2_term_code`. Rows without a cleansed Read code
  are dropped. `OriginalReadCode` is kept in the medcode lookup but not
  mapped.

- **prodcode to SNOMED CT** (`cprd_aurum_prodcode_snomed`): from
  `prodcodeid` to `dmdid`. dm+d identifiers are SNOMED CT concepts, so
  this links prodcodes to the dm+d hierarchy in SNOMED CT. Rows without
  a `dmdid` are dropped.

Codes are not checked against the target code systems. A code that isn't
in the target lookup raises a warning when mapped.

## See also

[`add_cprd_aurum()`](https://codeminer-io.github.io/codeminer/reference/add_cprd_aurum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- read_cprd_aurum("path/to/CPRD_CodeBrowser_YYYYMM_Aurum")
result$cprd_aurum_medcode_lkp$lookup$table
} # }
```
