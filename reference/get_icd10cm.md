# Get ICD-10-CM release files

Downloads an ICD-10-CM "code descriptions in tabular order" release from
the National Center for Health Statistics (NCHS), part of the Centers
for Disease Control and Prevention (CDC). ICD-10-CM is the US clinical
modification of ICD-10, released for each US fiscal year (effective 1
October) with occasional mid-year updates.

## Usage

``` r
get_icd10cm(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dir_path:

  Directory path to download to. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- release:

  Character string specifying which release to download. Either:

  - `"latest"` (default) — the most recent release known to the package.

  - An 8-digit effective date (`"YYYYMMDD"`), e.g. `"20261001"` for the
    FY2027 release. See Details for the available releases.

- overwrite:

  Logical. If `TRUE`, re-downloads and overwrites an existing file.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded release (invisibly). Depending on how NCHS
published that release this is either a `.zip` containing the order file
or the order `.txt` file itself; either is accepted by
[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md).
The file is named `icd10cm_order_<release>.<ext>` so the release date
can be recovered from the file name.

## Details

NCHS does not name release files consistently from year to year, so the
package carries a table of verified download URLs keyed by effective
date. Releases known to the package (effective date, fiscal year),
generated from that table when the documentation is built:

- `20151001` (FY2016)

- `20161001` (FY2017)

- `20171001` (FY2018)

- `20181001` (FY2019)

- `20191001` (FY2020)

- `20201001` (FY2021)

- `20210101` (FY2021 January update)

- `20211001` (FY2022)

- `20220401` (FY2022 April update)

- `20221001` (FY2023)

- `20230401` (FY2023 April update)

- `20231001` (FY2024)

- `20241001` (FY2025)

- `20251001` (FY2026)

- `20261001` (FY2027)

Releases are keyed by their full effective date, so a fiscal year with a
mid-year update has two entries (e.g. `20221001` and `20230401`). The
April 2024, April 2025 and April 2026 updates are deliberately absent:
their code descriptions match the previous October's release, so they
would give an identical code set under a second version label. A release
newer than this table can be downloaded by hand from the NCHS listing
and passed to
[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md)
or
[`add_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/add_icd10cm.md)
with an explicit `version`.

## Attribution

ICD-10-CM is published by NCHS/CDC and is available free of charge at
<https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files
does not imply endorsement by CDC, HHS or the US Government.

## See also

[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md),
[`add_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/add_icd10cm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_icd10cm()
read_icd10cm(path)

# A specific release
get_icd10cm(release = "20250401")
} # }
```
