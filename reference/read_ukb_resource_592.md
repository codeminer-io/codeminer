# Read UK Biobank Resource 592 clinical code mappings

Reads selected sheets from the UK Biobank Resource 592 Excel file
(`all_lkps_maps_v4.xlsx`, [resource
592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) and returns
lookup tables, mapping tables, and relationship tables with associated
metadata.

The function automatically includes required dependencies for sheets
that need extension processing. For example, requesting
`"read_v2_drugs_bnf"` will automatically include `"bnf_lkp"` and
`"read_v2_drugs_lkp"` for extending the mapping with BNF hierarchy
information.

## Usage

``` r
read_ukb_resource_592(
  path = get_ukb_resource_592(),
  sheets = c("bnf_lkp", "dmd_lkp", "icd9_lkp", "icd10_lkp", "icd9_icd10", "read_v2_lkp",
    "read_v2_drugs_lkp", "read_v2_drugs_bnf", "read_v2_icd9", "read_v2_icd10",
    "read_v2_opcs4", "read_v2_read_ctv3", "read_ctv3_lkp", "read_ctv3_icd9",
    "read_ctv3_icd10", "read_ctv3_opcs4", "read_ctv3_read_v2"),
  ukb_version = "UKB v4",
  ukb_source = "https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592"
)
```

## Arguments

- path:

  Path to the UKB Resource 592 Excel file. Default uses
  [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  to download the file if needed.

- sheets:

  Character vector of sheet names to read. Available sheets are:

  - Lookup tables: `"bnf_lkp"`, `"dmd_lkp"`, `"icd9_lkp"`,
    `"icd10_lkp"`, `"read_v2_lkp"`, `"read_v2_drugs_lkp"`,
    `"read_ctv3_lkp"`

  - Mapping tables: `"icd9_icd10"`, `"read_v2_drugs_bnf"`,
    `"read_v2_icd9"`, `"read_v2_icd10"`, `"read_v2_opcs4"`,
    `"read_v2_read_ctv3"`, `"read_ctv3_icd9"`, `"read_ctv3_icd10"`,
    `"read_ctv3_opcs4"`, `"read_ctv3_read_v2"`

- ukb_version:

  Version label for the UKB resource (default: `"UKB v4"`).

- ukb_source:

  Source URL or description for the UKB resource.

## Value

A named list where each element corresponds to a requested sheet. Each
sheet contains:

- `lookup`: A list with `table` (data frame) and `metadata` (list)

- `mapping`: A list with `table` (data frame) and `metadata` (list)

- `relationship`: A list with `table` (data frame) and `metadata` (list)

Sheets may contain one or more of these components depending on the
data.

## Details

### Auto-dependency inclusion

Some sheets require additional sheets for post-processing:

- `"read_v2_drugs_bnf"` requires `"bnf_lkp"` and `"read_v2_drugs_lkp"`

- `"read_v2_icd10"` requires `"icd10_lkp"`

Missing dependencies are automatically added with an informative
message.

### Extension processing

When dependencies are present, the function performs post-processing:

- `"read_v2_drugs_bnf"`: Extends with BNF hierarchy (chapter, section,
  paragraph) and Read code descriptions

- `"read_v2_icd10"`: Expands ICD-10 code ranges (e.g., "E100-E109") and
  extracts dagger/asterisk indicators

## Examples

``` r
# Read a single sheet using dummy data
result <- read_ukb_resource_592(
  path = dummy_ukb_resource_592_path(),
  sheets = "icd10_lkp"
)
#> Reading 1 selected table from UKB Resource 592
#> 

# Access the lookup table and metadata
icd10_lookup <- result$icd10_lkp$lookup$table
icd10_metadata <- result$icd10_lkp$lookup$metadata

# Read multiple sheets
result <- read_ukb_resource_592(
  path = dummy_ukb_resource_592_path(),
  sheets = c("icd9_lkp", "icd10_lkp", "icd9_icd10")
)
#> Reading 3 selected tables from UKB Resource 592
#> 

# Auto-dependency: requesting read_v2_drugs_bnf automatically includes
# bnf_lkp and read_v2_drugs_lkp for extension
result <- read_ukb_resource_592(
  path = dummy_ukb_resource_592_path(),
  sheets = "read_v2_drugs_bnf"
)
#> Adding bnf_lkp and read_v2_drugs_lkp (required for extending read_v2_drugs_bnf)
#> Reading 3 selected tables from UKB Resource 592
#> 
#> Extending read_v2_drugs_bnf with BNF hierarchy and descriptions

# Extended table includes BNF hierarchy
extended_table <- result$read_v2_drugs_bnf$mapping$table
names(extended_table) # includes BNF_Chapter, BNF_Section, etc.
#>  [1] "read_code"             "bnf_code"              "term_description"     
#>  [4] "status_flag"           "bnf_chapter_code"      "bnf_section_code"     
#>  [7] "bnf_paragraph_code"    "bnf_subparagraph_code" "BNF_Chapter"          
#> [10] "BNF_Section"           "BNF_Paragraph"         "BNF_Subparagraph"     
```
