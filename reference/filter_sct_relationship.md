# Filter SNOMED CT relationships based on various criteria

Filter SNOMED CT relationships based on various criteria

## Usage

``` r
filter_sct_relationship(
  codes = NULL,
  sourceId_filter = NULL,
  destinationId_filter = NULL,
  typeId_filter = NULL,
  active_only = TRUE,
  recursive = FALSE,
  all_lkps_maps = NULL
)
```

## Arguments

- codes:

  A vector of SNOMED CT codes to filter relationships by.

- sourceId_filter:

  A vector of SNOMED CT codes to filter relationships by source.

- destinationId_filter:

  A vector of SNOMED CT codes to filter relationships by destination.

- typeId_filter:

  A vector of SNOMED CT codes to filter relationships by type.

- active_only:

  Logical indicating whether to filter relationships by active status.

- recursive:

  Logical indicating whether to filter relationships recursively.

- all_lkps_maps:

  A list of lookup maps.

## Value

A data frame of filtered SNOMED CT relationships.
