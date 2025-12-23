# Retrieve parent or child codes

Returns immediate or transitive parent or child codes for the given
codes by traversing the relationship graph.

## Usage

``` r
CHILDREN(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

PARENTS(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

N_CHILDREN(
  codes,
  depth = 1,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

N_PARENTS(
  codes,
  depth = 1,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)
```

## Arguments

- codes:

  Character vector of codes to start from.

- code_type:

  Code type (character).

- lookup_version:

  Lookup table version (character).

- relationship_version:

  Relationship table version (character).

- codes_only:

  Logical. If `TRUE`, return only unique codes. If `FALSE`, return a
  data frame with code and description.

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- depth:

  Integer. Maximum number of steps to traverse. Use `Inf` for transitive
  closure (all ancestors/descendants). Only used by `N_PARENTS()` and
  `N_CHILDREN()`.

## Value

A data frame of codes and descriptions, or a character vector if
`codes_only = TRUE`.

## Details

Use `N_PARENTS()`/`N_CHILDREN()` for immediate relationships (one step),
and `PARENTS()`/`CHILDREN()` for transitive closure (all reachable
ancestors/descendants).

## See also

Other Code relationships:
[`attributes()`](https://codeminer-io.github.io/codeminer/reference/attributes.md),
[`relationship_types`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)

## Examples

``` r
create_dummy_database()
#> Creating new database at /tmp/RtmpE2JCfn/file1c241a7b1027.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Relationship table icd10_relationship_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
PARENTS(c("E10", "E11"), code_type = "icd10")
#> Error in N_PARENTS(codes, depth = Inf, code_type = code_type, lookup_version = lookup_version,     relationship_version = relationship_version, codes_only = codes_only,     preferred_description_only = preferred_description_only): Code type 'icd10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
CHILDREN(c("E10", "E11"), code_type = "icd10")
#> Error in N_CHILDREN(codes, depth = Inf, code_type = code_type, lookup_version = lookup_version,     relationship_version = relationship_version, codes_only = codes_only,     preferred_description_only = preferred_description_only): Code type 'icd10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
N_PARENTS(c("E10", "E11"), code_type = "icd10")
#> Error in N_PARENTS(c("E10", "E11"), code_type = "icd10"): Code type 'icd10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
N_CHILDREN(c("E10", "E11"), code_type = "icd10")
#> Error in N_CHILDREN(c("E10", "E11"), code_type = "icd10"): Code type 'icd10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
```
