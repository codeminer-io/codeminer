# Get the mapping table for the given from and to types in standardised format

Get the mapping table for the given from and to types in standardised
format

## Usage

``` r
get_mapping_table(con, from, to, version, call = rlang::caller_env())
```

## Arguments

- con:

  A database connection.

- from:

  The source code type to map from

- to:

  The target code type to map to

- version:

  The version of the mapping table.

- call:

  The calling environment. Passed to
  [codeminer_abort](https://codeminer-io.github.io/codeminer/reference/codeminer_abort.md).

## Value

A data frame containing the lookup table with two columns: `from` and
`to`.
