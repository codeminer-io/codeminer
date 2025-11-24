# Get the lookup table for the given code type in standardised format

Get the lookup table for the given code type in standardised format

## Usage

``` r
get_lookup_table(con, code_type, version, call = rlang::caller_env())
```

## Arguments

- con:

  A database connection.

- code_type:

  The code type for which to retrieve the lookup table.

- call:

  The calling environment. Passed to
  [codeminer_abort](https://codeminer-io.github.io/codeminer/reference/conditions.md).

## Value

A data frame containing the lookup table with three columns: `code`,
`description` and `code_type`.
