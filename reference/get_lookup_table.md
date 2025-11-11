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
  [cli::cli_abort](https://cli.r-lib.org/reference/cli_abort.html).

## Value

A data frame containing the lookup table with three columns: `code`,
`description` and `code_type`.
