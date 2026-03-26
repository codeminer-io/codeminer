# Collect and validate codes input from ... argument

Not a user-side function. Exported for use by packages that build on top
of codeminer. Most users should not need to call this directly.

## Usage

``` r
collect_codes_input(
  ...,
  type = NULL,
  allow_empty = TRUE,
  call = rlang::caller_env()
)
```

## Arguments

- ...:

  Code inputs - can be character vectors, a single data frame, or empty

- type:

  Optional code type to validate against

- allow_empty:

  Whether to allow empty input (default TRUE)

- call:

  The calling environment for error messages

## Value

A list with two elements:

- `codes`: Character vector of codes

- `code_type`: String code type (or NULL if not determinable)

## Details

Collects codes from the `...` argument, validates them, and returns a
standardised list containing the codes vector and code_type. Handles
empty input, single data frames, character vectors, and \|\| separated
strings.
