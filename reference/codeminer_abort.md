# CodeMiner error constructor *(internal helper)*

*For internal package used only.* A wrapper around
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
that creates a structured CodeMiner error condition.

## Usage

``` r
codeminer_abort(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
)
```

## Arguments

- message:

  It is formatted via a call to
  [`cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html).

- class:

  Optional character vector of additional classes to prepend before
  `codeminer_error`.

- ...:

  Passed through to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

- call:

  Call environment.

- .envir:

  Environment to evaluate the glue expressions in.

## Details

A base class `codeminer_error` is always appended to the condition class
vector, and additional custom classes may be optionally prepended via
the `class` argument.

The original named `cli` message vector is stored in `cli_error_message`
for use by the CodeMiner API (see examples).

## Examples

``` r
# Capture a CodeMiner error condition and inspect it
invalid_code_type <- "foo"

named_cli_message_vector <- c(
  x = "Code type {.arg {invalid_code_type}} not found.",
  i = "Use `add_lookup_table()`."
)

e <- tryCatch(
  codeminer_abort(
    named_cli_message_vector,
    class = "codeminer_arg_validation_error"
  ),
  error = function(e) e
)

# Inspect condition class hierarchy
class(e)
#> [1] "simpleError" "error"       "condition"  

# Inspect stored structured `cli` message
e$cli_error_message
#> NULL

# Inspect the default formatted condition message
conditionMessage(e)
#> [1] "could not find function \"codeminer_abort\""
```
