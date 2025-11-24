# CodeMiner condition constructors *(internal helpers)*

Internal helpers for constructing structured CodeMiner error, warning,
and informational conditions.

## Usage

``` r
codeminer_abort(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
)

codeminer_warn(message, class = NULL, ..., .envir = rlang::caller_env())

codeminer_inform(message, class = NULL, ..., .envir = rlang::caller_env())
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

  Call environment (only used by `codeminer_abort()`).

- .envir:

  Environment to evaluate the glue expressions in.

## Details

These wrap the corresponding `cli` signalling functions
([`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html),
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html))
and always append a CodeMiner-specific base class (`codeminer_error`,
`codeminer_warning`, `codeminer_message`).

Additional custom classes may be optionally prepended via the `class`
argument.

The original named `cli` message vector is stored in `cli_message` (see
examples).

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
  error = function(cnd) cnd
)

# Inspect condition class hierarchy
class(e)
#> [1] "simpleError" "error"       "condition"  

# Inspect stored structured `cli` message
e$cli_message
#> NULL

# Inspect the default formatted condition message
conditionMessage(e)
#> [1] "could not find function \"codeminer_abort\""

# This can be re-thrown to provide an identical error
f <- tryCatch(cli::cli_abort(e$cli_message), error = function(cnd) cnd)

identical(conditionMessage(f), conditionMessage(e))
#> [1] FALSE
```
