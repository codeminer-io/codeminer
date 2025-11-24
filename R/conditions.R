#' CodeMiner condition constructors *(internal helpers)*
#'
#' Internal helpers for constructing structured CodeMiner error, warning, and
#' informational conditions.
#'
#' These wrap the corresponding `cli` signalling
#' functions ([cli::cli_abort()], [cli::cli_warn()], [cli::cli_inform()]) and
#' always append a CodeMiner-specific base class (`codeminer_error`,
#' `codeminer_warning`, `codeminer_message`).
#'
#' Additional custom classes may be optionally prepended via the
#' `class` argument.
#'
#' The original named `cli` message vector is stored in `cli_message` (see examples).
#'
#' @inheritParams cli::cli_abort
#' @param class Optional character vector of additional classes to prepend
#'   before `codeminer_error`.
#' @param ... Passed through to [cli::cli_abort()].
#' @param call Call environment (only used by [codeminer_abort()]).
#'
#' @keywords internal
#' @name conditions
#' @examples
#' # Capture a CodeMiner error condition and inspect it
#' invalid_code_type <- "foo"
#'
#' named_cli_message_vector <- c(
#'   x = "Code type {.arg {invalid_code_type}} not found.",
#'   i = "Use `add_lookup_table()`."
#' )
#'
#' e <- tryCatch(
#'   codeminer_abort(
#'     named_cli_message_vector,
#'     class = "codeminer_arg_validation_error"
#'   ),
#'   error = function(cnd) cnd
#' )
#'
#' # Inspect condition class hierarchy
#' class(e)
#'
#' # Inspect stored structured `cli` message
#' e$cli_message
#'
#' # Inspect the default formatted condition message
#' conditionMessage(e)
#'
#' # This can be re-thrown to provide an identical error
#' f <- tryCatch(cli::cli_abort(e$cli_message), error = function(cnd) cnd)
#'
#' identical(conditionMessage(f), conditionMessage(e))
NULL

#' @rdname conditions
codeminer_abort <- function(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  message_interpolated <- codeminer_interpolate_message(message, .envir)
  cli::cli_abort(
    message_interpolated,
    class = c(class, "codeminer_error"),
    call = call,
    cli_message = message_interpolated,
    ...,
    .envir = .envir
  )
}

#' @rdname conditions
codeminer_warn <- function(
  message,
  class = NULL,
  ...,
  .envir = rlang::caller_env()
) {
  message_interpolated <- codeminer_interpolate_message(message, .envir)
  cli::cli_warn(
    message_interpolated,
    class = c(class, "codeminer_warning"),
    cli_message = message_interpolated,
    ...,
    .envir = .envir
  )
}

#' @rdname conditions
codeminer_inform <- function(
  message,
  class = NULL,
  ...,
  .envir = rlang::caller_env()
) {
  message_interpolated <- codeminer_interpolate_message(message, .envir)
  cli::cli_inform(
    message_interpolated,
    class = c(class, "codeminer_message"),
    cli_message = message_interpolated,
    ...,
    .envir = .envir
  )
}

# Helper for message interpolation
codeminer_interpolate_message <- function(
  message,
  .envir = rlang::caller_env()
) {
  vapply(
    message,
    cli::format_inline,
    character(1),
    .envir = .envir
  )
}
