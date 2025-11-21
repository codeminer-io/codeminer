#' CodeMiner error constructor *(internal helper)*
#'
#' *For internal package used only.* A wrapper around [cli::cli_abort()] that
#' creates a structured CodeMiner error condition.
#'
#' A base class `codeminer_error` is always appended to the condition class
#' vector, and additional custom classes may be optionally prepended via the
#' `class` argument.
#'
#' The original named `cli` message vector is stored in `cli_error_message` for
#' use by the CodeMiner API (see examples).
#'
#' @inheritParams cli::cli_abort
#' @param class Optional character vector of additional classes to prepend
#'   before `codeminer_error`.
#' @param ... Passed through to [cli::cli_abort()].
#' @param call Call environment.
#'
#' @keywords internal
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
#'   error = function(e) e
#' )
#'
#' # Inspect condition class hierarchy
#' class(e)
#'
#' # Inspect stored structured `cli` message
#' e$cli_error_message
#'
#' # Inspect the default formatted condition message
#' conditionMessage(e)
codeminer_abort <- function(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  message_interpolated <- vapply(
    message,
    cli::format_inline,
    character(1),
    .envir = .envir
  )

  cli::cli_abort(
    message_interpolated,
    class = c(class, "codeminer_error"),
    call = call,
    cli_error_message = message_interpolated,
    ...,
    .envir = .envir
  )
}
