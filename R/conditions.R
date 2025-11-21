#' CodeMiner error constructor
#'
#' Internal wrapper around [cli::cli_abort()] that creates a structured CodeMiner
#' error condition. A base class `codeminer_error` is always appended to the
#' condition class vector, and additional custom classes may be optionally
#' prepended via the `class` argument. The original named `cli` message vector
#' supplied in `message` is stored in the `cli_error_message` field of the
#' condition
#'
#' The original named `cli` message vector is stored in `cli_error_message` for
#' use by the CodeMiner API.
#'
#' @inheritParams cli::cli_abort
#' @param class Optional character vector of additional classes to prepend
#'   before `codeminer_error`.
#' @param ... Passed through to [cli::cli_abort()].
#' @param call Call environment.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Capture a CodeMiner error condition and inspect it
#' e <- tryCatch(
#'   codeminer_abort(
#'     c(
#'       x = "Code type 'icd1' not found.",
#'       i = "Use add_lookup_table()."
#'     ),
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
#' }
codeminer_abort <- function(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env()
) {
  cli::cli_abort(
    message,
    class = c(class, "codeminer_error"),
    call = call,
    cli_error_message = message,
    ...
  )
}
