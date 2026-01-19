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

#' Warn about missing codes in a table
#'
#' Emits a truncated warning listing codes that were not found in a given table.
#' The warning carries the missing codes and table context as structured data
#' and can be handled programmatically.
#'
#' ## Handling missing-code warnings
#'
#' This function signals a warning of class `"codeminer_missing_codes"`. The
#' warning object contains the following fields:
#'
#' - `missing_codes`: a character vector of all missing codes (untruncated)
#' - `table_type`: the type of table the codes were expected to be found in
#' (e.g. `"lookup"`, `"mapping"`, `"relationship"`)
#' - `table_meta`: metadata for the table that was queried (including table
#' name, version, code type, and source)
#'
#' You can intercept and act on these warnings using [withCallingHandlers()]
#' (see examples). This allows you to collect missing inputs without printing a
#' warning, while still preserving full context about where the lookup failed.
#'
#' @param missing_codes Character vector of all missing codes (untruncated).
#' @param table_type Type of table the codes were expected to be found in. Must
#'   be one of 'relationship', 'lookup', or 'mapping'.
#' @param table_meta Data frame of metadata for the table the codes were
#'   expected to be found in.
#' @param max_show Maximum number of missing codes to display. Defaults to 10.
#'
#' @return Invisibly returns `missing_codes`.
#'
#' @keywords internal
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#' missing_codes <- table_type <- table_meta <- NULL
#'
#' # Capture missing codes from waning using `withCallingHandlers()`
#' withCallingHandlers(
#'   {
#'     codes <- CODES(c("foo", "bar", "E10"), type = "ICD-10")
#'   },
#'   codeminer_missing_codes = function(w) {
#'     missing_codes <<- w$missing_codes
#'     table_type <<- w$table_type
#'     table_meta <<- w$table_meta
#'     invokeRestart("muffleWarning")
#'   }
#' )
#'
#' # Recognised codes
#' codes
#'
#' # Unrecognised codes and related context
#' missing_codes
#' table_type
#' table_meta
missing_codes_warning <- function(
  missing_codes,
  table_meta,
  table_type = c("lookup", "mapping", "relationship"),
  max_show = 10
) {
  if (length(missing_codes) == 0) {
    return(invisible(missing_codes))
  }

  table_type <- rlang::arg_match(
    table_type,
    names(codeminer_metadata_table_names)
  )

  missing_codes <- sort(unique(missing_codes))

  shown <- utils::head(missing_codes, max_show)
  n_extra <- length(missing_codes) - length(shown)

  msg <- if (n_extra > 0) {
    c(
      "!" = "The following codes were not found in the {table_type} table:",
      "*" = "{.code {shown}}",
      "i" = "{n_extra} more not shown"
    )
  } else {
    c(
      "!" = "The following codes were not found in the {table_type} table:",
      "*" = "{.code {shown}}"
    )
  }

  codeminer_warn(
    msg,
    class = "codeminer_missing_codes",
    missing_codes = missing_codes,
    table_type = table_type,
    table_meta = table_meta
  )

  invisible(missing_codes)
}
