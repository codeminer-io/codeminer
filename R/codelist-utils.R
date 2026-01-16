#' Split on || - handles single strings with multiple codes
#'
#' @param x A character string to split
#' @return Character vector of split codes
#' @keywords internal
#' @noRd
split_double_pipe <- function(x) {
  stopifnot(length(x) == 1, is.character(x))

  x |>
    stringr::str_split_1("\\|\\|") |>
    stringr::str_remove_all("\\n") |>
    stringr::str_trim() |>
    (\(x) x[nzchar(x)])()
}

#' Parse character inputs: extract code and optional << comment >>
#'
#' @param codes_vec Character vector of codes, possibly with << >> comments
#' @return A codeminer_codelist tibble with code, description, code_type columns
#' @keywords internal
#' @noRd
parse_codes <- function(codes_vec) {
  # First, expand any strings that contain ||
  codes_expanded <- codes_vec |>
    purrr::map(\(x) {
      if (stringr::str_detect(x, "\\|\\|")) {
        split_double_pipe(x)
      } else {
        x
      }
    }) |>
    purrr::flatten_chr()

  codes_expanded <- codes_expanded |>
    stringr::str_trim() |>
    (\(x) x[nzchar(x)])()

  if (length(codes_expanded) == 0) {
    return(as_codelist(tibble::tibble(
      code = character(),
      description = character(),
      code_type = character()
    )))
  }

  # Extract: code << optional comment >>
  parsed <- codes_expanded |>
    purrr::map(\(x) {
      m <- stringr::str_match(x, "^([^< ]+)\\s*(?:<<\\s*(.*?)\\s*>>)?$")
      tibble::tibble(
        code = m[, 2],
        description = if (!is.na(m[, 3])) m[, 3] else NA_character_,
        code_type = NA_character_
      )
    }) |>
    dplyr::bind_rows()

  as_codelist(parsed)
}

#' Prepare codes input - main helper for flexible input handling
#'
#' Handles character vectors, || separated strings, and codelist data frames.
#' Validates code_type matching when both input and parameter are provided.
#'
#' @param codes Input codes (character vector, || string, or codeminer_codelist)
#' @param code_type Optional code_type parameter
#' @param arg_name Name of the argument for error messages
#' @param call Calling environment for error reporting
#' @return List with $codes (character vector) and $code_type (string or NULL)
#' @keywords internal
#' @noRd
prepare_codes_input <- function(
  codes,
  code_type = NULL,
  arg_name = "codes",
  call = rlang::caller_env()
) {
  # Handle data frame input
  if (is.data.frame(codes)) {
    # Validate and get the code_type from df
    df_code_type <- validate_codeminer_codelist(codes, call = call)

    # Handle code_type matching logic
    code_type_missing <- is.null(code_type) || identical(code_type, "")

    if (!code_type_missing && df_code_type != code_type) {
      # Mismatch between supplied arg and df
      codeminer_abort(
        c(
          "Conflicting {.arg code_type} values.",
          "x" = "Data frame has: {.val {df_code_type}}",
          "x" = "Argument specifies: {.val {code_type}}",
          "i" = "Both must match, or omit the {.arg code_type} argument to use the data frame value."
        ),
        call = call
      )
    }

    # Return codes and code_type
    return(list(
      codes = codes$code,
      code_type = df_code_type
    ))
  }

  # Handle character input
  if (!is.character(codes)) {
    codeminer_abort(
      c(
        "{.arg {arg_name}} must be a character vector or data frame.",
        "x" = "Got {.cls {class(codes)}}"
      ),
      call = call
    )
  }

  # Parse character codes (handles || and << >>)
  parsed <- parse_codes(codes)

  # Return character vector of codes and code_type
  list(
    codes = parsed$code,
    code_type = code_type
  )
}
