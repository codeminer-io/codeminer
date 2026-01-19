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

#' Collect and validate codes input from ... argument
#'
#' This helper function collects codes from the `...` argument, validates them,
#' and returns a standardized list containing the codes vector and code_type.
#' Handles empty input, single data frames, character vectors, and || separated
#' strings.
#'
#' @param ... Code inputs - can be character vectors, a single data frame, or
#'   empty
#' @param type Optional code type to validate against
#' @param allow_empty Whether to allow empty input (default TRUE)
#' @param call The calling environment for error messages
#'
#' @return A list with two elements:
#'   - `codes`: Character vector of codes
#'   - `code_type`: String code type (or NULL if not determinable)
#'
#' @keywords internal
collect_codes_input <- function(
  ...,
  type = NULL,
  allow_empty = TRUE,
  call = rlang::caller_env()
) {
  # Collect all inputs using rlang::list2() to support !!! splicing
  args <- rlang::list2(...)

  # Empty input case
  if (length(args) == 0) {
    if (!allow_empty) {
      codeminer_abort("No codes provided", call = call)
    }
    return(list(codes = character(0), code_type = NULL))
  }

  # Check for single data frame input
  if (length(args) == 1 && is.data.frame(args[[1]])) {
    df <- args[[1]]

    # If already a codeminer_codelist, just extract code and code_type
    if (inherits(df, "codeminer_codelist")) {
      df_code_type <- unique(df$code_type)
      if (length(df_code_type) != 1) {
        codeminer_abort(
          "Data frame has multiple code types: {.val {df_code_type}}",
          call = call
        )
      }
    } else {
      # Validate it has required columns
      if (!all(c("code", "code_type") %in% names(df))) {
        missing_cols <- setdiff(c("code", "code_type"), names(df))
        codeminer_abort(
          c(
            "Data frame must have {.field code} and {.field code_type} columns.",
            "x" = "Missing: {.field {missing_cols}}"
          ),
          call = call
        )
      }

      df_code_type <- unique(df$code_type)
      if (length(df_code_type) != 1) {
        codeminer_abort(
          "Data frame has multiple code types: {.val {df_code_type}}",
          call = call
        )
      }
    }

    # Handle type matching logic
    type_missing <- is.null(type) || identical(type, "")

    if (!type_missing && df_code_type != type) {
      codeminer_abort(
        c(
          "Conflicting {.arg type} values.",
          "x" = "Data frame has: {.val {df_code_type}}",
          "x" = "Argument specifies: {.val {type}}",
          "i" = "Both must match, or omit the {.arg type} argument to use the data frame value."
        ),
        call = call
      )
    }

    # Extract codes and return
    codes_vec <- df$code
    return(list(codes = codes_vec, code_type = df_code_type))
  }

  # Validate all inputs are character (checks container type, not each element)
  is_char <- vapply(args, is.character, logical(1))

  if (!all(is_char)) {
    not_char <- which(!is_char)
    not_char_classes <- vapply(
      args[not_char],
      \(x) paste(class(x), collapse = "/"),
      character(1)
    )

    arg_labels <- if (length(not_char) == 1) {
      paste0("Argument ", not_char)
    } else {
      paste0("Arguments ", paste(not_char, collapse = ", "))
    }

    codeminer_abort(
      c(
        "All inputs must be character vectors or a single data frame.",
        "x" = "{arg_labels} {?has/have} class: {.cls {not_char_classes}}"
      ),
      call = call
    )
  }

  # Flatten all character inputs into single vector
  codes_raw <- unlist(args, use.names = FALSE)

  # Parse codes (handles || splitting and << >> comments)
  parsed <- parse_codes(codes_raw)
  codes_vec <- parsed$code

  return(list(codes = codes_vec, code_type = NULL))
}

#' Parse codes - split on || and extract << >> comments
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
  # Handle list input (from rlang::list2(...))
  if (is.list(codes) && !is.data.frame(codes)) {
    # Empty list case
    if (length(codes) == 0) {
      return(list(
        codes = character(0),
        code_type = code_type
      ))
    }

    # Single data frame in list
    if (length(codes) == 1 && is.data.frame(codes[[1]])) {
      codes <- codes[[1]] # Unwrap and continue to data frame handling below
    } else {
      # Validate all elements are character
      is_char <- vapply(codes, is.character, logical(1))
      if (!all(is_char)) {
        not_char <- which(!is_char)
        not_char_classes <- vapply(
          codes[not_char],
          function(x) paste(class(x), collapse = "/"),
          character(1)
        )

        arg_labels <- if (length(not_char) == 1) {
          paste0("Argument ", not_char)
        } else {
          paste0("Arguments ", paste(not_char, collapse = ", "))
        }

        codeminer_abort(
          c(
            "All inputs must be character vectors or a single data frame.",
            "x" = "{arg_labels} {?has/have} class: {.cls {not_char_classes}}"
          ),
          call = call
        )
      }

      # Flatten list to character vector
      codes <- unlist(codes, use.names = FALSE)
      # Continue to character handling below
    }
  }

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
