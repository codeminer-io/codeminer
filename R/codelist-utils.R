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
        # nolint next: object_usage_linter.
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
    # nolint next: object_usage_linter.
    not_char_classes <- vapply(
      args[not_char],
      \(x) paste(class(x), collapse = "/"),
      character(1)
    )

    # nolint next: object_usage_linter.
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

#' Strip `<<>>` comments from code strings
#'
#' Extracts just the code portion from strings that may contain `<< description >>`
#' comments.
#'
#' @param x Character vector of codes, possibly with `<< >>` comments.
#' @return Character vector with comments stripped.
#' @keywords internal
#' @noRd
strip_comments <- function(x) {
  stringr::str_remove(x, "\\s*<<.*?>>$") |>
    stringr::str_trim()
}

#' Resolve relationship_types input to a character vector
#'
#' Normalises `relationship_types` from various input types (NULL, character,
#' data frame, codelist) to a character vector suitable for database filtering.
#' Replaces the previous `check_relationship_types()` + `strip_comments()`
#' two-step.
#'
#' @param relationship_types Relationship types input. Can be NULL, a character
#'   vector, a `codeminer_codelist`, or a data frame with a `code` column.
#' @param expected_type Optional expected code type. If the input is a data
#'   frame with a `code_type` column, its value is validated against this.
#' @param call The calling environment for error messages.
#' @return NULL (if input is NULL) or a character vector of relationship types.
#' @keywords internal
#' @noRd
resolve_relationship_types <- function(
  relationship_types,
  expected_type = NULL,
  call = rlang::caller_env()
) {
  if (is.null(relationship_types)) {
    return(NULL)
  }

  # Extract codes from codelist/data.frame
  if (is.data.frame(relationship_types)) {
    if (!"code" %in% names(relationship_types)) {
      # nolint next: object_usage_linter.
      available_cols <- names(relationship_types)
      codeminer_abort(
        c(
          "{.arg relationship_types} data frame must have a {.field code} column.",
          "i" = "Available columns: {.field {available_cols}}"
        ),
        call = call
      )
    }

    # Validate code_type matches if both are available
    if (
      !is.null(expected_type) &&
        "code_type" %in% names(relationship_types)
    ) {
      df_type <- unique(relationship_types$code_type)
      df_type <- df_type[!is.na(df_type)]
      if (length(df_type) == 1 && df_type != expected_type) {
        codeminer_abort(
          c(
            "Conflicting code types in {.arg relationship_types}.",
            "x" = "Data frame has {.field code_type}: {.val {df_type}}",
            "x" = "Expected: {.val {expected_type}}",
            "i" = "Relationship types should come from the same coding system as the codes being queried."
          ),
          call = call
        )
      }
    }

    relationship_types <- relationship_types$code
  }

  if (!is.character(relationship_types)) {
    # nolint next: object_usage_linter.
    input_class <- class(relationship_types)
    codeminer_abort(
      c(
        "{.arg relationship_types} must be NULL, a character vector, or a data frame with a {.field code} column.",
        "x" = "Got {.cls {input_class}}."
      ),
      call = call
    )
  }

  strip_comments(relationship_types)
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

#' Strip `<<>>` comments from code strings
#'
#' Extracts just the code portion from strings that may contain `<< description >>`
#' comments. Uses the same regex pattern as [parse_codes()].
#'
#' @param x Character vector of codes, possibly with `<< >>` comments.
#' @return Character vector with comments stripped.
#' @keywords internal
#' @noRd
strip_comments <- function(x) {
  stringr::str_remove(x, "\\s*<<.*?>>$") |>
    stringr::str_trim()
}
