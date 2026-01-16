#' Standard codelist column names
#'
#' @return Character vector of standard column names: code, description, code_type
#' @keywords internal
#' @noRd
codelist_cols <- function() {
  c("code", "description", "code_type")
}

#' Lightweight class for codelists
#'
#' @param x A data frame to convert to a codelist
#' @return A `codeminer_codelist` object (tibble with additional class)
#' @keywords internal
#' @noRd
as_codelist <- function(x) {
  stopifnot(is.data.frame(x))

  tb <- tibble::as_tibble(x)
  class(tb) <- c("codeminer_codelist", class(tb))
  tb
}

#' Validate codelist data frame
#'
#' Validates that a data frame has the required structure for a codelist
#' and returns the inferred code_type.
#'
#' @param df A data frame to validate
#' @return The unique code_type value (character scalar)
#' @keywords internal
#' @noRd
validate_codeminer_codelist <- function(df, call = rlang::caller_env()) {
  # Check required columns exist
  required_cols <- codelist_cols()
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    codeminer_abort(
      c(
        "Data frame is missing required columns.",
        "x" = "Missing: {.field {missing_cols}}",
        "i" = "Required columns: {.field {required_cols}}"
      ),
      call = call
    )
  }

  # Check all columns are character type
  col_types <- vapply(df[required_cols], \(x) class(x)[1], character(1))
  non_char <- names(col_types)[col_types != "character"]

  if (length(non_char) > 0) {
    non_char_types <- col_types[non_char]
    codeminer_abort(
      c(
        "All required columns must be character type.",
        "x" = "Column {.field {names(non_char_types)}} has type {.cls {non_char_types}}"
      ),
      call = call
    )
  }

  # Check for missing values
  has_na <- vapply(df[required_cols], anyNA, logical(1))
  cols_with_na <- names(has_na)[has_na]

  if (length(cols_with_na) > 0) {
    codeminer_abort(
      c(
        "Data frame contains missing values.",
        "x" = "Column(s) with NA: {.field {cols_with_na}}"
      ),
      call = call
    )
  }

  # Check code_type column has exactly one unique value
  unique_code_types <- unique(df$code_type)

  if (length(unique_code_types) == 0) {
    codeminer_abort(
      "The {.field code_type} column is empty.",
      call = call
    )
  }

  if (length(unique_code_types) > 1) {
    codeminer_abort(
      c(
        "The {.field code_type} column must contain only one unique value.",
        "x" = "Found {length(unique_code_types)} unique values: {.val {unique_code_types}}"
      ),
      call = call
    )
  }

  # Return the unique code_type
  unique_code_types[1]
}

#' Print method for codeminer_codelist
#'
#' @param x A codeminer_codelist object
#' @param ... Additional arguments passed to print
#' @export
print.codeminer_codelist <- function(x, ...) {
  cli::cli_text("{.cls codeminer_codelist}: {nrow(x)} code{?s}")
  if (nrow(x) > 0 && "code_type" %in% names(x)) {
    code_types <- unique(x$code_type)
    if (length(code_types) == 1 && !is.na(code_types[1])) {
      cli::cli_text("Code type: {.val {code_types[1]}}")
    }
  }
  cli::cli_text("")
  NextMethod()
}
