#' Look up descriptions for clinical codes
#'
#' Returns a codelist with descriptions for the codes of interest. Supports
#' flexible input: character vectors, `||` separated strings, or data frames.
#'
#' @param ... Codes to look up. Can be:
#'   - Character vectors: `CODES("E10", "E11", type = "ICD-10")`
#'   - `||` separated strings: `CODES("E10 || E11", type = "ICD-10")`
#'   - Data frame with code/description/code_type columns: `CODES(my_df)`
#'   - Mixed: `CODES("E10", my_vector, "E13 || E14", type = "ICD-10")`
#'
#'   Special values: `"all"` returns all codes; empty input returns empty
#'   codelist.
#'
#'   Comments can be added with `<< >>` syntax: `"E10 << Type 1 diabetes >>"`.
#' @param type character. Type of clinical code system to be searched. Optional
#'   if input is a data frame with code_type column. Depends on what is
#'   available in the lookup tables. See [add_lookup_table()] on how to add new
#'   lookup tables. This can also be configured through the
#'   `codeminer.code_type` option.
#' @param lookup_version character. Version of the lookup table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.lookup_version`
#'   option.
#' @param preferred_description_only logical. If `TRUE`, only returns the
#'   preferred description for each code. Default: `FALSE`.
#'
#' @return A `codeminer_codelist` object (tibble) containing the codes and their
#'   descriptions
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_lookup_table()] for adding new lookup tables to the database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # Multiple arguments
#' CODES("E10", "E11", type = "ICD-10")
#'
#' # With comments
#' CODES("E10 << Type 1 diabetes >>", type = "ICD-10")
#'
#' # || separated string
#' CODES("E10 || E11", type = "ICD-10")
#'
#' # Splice operator
#' my_codes <- c("E10", "E11")
#' CODES(!!!my_codes, type = "ICD-10")
#'
#' # Data frame input
#' df <- data.frame(
#'   code = c("E10", "E11"),
#'   description = c("Type 1", "Type 2"),
#'   code_type = c("ICD-10", "ICD-10")
#' )
#' CODES(df)
CODES <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  # Validate logical parameter
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Special handling: if single data frame input that's already a codelist, return as-is
  args <- rlang::list2(...)
  if (
    length(args) == 1 &&
      is.data.frame(args[[1]]) &&
      inherits(args[[1]], "codeminer_codelist")
  ) {
    df <- args[[1]]

    # Validate type matches if provided
    df_code_type <- unique(df$code_type)
    type_missing <- is.null(type) || identical(type, "")

    if (!type_missing && !is.na(df_code_type) && df_code_type != type) {
      codeminer_abort(
        c(
          "Conflicting {.arg type} values.",
          "x" = "Data frame has: {.val {df_code_type}}",
          "x" = "Argument specifies: {.val {type}}",
          "i" = "Both must match, or omit the {.arg type} argument to use the data frame value."
        ),
        call = rlang::current_env()
      )
    }

    return(df)
  }

  # Use helper to collect and validate all other inputs
  collected <- collect_codes_input(
    ...,
    type = type,
    call = rlang::current_env()
  )
  codes_vec <- collected$codes

  # Use codelist code_type if provided
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  # Empty after parsing
  if (length(codes_vec) == 0) {
    empty_cols <- stats::setNames(
      replicate(3, character(), simplify = FALSE),
      codelist_cols()
    )
    return(as_codelist(tibble::as_tibble(empty_cols)))
  }

  # Handle special case: "all"
  if (length(codes_vec) == 1 && identical(codes_vec, "all")) {
    check_code_type(type)
    check_version(lookup_version)

    con <- get_db_con()
    lookup_table <- get_lookup_table(con, type, lookup_version)
    result <- dplyr::collect(lookup_table)

    if (preferred_description_only) {
      result <- dplyr::filter(result, .data$preferred_description)
    }

    # Select only standard columns
    result <- dplyr::select(result, dplyr::all_of(codelist_cols()))

    return(as_codelist(result))
  }

  # Validate type parameter
  check_code_type(type)
  check_version(lookup_version)

  con <- get_db_con()

  lookup_table <- get_lookup_table(con, type, lookup_version)

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% .env$codes_vec) |>
    dplyr::collect()

  missing_codes <- setdiff(codes_vec, result[["code"]])
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "lookup",
      table_meta = get_metadata_for_lookup(con, type, lookup_version)
    )
  }

  if (preferred_description_only) {
    result <- dplyr::filter(result, .data$preferred_description)
  }

  # Select only standard columns
  result <- dplyr::select(result, dplyr::all_of(codelist_cols()))

  return(as_codelist(result))
}

# Argument validation helpers
check_code_type <- function(code_type, call = rlang::caller_env()) {
  code_type_expr <- rlang::enquo(code_type)

  # nolint next: object_usage_linter.
  code_type_name <- rlang::as_label(code_type_expr)

  if (is.null(code_type)) {
    codeminer_abort(
      c(
        "{.arg {code_type_name}} is required but not provided.",
        "i" = "Either set the option: {.code options(codeminer.code_type = \"ICD-10\")}",
        "i" = "Or provide {.arg {code_type_name}} explicitly in your function call."
      ),
      call = call
    )
  }

  if (!rlang::is_string(code_type)) {
    codeminer_abort(
      "{.arg {code_type_name}} must be a string, not {typeof(code_type)} with length {length(code_type)}",
      call = call
    )
  }
}

check_version <- function(version, call = rlang::caller_env()) {
  version_expr <- rlang::enquo(version)

  # nolint next: object_usage_linter.
  version_name <- rlang::as_label(version_expr)

  if (length(version) != 1) {
    codeminer_abort(
      c(
        "x" = "{.arg {version_name}} must have length 1, not {length(version)}"
      ),
      ,
      call = call
    )
  }
}

check_logical_scalar <- function(arg, arg_name, call = rlang::caller_env()) {
  if (!rlang::is_scalar_logical(arg)) {
    codeminer_abort(
      "{.arg {arg_name}} must be TRUE or FALSE, not {typeof(arg)} with length {length(arg)}",
      call = call
    )
  }
}

check_depth <- function(depth, call = rlang::caller_env()) {
  if (!rlang::is_scalar_integerish(depth) && !identical(depth, Inf)) {
    codeminer_abort(
      "{.arg depth} must be a positive integer or Inf, not {typeof(depth)}",
      call = call
    )
  }
  if (is.finite(depth) && depth < 1) {
    codeminer_abort(
      "{.arg depth} must be at least 1, not {depth}",
      call = call
    )
  }
}

check_relationship_types <- function(
  relationship_types,
  call = rlang::caller_env()
) {
  if (
    !is.null(relationship_types) && !rlang::is_character(relationship_types)
  ) {
    codeminer_abort(
      "{.arg relationship_types} must be NULL or a character vector, not {typeof(relationship_types)}",
      call = call
    )
  }
}

check_pattern <- function(pattern, call = rlang::caller_env()) {
  if (!rlang::is_string(pattern)) {
    codeminer_abort(
      "{.arg pattern} must be a length 1 string, not {typeof(pattern)} with length {length(pattern)}",
      call = call
    )
  }
}

#' @param pattern a regular expression to search for
#'
#' @details `CODES_LIKE` searches for codes that match a given regular
#' expression. The matching is case-insensitive.
#'
#' @export
#' @rdname CODES
#' @examples
#' CODES_LIKE("^E1", type = "ICD-10")
CODES_LIKE <- function(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  check_pattern(pattern)
  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()

  lookup_table <- get_lookup_table(con, type, lookup_version)
  like_codes <- dplyr::filter(
    lookup_table,
    stringr::str_detect(.data$code, pattern)
  ) |>
    dplyr::pull("code")

  result <- CODES(
    unique(like_codes),
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )
  return(result)
}

#' Get the lookup table for the given code type in standardised format
#'
#' @param con A database connection.
#' @param code_type The code type for which to retrieve the lookup table.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A data frame containing the lookup table with three columns:
#'   `code`, `description` and `code_type`.
#' @keywords internal
get_lookup_table <- function(
  con,
  code_type,
  lookup_version,
  call = rlang::caller_env()
) {
  this_meta <- get_metadata_for_lookup(con, code_type, lookup_version, call)

  tbl_name <- this_meta$lookup_table_name
  tbl <- dplyr::tbl(con, tbl_name)

  tbl <- dplyr::select(
    tbl,
    code = .env$this_meta$lookup_code_col,
    description = .env$this_meta$lookup_description_col,
    dplyr::everything()
  ) |>
    dplyr::mutate(code_type = .env$code_type)

  if (!is.na(this_meta$preferred_description_col)) {
    tbl <- dplyr::rename(
      tbl,
      preferred_description = .env$this_meta$preferred_description_col,
    )
  } else {
    tbl <- dplyr::mutate(tbl, preferred_description = TRUE)
  }

  return(tbl)
}

get_metadata_for_lookup <- function(
  con,
  code_type,
  lookup_version,
  call = rlang::caller_env()
) {
  meta <- get_lookup_metadata(con = con)
  resolve_versioned_metadata(
    meta,
    code_type_val = code_type,
    version_val = lookup_version,
    version_col = "lookup_version",
    pin_type = "lookup",
    type_label = "lookup",
    add_fun_name = "codeminer::add_lookup_table",
    call = call
  )
}

# NOTE: this helper has a strong assumption that the versions have some numeric component (e.g. "v42")
# that can be extracted and used to sort, in order to get the latest one
# This is is not something we enforce, so this may not always return the correct result.
get_latest_version <- function(versions) {
  versions_numeric <- as.numeric(stringr::str_extract(versions, "\\d+"))
  if (any(is.na(versions_numeric))) {
    # resort to alphabetic ordering
    latest_version <- max(versions)
  } else {
    latest_version <- versions[which.max(versions_numeric)]
  }
  codeminer_inform(c("i" = "Using '{latest_version}' as latest version"))
  return(latest_version)
}
