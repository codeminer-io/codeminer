#' Look up descriptions for clinical codes
#'
#' Returns a codelist with descriptions for the codes of interest.
#' Supports flexible input: character vectors, `||` separated strings, or data frames.
#'
#' @param codes Character vector, `||` separated string, or data frame with code/description/code_type columns.
#'   Special values: `"all"` returns all codes; empty input returns empty codelist.
#'   Comments can be added with `<< >>` syntax: `"E10 << Type 1 diabetes >>"`.
#' @param code_type character. Type of clinical code system to be searched.
#'   Optional if `codes` is a data frame with code_type column.
#'   Depends on what is available in the lookup tables. See [add_lookup_table()]
#'   on how to add new lookup tables. This can also be configured through the `codeminer.code_type` option.
#' @param lookup_version character. Version of the lookup table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.lookup_version` option.
#' @param preferred_description_only logical. If `TRUE`, only returns the preferred description for each code.
#'   Default: `FALSE`.
#'
#' @return A `codeminer_codelist` object (tibble) containing the codes and their descriptions
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_lookup_table()] for adding new lookup tables to the database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # Character vector
#' CODES(c("E10", "E11"), code_type = "ICD-10")
#'
#' # With comments
#' CODES("E10 << Type 1 diabetes >>", code_type = "ICD-10")
#'
#' # || separated string
#' CODES("E10 || E11", code_type = "ICD-10")
#'
#' # Data frame input
#' df <- data.frame(
#'   code = c("E10", "E11"),
#'   description = c("Type 1", "Type 2"),
#'   code_type = c("ICD-10", "ICD-10")
#' )
#' CODES(df)
CODES <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  # Validate logical parameter
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Handle special case: "all"
  if (is.character(codes) && length(codes) == 1 && identical(codes, "all")) {
    check_code_type(code_type)
    check_version(lookup_version)

    con <- connect_to_db()
    check_database(con)
    lookup_table <- get_lookup_table(con, code_type, lookup_version)
    result <- dplyr::collect(lookup_table)

    if (preferred_description_only) {
      result <- dplyr::filter(result, .data$preferred_description)
    }

    # Select only standard columns
    result <- dplyr::select(result, dplyr::all_of(codelist_cols()))

    return(as_codelist(result))
  }

  # Prepare input using helper (handles character/||/data frame)
  prepared <- prepare_codes_input(codes, code_type, arg_name = "codes")
  codes_vec <- prepared$codes
  code_type <- prepared$code_type

  # Empty input case
  if (length(codes_vec) == 0) {
    empty_cols <- setNames(
      replicate(3, character(), simplify = FALSE),
      codelist_cols()
    )
    return(as_codelist(tibble::as_tibble(empty_cols)))
  }

  # Validate remaining parameters
  check_code_type(code_type)
  check_version(lookup_version)

  con <- connect_to_db()
  check_database(con)

  lookup_table <- get_lookup_table(con, code_type, lookup_version)

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% .env$codes_vec) |>
    dplyr::collect()

  missing_codes <- setdiff(codes_vec, result[["code"]])
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "lookup",
      table_meta = get_metadata_for_table(con, code_type, lookup_version)
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
#' @details
#' `CODES_LIKE` searches for codes that match a given regular expression.
#' The matching is case-insensitive.
#'
#' @export
#' @rdname CODES
#' @examples
#' CODES_LIKE("^E1", code_type = "ICD-10")
CODES_LIKE <- function(
  pattern,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  check_pattern(pattern)
  check_code_type(code_type)
  check_version(lookup_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- connect_to_db()
  check_database(con)

  lookup_table <- get_lookup_table(con, code_type, lookup_version)
  like_codes <- dplyr::filter(
    lookup_table,
    stringr::str_detect(.data$code, pattern)
  ) |>
    dplyr::pull("code")

  result <- CODES(
    unique(like_codes),
    code_type = code_type,
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
  this_meta <- get_metadata_for_table(con, code_type, lookup_version, call)

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

get_metadata_for_table <- function(
  con,
  code_type,
  lookup_version,
  call = rlang::caller_env()
) {
  meta <- get_lookup_metadata(con = con)
  if (!(code_type %in% meta$code_type)) {
    codeminer_abort(
      c(
        "Code type '{code_type}' not found in lookup metadata.",
        "i" = "Did you add the lookup table with {.fun codeminer::add_lookup_table}?"
      ),
      call = call
    )
  }

  all_version_meta <- dplyr::filter(meta, .data$code_type == .env$code_type)

  if (lookup_version == "latest") {
    lookup_version <- get_latest_version(all_version_meta$lookup_version)
  }
  this_meta <- dplyr::filter(
    all_version_meta,
    .data$lookup_version == .env$lookup_version
  )

  if (nrow(this_meta) == 0) {
    codeminer_abort(
      "No lookup metadata found for '{code_type}' version '{lookup_version}'",
      call = call
    )
  }
  stopifnot(nrow(this_meta) == 1) # code_type + version combo should be unique

  return(this_meta)
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
