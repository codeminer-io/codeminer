#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup. If passing `"all"`, returns all codes.
#' @param code_type character. Type of clinical code system to be searched.
#'   Depends on what is available in the lookup tables. See [add_lookup_table()]
#'   on how to add new lookup tables. This can also be configured through the `codeminer.code_type` option.
#' @param lookup_version character. Version of the lookup table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.lookup_version` option.
#' @param preferred_description_only logical. If `TRUE`, only returns the preferred description for each code.
#'   Default: `FALSE`.
#'
#' @return A `data.frame` containing the codes and their descriptions
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_lookup_table()] for adding new lookup tables to the database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # look up ICD10 codes
#' CODES(
#'   codes = c("E10", "E11"),
#'   code_type = "icd10"
#' )
CODES <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  check_lookup_args(codes, code_type, lookup_version)

  con <- connect_to_db()
  check_database(con)

  lookup_table <- get_lookup_table(con, code_type, lookup_version)

  if (identical(codes, "all")) {
    return(dplyr::collect(lookup_table))
  }

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% .env$codes) |>
    dplyr::collect()

  missing_codes <- setdiff(codes, result[["code"]])
  if (length(missing_codes) > 0) {
    codeminer_warn(
      c(
        "!" = "The following codes were not found in the lookup table: {.code {missing_codes}}"
      )
    )
  }

  if (preferred_description_only) {
    result <- dplyr::filter(result, .data$preferred_description)
  }

  return(result)
}

# Argument validation helpers
check_lookup_args <- function(
  codes,
  code_type,
  lookup_version,
  call = rlang::caller_env()
) {
  check_codes(codes, call)
  check_code_type(code_type, call)
  check_version(lookup_version, call)
}

check_codes <- function(codes, call = rlang::caller_env()) {
  if (!rlang::is_character(codes)) {
    codeminer_abort(
      "{.arg codes} must be a character vector, not {typeof(codes)}",
      call = call
    )
  }
}

check_code_type <- function(code_type, call = rlang::caller_env()) {
  if (!rlang::is_string(code_type)) {
    codeminer_abort(
      "{.arg code_type} must be a string, not {typeof(code_type)} with length {length(code_type)}",
      call = call
    )
  }
}

check_version <- function(version, call = rlang::caller_env()) {
  version_expr <- rlang::enquo(version)
  version_name <- rlang::as_label(version_expr)

  if (length(version) != 1) {
    codeminer_abort(
      "{.arg {version_name}} must have length 1, not {length(version)}",
      ,
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
#' CODES_LIKE("^E1", code_type = "icd10")
CODES_LIKE <- function(
  pattern,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  check_lookup_args(pattern, code_type, lookup_version)

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
