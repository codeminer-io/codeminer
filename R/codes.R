#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup. If passing `"all"`, returns all codes.
#' @param code_type character. Type of clinical code system to be searched.
#'   Depends on what is available in the lookup tables. See [add_lookup_table()]
#'   on how to add new lookup tables. This can also be configured through the `codeminer.code_type` option.
#'
#' @return A `data.frame` containing the codes and their descriptions
#' @export
#' @family Clinical code lookups and mappings
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
  version = "latest"
) {
  check_lookup_args(code_type, version)

  con <- connect_to_db()
  check_database(con)

  lookup_table <- get_lookup_table(con, code_type, version)

  if (length(codes) == 1 && codes == "all") {
    return(lookup_table)
  }

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% !!codes) |>
    dplyr::collect()

  missing_codes <- setdiff(codes, result[["code"]])
  if (length(missing_codes) > 0) {
    cli::cli_warn(
      "The following codes were not found in the lookup table: {.code {missing_codes}}"
    )
  }

  return(result)
}

# Argument validation helpers
check_lookup_args <- function(
  code_type,
  version,
  call = rlang::caller_env()
) {
  check_code_type(code_type, call)
  check_version(version, call)
}

check_code_type <- function(code_type, call = rlang::caller_env()) {
  if (length(code_type) != 1) {
    cli::cli_abort(
      "{.arg code_type} must have length 1, not {length(code_type)}",
      call = call
    )
  }
}

check_version <- function(version, call = rlang::caller_env()) {
  if (length(version) != 1) {
    cli::cli_abort(
      "{.arg version} must have length 1, not {length(version)}",
      call = call
    )
  }
}

#' Get the lookup table for the given code type in standardised format
#'
#' @param con A database connection.
#' @param code_type The code type for which to retrieve the lookup table.
#' @param call The calling environment. Passed to [cli::cli_abort].
#'
#' @return A data frame containing the lookup table with three columns:
#'   `code`, `description` and `code_type`.
#' @keywords internal
get_lookup_table <- function(
  con,
  code_type,
  version,
  call = rlang::caller_env()
) {
  this_meta <- get_meta_for_table(con, code_type, version, call)

  tbl_name <- this_meta$lookup_table_name
  tbl <- get_table_from_db(con, tbl_name)

  tbl <- dplyr::select(
    tbl,
    code = this_meta$lookup_code_col,
    description = this_meta$lookup_description_col
  ) |>
    dplyr::mutate(code_type = code_type)

  return(tbl)
}

get_meta_for_table <- function(
  con,
  type,
  version,
  call = rlang::caller_env()
) {
  meta <- get_lookup_metadata(con = con)
  if (!(type %in% meta$code_type)) {
    cli::cli_abort(
      c(
        "Code type '{type}' not found in lookup metadata.",
        "i" = "Did you add the lookup table with {.fun codeminer::add_lookup_table}?"
      ),
      call = call
    )
  }

  all_version_meta <- dplyr::filter(meta, .data$code_type == type)

  if (version == "latest") {
    version <- get_latest_version(all_version_meta$lookup_version)
  }
  this_meta <- dplyr::filter(all_version_meta, .data$lookup_version == version)

  if (nrow(this_meta) == 0) {
    cli::cli_abort("No metadata found for '{type}' version '{version}'")
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
  cli::cli_alert_info("Using '{latest_version}' as latest version")
  return(latest_version)
}
