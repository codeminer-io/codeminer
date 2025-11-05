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
  version = "v0"
) {
  check_codes(codes)
  check_code_type(code_type)

  con <- connect_to_db()

  lookup_table <- get_lookup_table(con, code_type, version)

  if (length(codes) == 1 && codes == "all") {
    return(lookup_table)
  }

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% codes) |>
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
check_codes <- function(codes) {
  if (!is.character(codes)) {
    cli::cli_abort(
      "{.arg codes} must be a character vector, not {typeof(codes)}"
    )
  }
}

check_code_type <- function(code_type) {
  if (!is.character(code_type)) {
    cli::cli_abort(
      "{.arg code_type} must be of type character, not {typeof(code_type)}"
    )
  }
  if (length(code_type) != 1) {
    cli::cli_abort(
      "{.arg code_type} must have length 1, not {length(code_type)}"
    )
  }
}

#' Get the lookup table for the given code type in standardised format
#'
#' @param con A database connection.
#' @param code_type The code type for which to retrieve the lookup table.
#' @param call The calling environment. Passed to [cli::cli_abort].
#'
#' @return A data frame containing the lookup table with two columns: `code` and `description`.
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
  code_type,
  version,
  call = rlang::caller_env()
) {
  meta <- get_lookup_metadata(con = con)
  if (!(code_type %in% meta$code_type)) {
    cli::cli_abort(
      c(
        "Code type '{code_type}' not found in lookup metadata.",
        "i" = "Did you add the lookup table with {.fun codeminer::add_lookup_table}?"
      ),
      call = call
    )
  }

  # Need to use `.env` pronoun here to avoid confusing dplyr::filter
  # see https://rlang.r-lib.org/reference/dot-data.html
  this_meta <- dplyr::filter(meta, .data$code_type == .env$code_type)
  available_versions <- this_meta$lookup_version

  if (!(version %in% available_versions)) {
    cli::cli_abort(c(
      "No metadata found for '{code_type}' version '{version}'",
      "i" = "Available versions for '{code_type}': {available_versions}"
    ))
  }

  this_meta <- dplyr::filter(this_meta, .data[["lookup_version"]] == version)
  stopifnot(nrow(this_meta) == 1) # code_type + version combo should be unique

  return(this_meta)
}
