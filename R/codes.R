#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup. If passing `"all"`, returns all codes.
#' @param code_type character. Type of clinical code system to be searched.
#'   Depends on what is available in the lookup tables. See [add_lookup_table()]
#'   on how to add new lookup tables. This can also be configured through the `codeminer.code_type` option.
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
  version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
) {
  check_lookup_args(codes, code_type, version)

  con <- connect_to_db()
  check_database(con)

  lookup_table <- get_lookup_table(con, code_type, version)

  if (identical(codes, "all")) {
    return(dplyr::collect(lookup_table))
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

  if (preferred_description_only) {
    result <- dplyr::filter(result, .data$preferred_description)
  }

  return(result)
}

# Argument validation helpers
check_lookup_args <- function(
  codes,
  code_type,
  version,
  call = rlang::caller_env()
) {
  check_codes(codes)
  check_code_type(code_type, call)
  check_version(version, call)
}

check_codes <- function(codes) {
  if (!rlang::is_character(codes)) {
    cli::cli_abort(
      "{.arg codes} must be a character vector, not {typeof(codes)}",
      call = call
    )
  }
}

check_code_type <- function(code_type, call = rlang::caller_env()) {
  if (!rlang::is_string(code_type)) {
    cli::cli_abort(
      "{.arg code_type} must be a string, not {typeof(code_type)} with length {length(code_type)}",
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

#' Search for codes matching a regular expression
#'
#' Returns a data frame with clinical codes that match the supplied regular
#' expression. Case is *not* ignored.
#'
#' @param reg_expr a regular expression to search for
#' @inheritParams stringr::regex
#' @inheritParams CODES
#' @param codes_only bool. If \code{TRUE}, return a character vector of
#'   \emph{unique} codes. If \code{FALSE} (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#'
#' @return data frame by default, or a character vector of codes if
#'   \code{codes_only} is \code{TRUE}.
#' @export
#' @name codes_like
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' CODES_LIKE(
#'   reg_expr = "^E10.*",
#'   code_type = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
CODES_LIKE <- function(
  reg_expr,
  code_type = getOption("codeminer.code_type"),
  all_lkps_maps = NULL,
  codes_only = FALSE,
  preferred_description_only = TRUE,
  standardise_output = TRUE,
  col_filters = getOption("codeminer.col_filters")
) {
  # validate args
  assertthat::is.string(reg_expr)

  assertthat::assert_that(
    !(codes_only & standardise_output),
    msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`"
  )

  match.arg(arg = code_type, choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code and description columns for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "description_col"
    )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # search for codes

  ## get all codes matching regex

  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(stringr::str_detect(
      string = .data[[code_col]],
      pattern = reg_expr
    )) %>%
    dplyr::collect()

  ## then expand, optionally including both primary and secondary descriptions
  codes <- unique(result[[code_col]])

  codes <- subset(codes, !is.na(codes))

  result <- CODES(
    codes = codes,
    code_type = code_type,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )

  if (codes_only) {
    if (standardise_output) {
      return(result$code)
    } else {
      return(result[[code_col]])
    }
  } else {
    return(result)
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

  all_version_meta <- dplyr::filter(meta, .data$code_type == .env$code_type)

  if (version == "latest") {
    version <- get_latest_version(all_version_meta$lookup_version)
  }
  this_meta <- dplyr::filter(all_version_meta, .data$lookup_version == version)

  if (nrow(this_meta) == 0) {
    cli::cli_abort("No metadata found for '{code_type}' version '{version}'")
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
