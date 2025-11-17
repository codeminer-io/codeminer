#' Get descendents for a code
#'
#' Retrieves children codes for a given set of codes (including the codes
#' themselves). This function works with any relationship table added via
#' [add_relationship_table()].
#'
#' @param codes character. A vector of code strings to retrieve child codes for.
#' @param code_type character. Type of clinical code system to be searched.
#'   This can also be configured through the `codeminer.code_type` option.
#' @param version character. Version of the relationship table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.relationship_version` option.
#' @param codes_only logical. If `TRUE`, return a character vector of
#'   \emph{unique} codes. If `FALSE` (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#' @param preferred_description_only logical. If `TRUE` (default), only returns
#'   the preferred description for each code.
#'
#' @return A data frame with columns `code`, `description`, and `code_type`
#'   (when `codes_only = FALSE`), or a character vector of codes (when
#'   `codes_only = TRUE`).
#'
#' @seealso [add_relationship_table()]
#' @family Clinical code lookups and mappings
#' @export
#' @examples
#' create_dummy_database()
#'
#' # Get children for ICD-10 codes (if relationship table exists)
#' CHILDREN(c("E10", "E11"), code_type = "icd10")
#'
#' # Get only the codes without descriptions
#' CHILDREN(c("E10", "E11"), code_type = "icd10", codes_only = TRUE)
CHILDREN <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  check_codes(codes)
  check_code_type(code_type)
  check_version(version)

  con <- connect_to_db()
  check_database(con)

  relationship_meta <- get_relationship_metadata(con)
  if (!(code_type %in% relationship_meta$code_type)) {
    cli::cli_abort(
      c(
        "No relationship table found for code type '{code_type}'.",
        "i" = "Add a relationship table with {.fun codeminer::add_relationship_table}."
      )
    )
  }

  child_codes <- get_children(
    con = con,
    codes = codes,
    code_type = code_type,
    version = version
  )

  if (length(child_codes) == 0) {
    cli::cli_warn("No valid child codes found.")
    return(if (codes_only) character(0) else data.frame())
  }

  result <- CODES(
    codes = child_codes,
    code_type = code_type,
    version = version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) {
    return(unique(result$code))
  }

  return(result)
}

#' Get child codes from relationship table
#'
#' Generic function to traverse relationship tables and retrieve related codes.
#' This is an internal function used by [CHILDREN()].
#'
#' @param codes Character vector of codes to start from.
#' @param code_type The type of coding system.
#' @param version The version of the relationship table.
#'
#' @return A character vector of related codes.
#' @keywords internal
#' @noRd
get_children <- function(
  con,
  codes,
  code_type,
  version
) {
  rel_meta <- get_metadata_for_relationship(con, code_type, version)

  # Get the relationship table
  rel_table_name <- rel_meta$relationship_table_name
  rel_table <- dplyr::tbl(con, rel_table_name)

  # Determine column names based on metadata
  return_col <- rel_meta$from_col
  filter_col <- rel_meta$to_col
  type_col <- rel_meta$type_col

  # Filter for child-parent relationships only
  rel_table <- rel_table |>
    dplyr::filter(
      .data[[type_col]] == .env$rel_meta$child_parent_relationship_code
    )

  result <- rel_table |>
    dplyr::filter(.data[[filter_col]] %in% .env$codes) |>
    dplyr::select(dplyr::all_of(return_col)) |>
    dplyr::distinct() |>
    dplyr::pull(1)

  return(unique(result))
}

#' Get relationship metadata for a specific code type and version
#'
#' @param con Database connection.
#' @param code_type The code type.
#' @param version The version.
#' @param call Calling environment for error messages.
#'
#' @return A single row data frame with the relationship metadata.
#' @keywords internal
#' @noRd
get_metadata_for_relationship <- function(
  con,
  code_type,
  version,
  call = rlang::caller_env()
) {
  meta <- get_relationship_metadata(con = con)

  if (!(code_type %in% meta$code_type)) {
    cli::cli_abort(
      c(
        "Code type '{code_type}' not found in relationship metadata.",
        "i" = "Did you add the relationship table with {.fun codeminer::add_relationship_table}?"
      ),
      call = call
    )
  }

  all_version_meta <- dplyr::filter(meta, .data$code_type == .env$code_type)

  if (version == "latest") {
    version <- get_latest_version(all_version_meta$relationship_version)
  }

  this_meta <- dplyr::filter(
    all_version_meta,
    .data$relationship_version == .env$version
  )

  if (nrow(this_meta) == 0) {
    cli::cli_abort(
      "No relationship metadata found for '{code_type}' version '{version}'",
      call = call
    )
  }

  stopifnot(nrow(this_meta) == 1) # code_type + version combo should be unique

  return(this_meta)
}
