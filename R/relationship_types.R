#' Get relationship types for codes
#'
#' These functions return the distinct relationship types that originate from or
#' point to the supplied codes.
#'
#' - `RELATIONSHIP_TYPES_FROM()` returns relationship types originating from codes
#' - `RELATIONSHIP_TYPES_TO()` returns relationship types pointing to codes
#'
#' @inheritParams CHILDREN
#' @return A character vector of distinct relationship types.
#' @family Code relationships
#' @name relationship_types
#' @examples
#' create_dummy_database()
#' # RELATIONSHIP_TYPES_FROM returns types originating from codes
#' # RELATIONSHIP_TYPES_TO returns types pointing to codes
NULL

#' @rdname relationship_types
#' @export
RELATIONSHIP_TYPES_FROM <- function(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  col_filters = "default"
) {
  # Collect and validate codes input
  collected <- collect_codes_input(..., type = type)
  codes_vec <- collected$codes

  # Use codelist code_type if provided
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  check_code_type(type)
  check_version(relationship_version)

  con <- get_db_con()
  meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call = rlang::caller_env()
  )
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  # Apply col_filters to relationship table
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  rel_table <- apply_col_filters(
    rel_table, resolved,
    tbl_name = meta$relationship_table_name
  )

  # Get relationship types where codes are in the 'from' column
  result <- rel_table |>
    dplyr::filter(.data[[meta$from_col]] %in% .env$codes_vec) |>
    dplyr::select(dplyr::all_of(meta$type_col)) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(dplyr::all_of(meta$type_col))

  # Warning if no codes found
  available_codes <- rel_table |>
    dplyr::filter(.data[[meta$from_col]] %in% .env$codes_vec) |>
    dplyr::select(dplyr::all_of(meta$from_col)) |>
    dplyr::distinct() |>
    dplyr::pull(dplyr::all_of(meta$from_col))

  missing_codes <- setdiff(codes_vec, available_codes)
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "relationship",
      table_meta = meta
    )
  }

  if (length(result) == 0) {
    codeminer_warn(
      "No relationship types found originating from the specified codes."
    )
    return(as_codelist(tibble::tibble(
      code = character(),
      description = character(),
      code_type = character()
    )))
  }

  # Return as codelist (relationship types are metadata, not codes in lookup table)
  as_codelist(tibble::tibble(
    code = result,
    description = result, # For relationship types, code and description are the same
    code_type = type
  ))
}

#' @rdname relationship_types
#' @export
RELATIONSHIP_TYPES_TO <- function(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  col_filters = "default"
) {
  # Collect and validate codes input
  collected <- collect_codes_input(..., type = type)
  codes_vec <- collected$codes

  # Use codelist code_type if provided
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  check_code_type(type)
  check_version(relationship_version)

  con <- get_db_con()
  meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call = rlang::caller_env()
  )
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  # Apply col_filters to relationship table
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  rel_table <- apply_col_filters(
    rel_table, resolved,
    tbl_name = meta$relationship_table_name
  )

  # Get relationship types where codes are in the 'to' column
  result <- rel_table |>
    dplyr::filter(.data[[meta$to_col]] %in% .env$codes_vec) |>
    dplyr::select(dplyr::all_of(meta$type_col)) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(dplyr::all_of(meta$type_col))

  # Warning if no codes found
  available_codes <- rel_table |>
    dplyr::filter(.data[[meta$to_col]] %in% .env$codes_vec) |>
    dplyr::select(dplyr::all_of(meta$to_col)) |>
    dplyr::distinct() |>
    dplyr::pull(dplyr::all_of(meta$to_col))

  missing_codes <- setdiff(codes_vec, available_codes)
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "relationship",
      table_meta = meta
    )
  }

  if (length(result) == 0) {
    codeminer_warn(
      "No relationship types found pointing to the specified codes."
    )
    return(as_codelist(tibble::tibble(
      code = character(),
      description = character(),
      code_type = character()
    )))
  }

  # Return as codelist (relationship types are metadata, not codes in lookup table)
  as_codelist(tibble::tibble(
    code = result,
    description = result, # For relationship types, code and description are the same
    code_type = type
  ))
}
