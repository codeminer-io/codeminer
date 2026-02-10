#' Get attributes or codes with attributes
#'
#' These functions traverse the relationship graph to find attributes for codes
#' or codes that have specific attributes.
#'
#' - `ATTRIBUTES_FOR()` returns attribute codes for the supplied codes
#' - `HAS_ATTRIBUTES()` returns codes that have the specified attribute codes
#'
#' @param ... Codes to start from. Supports flexible input like [CODES()].
#' @param relationship_types Character vector of relationship types to filter
#'   by. If `NULL` (default), all relationship types are included.
#' @inheritParams CHILDREN
#' @family Code relationships
#' @return A data frame of codes and descriptions
#' @name attributes
#' @examples
#' create_dummy_database()
#' # ATTRIBUTES_FOR returns attributes for codes
#' # HAS_ATTRIBUTES returns codes that have the specified attributes
NULL

#' @rdname attributes
#' @export
ATTRIBUTES_FOR <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  check_version(lookup_version)
  check_version(relationship_version)
  check_relationship_types(relationship_types)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Collect and validate input
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

  graph_closure_codes(
    codes = codes_vec,
    code_type = type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    direction = "out",
    rel_type = relationship_types,
    include_self = FALSE,
    max_depth = 1,
    empty_warning = "No codes found with the specified attributes.",
    col_filters = col_filters
  )
}

#' @rdname attributes
#' @export
HAS_ATTRIBUTES <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  check_version(lookup_version)
  check_version(relationship_version)
  check_relationship_types(relationship_types)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Collect and validate input
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

  graph_closure_codes(
    codes = codes_vec,
    code_type = type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    direction = "in",
    rel_type = relationship_types,
    include_self = FALSE,
    max_depth = 1,
    empty_warning = "No codes found with the specified attributes.",
    col_filters = col_filters
  )
}
