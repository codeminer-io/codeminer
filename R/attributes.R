#' Get attributes or codes with attributes
#'
#' These functions traverse the relationship graph to find attributes for codes
#' or codes that have specific attributes.
#'
#' - `ATTRIBUTES_FOR()` returns attribute codes for the supplied codes
#' - `HAS_ATTRIBUTES()` returns codes that have the supplied attribute codes
#'
#' @param codes Character vector of codes to start from.
#' @param attribute_codes Character vector of attribute codes to search for.
#' @param relationship_types Character vector of relationship types to filter by.
#'   If `NULL` (default), all relationship types are included.
#' @inheritParams CHILDREN
#' @family Code relationships
#' @return A data frame of codes and descriptions, or a character vector if
#'   `codes_only = TRUE`.
#' @name attributes
#' @examples
#' create_dummy_database()
#' # ATTRIBUTES_FOR returns attributes for codes
#' # HAS_ATTRIBUTES returns codes that have the specified attributes
NULL

#' @rdname attributes
#' @export
ATTRIBUTES_FOR <- function(
  codes,
  relationship_types = NULL,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE
) {
  check_relationship_types(relationship_types)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  graph_closure_codes(
    codes = codes,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    direction = "out",
    rel_type = relationship_types,
    include_self = FALSE,
    max_depth = 1,
    empty_warning = "No attributes found for the specified codes."
  )
}

#' @rdname attributes
#' @export
HAS_ATTRIBUTES <- function(
  attribute_codes,
  relationship_types = NULL,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE
) {
  check_relationship_types(relationship_types)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  graph_closure_codes(
    codes = attribute_codes,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    direction = "in",
    rel_type = relationship_types,
    include_self = FALSE,
    max_depth = 1,
    empty_warning = "No codes found with the specified attributes."
  )
}
