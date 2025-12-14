#' Get attributes for codes
#'
#' Returns attributes for a set of codes by traversing the relationship graph.
#'
#' @inheritParams CHILDREN
#' @family Code relationships
#' @return A data frame of attribute codes and descriptions, or a character
#'   vector if `codes_only = TRUE`.
#' @export
ATTRIBUTES <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  graph_closure_codes(
    codes = codes,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    codes_only = codes_only,
    preferred_description_only = preferred_description_only,
    direction = "out",
    rel_type = NULL,
    include_self = FALSE,
    max_depth = 1,
    empty_warning = "No codes found with specified attributes."
  )
}
