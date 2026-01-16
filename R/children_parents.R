#' Retrieve parent or child codes
#'
#' Returns immediate or transitive parent or child codes for the given codes by
#' traversing the relationship graph.
#'
#' Use `N_PARENTS()`/`N_CHILDREN()` for immediate relationships (one step), and
#' `PARENTS()`/`CHILDREN()` for transitive closure (all reachable
#' ancestors/descendants).
#'
#' @param codes Character vector of codes to start from.
#' @param depth Integer. Maximum number of steps to traverse. Use `Inf` for
#'   transitive closure (all ancestors/descendants). Only used by `N_PARENTS()`
#'   and `N_CHILDREN()`.
#' @param code_type Code type (character).
#' @param lookup_version Lookup table version (character).
#' @param relationship_version Relationship table version (character).
#' @param codes_only Logical. If `TRUE`, return only unique codes. If `FALSE`,
#'   return a data frame with code and description.
#' @param preferred_description_only Logical. If `TRUE`, return only preferred
#'   descriptions.
#'
#' @return A data frame of codes and descriptions, or a character vector if
#'   `codes_only = TRUE`.
#' @family Code relationships
#' @name parent_child_retrieval
#' @examples
#' create_dummy_database()
#' PARENTS(c("E10", "E11"), code_type = "ICD-10")
#' CHILDREN(c("E10", "E11"), code_type = "ICD-10")
#' N_PARENTS(c("E10", "E11"), code_type = "ICD-10")
#' N_CHILDREN(c("E10", "E11"), code_type = "ICD-10")
NULL

#' @rdname parent_child_retrieval
#' @export
CHILDREN <- function(
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
  N_CHILDREN(
    codes,
    depth = Inf,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    codes_only = codes_only,
    preferred_description_only = preferred_description_only
  )
}

#' @rdname parent_child_retrieval
#' @export
PARENTS <- function(
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
  N_PARENTS(
    codes,
    depth = Inf,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    codes_only = codes_only,
    preferred_description_only = preferred_description_only
  )
}

#' @rdname parent_child_retrieval
#' @export
N_CHILDREN <- function(
  codes,
  depth = 1,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  check_codes(codes)
  check_depth(depth)
  check_code_type(code_type)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(codes_only, "codes_only")
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  graph_closure_codes(
    codes = codes,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    codes_only = codes_only,
    preferred_description_only = preferred_description_only,
    direction = "in",
    rel_type = from_meta("child_parent_relationship_code"),
    include_self = TRUE,
    max_depth = depth,
    empty_warning = "No valid child codes found."
  )
}

#' @rdname parent_child_retrieval
#' @export
N_PARENTS <- function(
  codes,
  depth = 1,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  check_codes(codes)
  check_depth(depth)
  check_code_type(code_type)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(codes_only, "codes_only")
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  graph_closure_codes(
    codes = codes,
    code_type = code_type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    codes_only = codes_only,
    preferred_description_only = preferred_description_only,
    direction = "out",
    rel_type = from_meta("child_parent_relationship_code"),
    include_self = TRUE,
    max_depth = depth,
    empty_warning = "No valid parent codes found."
  )
}
