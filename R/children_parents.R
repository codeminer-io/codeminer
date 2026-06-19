#' Retrieve parent or child codes
#'
#' Returns immediate or transitive parent or child codes for the given codes by
#' traversing the relationship graph.
#'
#' Use `N_PARENTS()`/`N_CHILDREN()` for immediate relationships (one step), and
#' `PARENTS()`/`CHILDREN()` for transitive closure (all reachable
#' ancestors/descendants).
#'
#' @param ... Codes to start from. Supports flexible input like [CODES()].
#' @param depth Integer. Maximum number of steps to traverse. Use `Inf` for
#'   transitive closure (all ancestors/descendants). Only used by `N_PARENTS()`
#'   and `N_CHILDREN()`.
#' @param type Code type (character).
#' @param lookup_version Lookup table version (character).
#' @param relationship_version Relationship table version (character).
#' @param preferred_description_only Logical. If `TRUE`, return only preferred
#'   descriptions.
#' @param col_filters Column filters to apply. See [CODES()] for details.
#' @param call **For internal use only.** The execution environment of a
#'   currently running function. Used for error reporting. Users should not
#'   need to set this parameter.
#'
#' @return A data frame of codes and descriptions.
#' @family Code relationships
#' @name parent_child_retrieval
#' @examples
#' create_dummy_database()
#' PARENTS("E10", "E11", type = "ICD-10")
#' CHILDREN("E10", "E11", type = "ICD-10")
#' N_PARENTS("E10", "E11", type = "ICD-10")
#' N_CHILDREN("E10", "E11", type = "ICD-10")
NULL

#' @rdname parent_child_retrieval
#' @export
CHILDREN <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  N_CHILDREN(
    ...,
    depth = Inf,
    type = type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters,
    call = rlang::current_env()
  )
}

#' @rdname parent_child_retrieval
#' @export
PARENTS <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  N_PARENTS(
    ...,
    depth = Inf,
    type = type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters,
    call = rlang::current_env()
  )
}

#' @rdname parent_child_retrieval
#' @export
N_CHILDREN <- function(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE,
  col_filters = "default",
  call = rlang::current_env()
) {
  check_depth(depth)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Collect and validate input
  collected <- collect_codes_input(..., type = type, call = call)
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
    rel_type = from_meta("child_parent_relationship_code"),
    include_self = TRUE,
    max_depth = depth,
    empty_warning = "No valid child codes found.",
    col_filters = col_filters,
    call = call
  )
}

#' @rdname parent_child_retrieval
#' @export
N_PARENTS <- function(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  preferred_description_only = TRUE,
  col_filters = "default",
  call = rlang::current_env()
) {
  check_depth(depth)
  check_version(lookup_version)
  check_version(relationship_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Collect and validate input
  collected <- collect_codes_input(..., type = type, call = call)
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
    rel_type = from_meta("child_parent_relationship_code"),
    include_self = TRUE,
    max_depth = depth,
    empty_warning = "No valid parent codes found.",
    col_filters = col_filters,
    call = call
  )
}
