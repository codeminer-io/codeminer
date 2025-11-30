#' Get relationship metadata for a specific code type and version
#'
#' @param con Database connection.
#' @param code_type The code type.
#' @param relationship_version The version.
#' @param call Calling environment for error messages.
#'
#' @return A single row data frame with the relationship metadata.
#' @keywords internal
#' @noRd
get_metadata_for_relationship <- function(
  con,
  code_type,
  relationship_version,
  call = rlang::caller_env()
) {
  meta <- get_relationship_metadata(con = con)

  if (!(code_type %in% meta$code_type)) {
    codeminer_abort(
      c(
        "Code type '{code_type}' not found in relationship metadata.",
        "i" = "Did you add the relationship table with {.fun codeminer::add_relationship_table}?"
      ),
      call = call
    )
  }

  all_version_meta <- dplyr::filter(meta, .data$code_type == .env$code_type)

  if (identical(relationship_version, "latest")) {
    relationship_version <- get_latest_version(
      all_version_meta$relationship_version
    )
  }

  this_meta <- dplyr::filter(
    all_version_meta,
    .data$relationship_version == .env$relationship_version
  )

  if (nrow(this_meta) == 0) {
    codeminer_abort(
      "No relationship metadata found for '{code_type}' version '{relationship_version}'",
      call = call
    )
  }

  stopifnot(nrow(this_meta) == 1) # code_type + version combo should be unique

  return(this_meta)
}


#' Perform a single step graph traversal
#'
#' Returns nodes that are immediately connected to the input nodes by following
#' edges in the specified direction.
#'
#' @param nodes Character vector of node IDs to start from.
#' @param relationship_tbl A dbplyr table containing relationship data.
#' @param from_colname Name of column containing 'from' nodes (e.g., child).
#' @param to_colname Name of column containing 'to' nodes (e.g., parent).
#' @param type_colname Name of column containing relationship type.
#' @param direction Character. Either "out" (follow edges from nodes) or "in"
#'   (follow edges to nodes).
#' @param rel_type Character vector of relationship types to filter by. If NULL,
#'   all relationship types are included.
#'
#' @return Character vector of node IDs reached in one step.
#' @keywords internal
graph_step <- function(
  nodes,
  relationship_tbl,
  from_colname = "from_col",
  to_colname = "to_col",
  type_colname = "type_col",
  direction = c("out", "in"),
  rel_type = NULL
) {
  direction <- rlang::arg_match(direction)

  # Determine which column to filter and which to return
  filter_col <- if (direction == "out") from_colname else to_colname
  return_col <- if (direction == "out") to_colname else from_colname

  # Ensure nodes is a vector
  if (!is.vector(nodes)) {
    nodes <- unique(nodes)
  } else {
    nodes <- unique(nodes)
  }

  # Query the relationship table
  result <- relationship_tbl |>
    dplyr::filter(.data[[filter_col]] %in% .env$nodes)

  # Apply relationship type filter if specified
  if (!is.null(rel_type)) {
    result <- result |>
      dplyr::filter(.data[[type_colname]] %in% .env$rel_type)
  }

  # Return unique IDs from the return column
  result |>
    dplyr::pull(!!return_col) |>
    unique()
}

#' Perform transitive closure graph traversal
#'
#' Returns all nodes reachable from the input nodes by recursively following
#' edges until no new nodes are discovered.
#'
#' @inheritParams graph_step
#' @param include_self Logical. If TRUE, include the starting nodes in the result.
#' @param max_depth Integer. Maximum number of steps to traverse. Default is Inf
#'   (complete traversal).
#'
#' @return Character vector of all reachable node IDs.
#' @keywords internal
graph_closure <- function(
  nodes,
  relationship_tbl,
  from_colname = "from_col",
  to_colname = "to_col",
  type_colname = "type_col",
  direction = c("out", "in"),
  rel_type = NULL,
  include_self = FALSE,
  max_depth = Inf
) {
  direction <- rlang::arg_match(direction)

  # Determine which column to filter and which to return
  filter_col <- if (direction == "out") from_colname else to_colname
  return_col <- if (direction == "out") to_colname else from_colname

  # Ensure nodes is a vector
  if (!is.vector(nodes)) {
    seed_nodes <- unique(nodes)
  } else {
    seed_nodes <- unique(nodes)
  }

  # Recursive traversal function
  traverse <- function(
    accumulated_edges = NULL,
    frontier_nodes = NULL,
    depth = 0
  ) {
    # Check max depth
    if (depth >= max_depth) {
      return(accumulated_edges)
    }

    # Query relationship table for edges from frontier
    related_edges <- relationship_tbl |>
      dplyr::filter(.data[[filter_col]] %in% .env$frontier_nodes)

    # Apply relationship type filter if specified
    if (!is.null(rel_type)) {
      related_edges <- related_edges |>
        dplyr::filter(.data[[type_colname]] %in% .env$rel_type)
    }

    # Collect the edges
    related_edges <- related_edges |>
      dplyr::select(
        !!from_colname := .data[[from_colname]],
        !!to_colname := .data[[to_colname]],
        !!type_colname := .data[[type_colname]]
      ) |>
      dplyr::collect()

    # Combine with accumulated edges
    result <- dplyr::bind_rows(accumulated_edges, related_edges) |>
      dplyr::distinct()

    # Check if we found new edges
    if (is.null(accumulated_edges) || nrow(result) > nrow(accumulated_edges)) {
      # Extract new frontier nodes for next iteration
      new_frontier <- unique(related_edges[[return_col]])

      return(
        traverse(
          accumulated_edges = result,
          frontier_nodes = new_frontier,
          depth = depth + 1
        )
      )
    } else {
      return(result)
    }
  }

  # Execute traversal
  edges <- traverse(
    accumulated_edges = NULL,
    frontier_nodes = seed_nodes,
    depth = 0
  )

  # Extract unique node IDs from return column
  result_nodes <- unique(edges[[return_col]])

  # Optionally include starting nodes
  if (include_self) {
    result_nodes <- c(seed_nodes, result_nodes) |> unique()
  }

  result_nodes
}

#' Get parent codes
#'
#' Returns immediate parent codes for the given codes.
#'
#' @inheritParams CHILDREN
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
  check_code_type(code_type)

  con <- connect_to_db()
  meta <- get_metadata_for_relationship(con, code_type, relationship_version)
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  parent_codes <- graph_closure(
    nodes = codes,
    relationship_tbl = rel_table,
    from_colname = meta$from_col,
    to_colname = meta$to_col,
    type_colname = meta$type_col,
    direction = "out",
    rel_type = meta$child_parent_relationship_code,
    include_self = TRUE,
    max_depth = depth
  )

  # Include self
  parent_codes <- c(codes, parent_codes) |> unique()

  if (length(parent_codes) == 0) {
    codeminer_warn("No valid parent codes found.")
    return(if (codes_only) character(0) else data.frame())
  }

  result <- CODES(
    codes = parent_codes,
    code_type = code_type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) return(unique(result$code))
  return(result)
}

#' Get ancestor codes
#'
#' Returns all ancestor codes (transitive closure) for the given codes.
#'
#' @inheritParams CHILDREN
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

#' Get child codes
#'
#' Retrieves child codes for a given set of codes. This function works with any
#' relationship table added via [add_relationship_table()]. After finding child
#' codes, the code and description information is retrieved from the lookup
#' table.
#'
#' @param codes character. A vector of code strings to retrieve child codes for.
#' @param code_type character. Type of clinical code system to be searched. This
#'   can also be configured through the `codeminer.code_type` option.
#' @param lookup_version character. Version of the lookup table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.lookup_version`
#'   option.
#' @param relationship_version character. Version of the relationship table to
#'   use. Default: `"latest"`. Can be configured through the
#'   `codeminer.relationship_version` option.
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
#' @seealso [CODES()], which is used to retrieve code information, and
#'   [add_relationship_table()] for how to add relationship tables.
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
  check_code_type(code_type)

  con <- connect_to_db()
  meta <- get_metadata_for_relationship(con, code_type, relationship_version)
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  child_codes <- graph_closure(
    nodes = codes,
    relationship_tbl = rel_table,
    from_colname = meta$from_col,
    to_colname = meta$to_col,
    type_colname = meta$type_col,
    direction = "in",
    rel_type = meta$child_parent_relationship_code,
    include_self = TRUE,
    max_depth = depth
  )

  # Include self
  child_codes <- c(codes, child_codes) |> unique()

  if (length(child_codes) == 0) {
    codeminer_warn("No valid child codes found.")
    return(if (codes_only) character(0) else data.frame())
  }

  result <- CODES(
    codes = child_codes,
    code_type = code_type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) return(unique(result$code))
  return(result)
}

#' Get descendant codes
#'
#' Returns all descendant codes (transitive closure) for the given codes.
#'
#' @inheritParams CHILDREN
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

#' Get attributes for codes
#'
#' Returns attributes for a set of codes.
#'
#' @inheritParams CHILDREN
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
  check_codes(codes)
  check_code_type(code_type)

  con <- connect_to_db()
  meta <- get_metadata_for_relationship(con, code_type, relationship_version)
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  codes_with_attributes <- graph_closure(
    nodes = codes,
    relationship_tbl = rel_table,
    from_colname = meta$from_col,
    to_colname = meta$to_col,
    type_colname = meta$type_col,
    direction = "out",
    rel_type = NULL,
    include_self = FALSE,
    max_depth = 1
  )

  if (length(codes_with_attributes) == 0) {
    codeminer_warn("No codes found with specified attributes.")
    return(if (codes_only) character(0) else data.frame())
  }

  result <- CODES(
    codes = codes_with_attributes,
    code_type = code_type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) return(unique(result$code))
  return(result)
}
