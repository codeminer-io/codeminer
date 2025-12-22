#' Retrieve relationship metadata for a code type and version
#'
#' @param con A database connection.
#' @param code_type Code type (character).
#' @param relationship_version Relationship table version (character).
#' @param call Calling environment for error messages. Defaults to
#'   [rlang::caller_env()].
#'
#' @return A single-row data frame with the relationship metadata.
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

#' Perform transitive closure graph traversal
#'
#' Returns all nodes reachable from the input nodes by recursively following
#' edges until no new nodes are discovered.
#'
#' @param nodes Character vector of node IDs to start from.
#' @param relationship_tbl A `dbplyr` table containing relationship data.
#' @param from_colname Name of column containing 'from' nodes (e.g. child).
#' @param to_colname Name of column containing 'to' nodes (e.g. parent).
#' @param type_colname Name of column containing relationship type.
#' @param direction Either `"out"` (follow edges from nodes) or `"in"` (follow
#'   edges to nodes).
#' @param rel_type Character vector of relationship types to filter by. If
#'   `NULL`, all types are included.
#' @param include_self Logical. If `TRUE`, include the starting nodes in the
#'   result.
#' @param max_depth Integer. Maximum number of steps to traverse. Default is
#'   `Inf` (complete traversal).
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
        !!from_colname := dplyr::all_of(from_colname),
        !!to_colname := dplyr::all_of(to_colname),
        !!type_colname := dplyr::all_of(type_colname)
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


#' Graph closure with code lookup
#'
#' Wrapper around `graph_closure()` that handles metadata retrieval, performs
#' the graph traversal, and returns codes with descriptions via [CODES()].
#'
#' @param codes Character vector of codes to start from.
#' @param code_type Code type (character).
#' @param lookup_version Lookup table version (character).
#' @param relationship_version Relationship table version (character).
#' @param codes_only Logical. If `TRUE`, return only unique codes.
#' @param preferred_description_only Logical. If `TRUE`, return only preferred
#'   descriptions.
#' @param direction Either `"out"` or `"in"`.
#' @param rel_type Relationship type filter. Can be:
#'   - `from_meta()` to extract from metadata
#'   - A direct value (character vector)
#'   - `NULL` for no filtering.
#' @param include_self Logical. If `TRUE`, include starting codes in result.
#' @param max_depth Maximum traversal depth (integer).
#' @param empty_warning Warning message when no codes are found (character).
#'
#' @return A data frame with code information, or a character vector if
#'   `codes_only = TRUE`.
#' @keywords internal
#' @noRd
graph_closure_codes <- function(
  codes,
  code_type,
  lookup_version,
  relationship_version,
  codes_only,
  preferred_description_only,
  direction,
  rel_type = from_meta("child_parent_relationship_code"),
  include_self = TRUE,
  max_depth = Inf,
  empty_warning = "No valid codes found.",
  call = rlang::caller_env()
) {
  check_codes(codes)
  check_code_type(code_type)

  con <- connect_to_db()
  meta <- get_metadata_for_relationship(
    con,
    code_type,
    relationship_version,
    call = call
  )
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  # Resolve rel_type if it's a from_meta reference

  if (inherits(rel_type, "from_meta")) {
    rel_type <- meta[[as.character(rel_type)]]
  }

  # Warning if any input codes are not present in relationship table
  available_codes <- rel_table |>
    dplyr::filter(
      .data[[meta$from_col]] %in%
        .env$codes |
        .data[[meta$to_col]] %in% .env$codes
    ) |>
    dplyr::select(dplyr::all_of(c(meta$from_col, meta$to_col))) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::select(dplyr::all_of("value")) |>
    dplyr::distinct() |>
    dplyr::pull(dplyr::all_of("value"))

  missing_codes <- setdiff(codes, available_codes)

  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "relationship",
      table_meta = meta
    )
  }

  result_codes <- graph_closure(
    nodes = codes,
    relationship_tbl = rel_table,
    from_colname = meta$from_col,
    to_colname = meta$to_col,
    type_colname = meta$type_col,
    direction = direction,
    rel_type = rel_type,
    include_self = include_self,
    max_depth = max_depth
  )

  # Include self (graph_closure handles this, but we also add original codes)
  if (include_self) {
    result_codes <- c(codes, result_codes) |> unique()
  }

  if (length(result_codes) == 0) {
    codeminer_warn(empty_warning)
    return(if (codes_only) character(0) else data.frame())
  }

  result <- CODES(
    codes = result_codes,
    code_type = code_type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) {
    return(unique(result$code))
  }
  return(result)
}

#' Mark a value to be extracted from metadata
#'
#' Use this helper to indicate that a parameter value should be extracted
#' from the relationship metadata rather than used directly.
#'
#' @param col_name A string. The column name to extract from metadata.
#' @return A character vector of class `from_meta` and `character`.
#' @keywords internal
#' @noRd
from_meta <- function(col_name) {
  rlang::check_required(col_name)
  if (!rlang::is_string(col_name)) {
    cli::cli_abort(
      "{.arg col_name} must be a single string, not {.obj_type_friendly {col_name}}."
    )
  }
  structure(col_name, class = c("from_meta", "character"))
}
