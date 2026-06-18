#' Get the full relationship table for a code type
#'
#' Returns a lazy `dplyr::tbl()` containing the relationship table with
#' standardised column names (`from`, `to`, `type`, `code_type`) plus all
#' additional columns from the underlying database table. Call
#' [dplyr::collect()] to materialise the result.
#'
#' This is useful for inspecting the raw relationship data used by
#' [CHILDREN()], [PARENTS()], and other graph traversal functions.
#'
#' @param type The code type for which to retrieve relationships.
#' @param codes Optional character vector of codes used to filter edges. If
#'   `NULL` (default), all rows are returned. The `endpoints` argument
#'   controls how an edge is matched against `codes`.
#' @param endpoints One of `"both"` (default) or `"either"`. With `"both"`,
#'   an edge is kept only when both endpoints (`from` *and* `to`) are in
#'   `codes`. With `"either"`, an edge is kept when at least one endpoint is
#'   in `codes` — this surfaces edges crossing the boundary of the input set.
#'   For the common "give me ancestors / descendants of these codes as a
#'   codelist" question, prefer [PARENTS()] / [CHILDREN()].
#' @param relationship_version The version to retrieve. Defaults to `"latest"`.
#' @param col_filters Column filters to apply. See [CODES()] for details.
#' @param con Optional DBI connection. If `NULL` (default), uses the
#'   workbench connection.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A lazy `dplyr::tbl()` with standardised columns (`from`, `to`,
#'   `type`, `code_type`) plus all other columns from the underlying table.
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [PARENTS()], [CHILDREN()] for the common "give me ancestors /
#'   descendants of these codes as a codelist" use case — this helper
#'   returns raw edge rows and is mainly useful when the edge structure
#'   itself is needed. [get_relationship_tree()] for a `{nodes, edges}`
#'   tree view. [get_codeminer_metadata()] for discovering available
#'   tables.
#' @examples
#' create_dummy_database()
#'
#' # Get the full ICD-10 relationship table
#' get_relationship_table("ICD-10") |> dplyr::collect()
#'
#' # Default endpoints = "both": only internal edges of the input set
#' # (UKB-style ICD-10 codes have no dot — e.g. E101 = E10.1).
#' get_relationship_table(
#'   "ICD-10",
#'   codes = c("E10", "E101", "E102")
#' ) |> dplyr::collect()
#'
#' # endpoints = "either": also surfaces edges crossing the boundary —
#' # e.g. the parent of E10 and any siblings of E101 / E102. Rarely
#' # needed in user code; prefer PARENTS() / CHILDREN() for those.
#' get_relationship_table(
#'   "ICD-10",
#'   codes = c("E10", "E101", "E102"),
#'   endpoints = "either"
#' ) |> dplyr::collect()
get_relationship_table <- function(
  type,
  codes = NULL,
  endpoints = c("both", "either"),
  relationship_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
) {
  endpoints <- rlang::arg_match(endpoints)
  con <- get_db_con(con)
  meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call
  )
  tbl <- dplyr::tbl(con, meta$relationship_table_name)

  # Apply col_filters
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  tbl <- apply_col_filters(
    tbl,
    resolved,
    tbl_name = meta$relationship_table_name,
    call = call
  )

  check_meta_columns_exist(
    tbl,
    cols = c(
      from_col = meta$from_col,
      to_col = meta$to_col,
      type_col = meta$type_col
    ),
    tbl_name = meta$relationship_table_name,
    metadata_type = "relationship",
    call = call
  )

  # Standardise key columns, keep all others
  tbl <- dplyr::select(
    tbl,
    from = .env$meta$from_col,
    to = .env$meta$to_col,
    type = .env$meta$type_col,
    dplyr::everything()
  ) |>
    dplyr::mutate(code_type = .env$type)

  if (!is.null(codes)) {
    tbl <- if (endpoints == "both") {
      dplyr::filter(
        tbl,
        .data$from %in% .env$codes & .data$to %in% .env$codes
      )
    } else {
      dplyr::filter(
        tbl,
        .data$from %in% .env$codes | .data$to %in% .env$codes
      )
    }
  }

  tbl
}

get_metadata_for_relationship <- function(
  con,
  code_type,
  relationship_version,
  call = rlang::caller_env()
) {
  meta <- get_relationship_metadata(con = con)
  resolve_versioned_metadata(
    meta,
    code_type_val = code_type,
    version_val = relationship_version,
    version_col = "relationship_version",
    pin_type = "relationship",
    type_label = "relationship",
    add_fun_name = "codeminer::add_relationship_table",
    call = call
  )
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

  # Ensure nodes is unique
  seed_nodes <- unique(nodes)

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

    # NA / all-NA / length-0 rel_type means "no filter" (see #148).
    if (
      !is.null(rel_type) &&
        length(rel_type) > 0L &&
        !all(is.na(rel_type))
    ) {
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
#' @return A data frame with code information.
#' @keywords internal
#' @noRd
graph_closure_codes <- function(
  codes,
  code_type,
  lookup_version,
  relationship_version,
  preferred_description_only,
  direction,
  rel_type = from_meta("child_parent_relationship_code"),
  include_self = TRUE,
  max_depth = Inf,
  empty_warning = "No valid codes found.",
  col_filters = "default",
  call = rlang::caller_env()
) {
  check_code_type(code_type, call = call)

  con <- get_db_con()
  meta <- get_metadata_for_relationship(
    con,
    code_type,
    relationship_version,
    call = call
  )
  rel_table <- dplyr::tbl(con, meta$relationship_table_name)

  # Apply col_filters to relationship table before traversal
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = code_type
  )
  rel_table <- apply_col_filters(
    rel_table,
    resolved,
    tbl_name = meta$relationship_table_name,
    call = call
  )

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
    # Classify the codes absent from the relationship table: those present in
    # the lookup point to a lookup/relationship version mismatch (or a code with
    # no recorded relationships); those absent from both are likely invalid.
    in_lookup <- codes_present_in_lookup(
      missing_codes,
      code_type,
      lookup_version,
      con
    )
    not_in_lookup <- setdiff(missing_codes, in_lookup)
    n_in <- length(in_lookup)
    n_out <- length(not_in_lookup)

    extra <- c(
      if (n_in > 0) {
        c(
          "i" = paste0(
            "{n_in} of these {cli::qty(n_in)}{?is/are} in the {code_type} lookup ",
            "table but absent from its relationship table - they may have no ",
            "recorded relationships, or the lookup and relationship versions may ",
            "be mismatched."
          )
        )
      },
      if (n_out > 0) {
        c(
          "i" = paste0(
            "{n_out} of these {cli::qty(n_out)}{?is/are} not in the {code_type} ",
            "lookup table either - they may be invalid codes."
          )
        )
      }
    )

    missing_codes_warning(
      missing_codes,
      table_type = "relationship",
      table_meta = meta,
      extra = extra
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
    return(as_codelist(tibble::tibble(
      code = character(),
      description = character(),
      code_type = character()
    )))
  }

  # Codes reached by traversal are looked up for descriptions; if any are absent
  # from the lookup, enrich the resulting warning with the likely cause.
  result <- with_lookup_miss_hint(
    CODES(
      result_codes,
      type = code_type,
      lookup_version = lookup_version,
      preferred_description_only = preferred_description_only,
      col_filters = col_filters
    ),
    hint = c(
      "i" = paste0(
        "These codes are in the {code_type} relationship table but absent from ",
        "its lookup table - check the lookup and relationship versions were built ",
        "together."
      )
    )
  )

  return(result)
}

#' Build a flat nodes/edges tree view for a set of codes
#'
#' Given a set of seed `codes`, returns a `list(nodes, edges)` suitable for
#' hierarchy analysis or tree rendering (e.g. `data.tree`, `ggraph`,
#' `visNetwork`). By default the seed is first expanded into its full
#' descendant set, then the parent/child edges *among* that set are returned
#' alongside a node table carrying term and category.
#'
#' Internally composes [graph_closure()] (for descendant expansion) and the
#' filtered [get_relationship_table()] / [get_lookup_table()] getters. Edges
#' are restricted to the hierarchical relationship type defined by
#' `child_parent_relationship_code` in the relationship metadata — non
#' hierarchical edges (e.g. SNOMED "has finding site") are excluded.
#'
#' @param codes Character vector of seed codes.
#' @param type The code type (character). Can also be configured via the
#'   `codeminer.code_type` option.
#' @param expand_to_descendants Logical. If `TRUE` (default), `codes` are
#'   expanded to include all descendants via [graph_closure()] before
#'   collecting edges and nodes. If `FALSE`, `codes` is used as-is.
#' @param max_codes Integer. Maximum size of the (expanded) code set.
#'   Aborts with class `codeminer_max_tree_codes_exceeded` if exceeded.
#'   Defaults to `getOption("codeminer.max_tree_codes", default = 10000)` —
#'   a guardrail against accidentally materialising a SNOMED-sized
#'   subgraph in R memory.
#' @param relationship_version Relationship table version. Defaults to
#'   `"latest"`. Can be configured via the `codeminer.relationship_version`
#'   option.
#' @param lookup_version Lookup table version. Defaults to `"latest"`. Can
#'   be configured via the `codeminer.lookup_version` option.
#' @param col_filters Column filters to apply to both the relationship
#'   and lookup tables. See [CODES()] for details.
#' @param preferred_description_only Logical. If `TRUE` (default), node
#'   `term` is the preferred description only (one row per code).
#' @param con Optional DBI connection. If `NULL` (default), uses the
#'   workbench connection.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A plain `list` with two tibbles:
#' \describe{
#'   \item{`nodes`}{`code`, `term`, `category`, `in_input_set` (logical:
#'     `TRUE` for codes in the original input, `FALSE` for codes added by
#'     descendant expansion).}
#'   \item{`edges`}{`parent`, `child` — one row per hierarchical edge among
#'     the (expanded) code set.}
#' }
#'
#' Orphan codes (in the set but with no hierarchical edges) appear in
#' `nodes` with no rows in `edges`.
#'
#' @section Options:
#' - `codeminer.max_tree_codes`: default size cap for the expanded code set.
#'   Overridden by the `max_codes` argument.
#' - `codeminer.code_type`, `codeminer.lookup_version`,
#'   `codeminer.relationship_version`: shared with [CODES()] / [CHILDREN()] /
#'   [PARENTS()], used as defaults for the corresponding arguments.
#'
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [get_relationship_table()] for raw edges,
#'   [get_lookup_table()] for terms, [PARENTS()] / [CHILDREN()] for
#'   flat codelist traversal.
#' @examples
#' create_dummy_database()
#'
#' tree <- get_relationship_tree("E10", type = "ICD-10")
#' tree$nodes
#' tree$edges
#'
#' # `in_input_set` flags the seed apart from expanded descendants
#' subset(tree$nodes, in_input_set)
get_relationship_tree <- function(
  codes,
  type = getOption("codeminer.code_type"),
  expand_to_descendants = TRUE,
  max_codes = getOption("codeminer.max_tree_codes", default = 10000),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  col_filters = "default",
  preferred_description_only = TRUE,
  con = NULL,
  call = rlang::caller_env()
) {
  # Validate inputs
  check_code_type(type, call = call)
  check_version(relationship_version, call = call)
  check_version(lookup_version, call = call)
  check_logical_scalar(
    expand_to_descendants,
    "expand_to_descendants",
    call = call
  )
  check_logical_scalar(
    preferred_description_only,
    "preferred_description_only",
    call = call
  )
  if (!is.character(codes) || length(codes) == 0) {
    codeminer_abort(
      "{.arg codes} must be a non-empty character vector.",
      call = call
    )
  }
  if (
    !rlang::is_scalar_integerish(max_codes) ||
      !is.finite(max_codes) ||
      max_codes < 1
  ) {
    codeminer_abort(
      "{.arg max_codes} must be a positive integer.",
      call = call
    )
  }

  con <- get_db_con(con)

  # Early guard: bail before doing any expansion work if the input is already
  # too big.
  input_unique <- unique(codes)
  if (length(input_unique) > max_codes) {
    codeminer_abort(
      c(
        "Input has {length(input_unique)} codes, exceeding {.code max_codes} ({max_codes}).",
        "i" = "Raise the limit by passing {.code max_codes} or setting {.code options(codeminer.max_tree_codes = N)}."
      ),
      class = "codeminer_max_tree_codes_exceeded",
      call = call
    )
  }

  # Resolve relationship metadata once and build a lazy table with col_filters
  # applied — shared between the expansion step and the edges fetch.
  rel_meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call = call
  )
  rel_tbl <- dplyr::tbl(con, rel_meta$relationship_table_name)
  resolved <- resolve_col_filters(
    col_filters,
    rel_meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  rel_tbl <- apply_col_filters(
    rel_tbl,
    resolved,
    tbl_name = rel_meta$relationship_table_name,
    call = call
  )

  hierarchy_rel <- rel_meta$child_parent_relationship_code

  # Expand the seed set to all descendants if requested. In this package
  # `from = child`, `to = parent`, so descendants of a seed are reached by
  # filtering on `to == seed` and returning `from` — i.e. direction = "in".
  expanded <- if (expand_to_descendants) {
    graph_closure(
      nodes = input_unique,
      relationship_tbl = rel_tbl,
      from_colname = rel_meta$from_col,
      to_colname = rel_meta$to_col,
      type_colname = rel_meta$type_col,
      direction = "in",
      rel_type = hierarchy_rel,
      include_self = TRUE
    )
  } else {
    input_unique
  }

  # Post-expansion guard
  if (length(expanded) > max_codes) {
    codeminer_abort(
      c(
        "Expanded code set has {length(expanded)} codes, exceeding {.code max_codes} ({max_codes}).",
        "i" = "Pass {.code expand_to_descendants = FALSE} or raise {.code max_codes}.",
        "i" = "You can also set {.code options(codeminer.max_tree_codes = N)}."
      ),
      class = "codeminer_max_tree_codes_exceeded",
      call = call
    )
  }

  # Fetch edges: both endpoints must be in the expanded set, and only the
  # hierarchical relationship type.
  edges <- get_relationship_table(
    type = type,
    codes = expanded,
    endpoints = "both",
    relationship_version = relationship_version,
    col_filters = col_filters,
    con = con,
    call = call
  ) |>
    dplyr::filter(.data$type %in% .env$hierarchy_rel) |>
    dplyr::collect() |>
    dplyr::select(parent = "to", child = "from")

  # Build nodes from the lookup table.
  nodes_tbl <- get_lookup_table(
    type = type,
    codes = expanded,
    lookup_version = lookup_version,
    col_filters = col_filters,
    con = con,
    call = call
  )
  if (preferred_description_only) {
    nodes_tbl <- dplyr::filter(nodes_tbl, .data$preferred_description)
  }
  nodes <- nodes_tbl |>
    dplyr::collect() |>
    dplyr::select("code", term = "description", "category") |>
    dplyr::mutate(in_input_set = .data$code %in% .env$input_unique)

  list(nodes = nodes, edges = edges)
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
