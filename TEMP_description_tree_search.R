con <-
  check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"))
db <- ukbwranglr::db_tables_to_list(con)

# dr <- (DESCRIPTION("diab", "sct") %AND% DESCRIPTION("retin|macul", "sct")) %NOT% DESCRIPTION("absent|without", "sct")

dr <- DESCRIPTION("diabetic retinopathy", "sct")

dr_children <- CHILDREN(dr, "sct")

# dr_children_raw <- CHILDREN(dr, "sct", standardise_output = FALSE)

# Returns too many codes

# dr_plus_immediate_parent <- get_parents_sct(dr, include_self = FALSE, include_ancestors = FALSE) |>
#   dplyr::bind_rows(dr) |>
#   dplyr::distinct()
#
# dr_children_plus_immediate_parent <- CHILDREN(dr_plus_immediate_parent, "sct")

# Create parent-child trees -----------------------------------------------

rels <- db$sct_relationship |>
  dplyr::filter(sourceId %in% !!dr_children$code) |>
  dplyr::filter(destinationId %in% !!dr_children$code) |>
  dplyr::filter(typeId == "116680003") |>
  dplyr::filter(active == "1") |>
  dplyr::collect()

# now convert to tree for use with shinyTree or shinyWidgets tree. Use shinyTree
# - has search feature, try runApp(system.file("examples/06-search", package =
# "shinyTree"))

# data.tree::FromDataFrameNetwork(rels[1:50, c("sourceId", "destinationId")]) # error, needs a root

# Group by '(disorder)$' etc ----------------------------------------------

# # get top level ancestors
# rels_top <- rels |>
#   dplyr::pull(destinationId) |>
#   unique() |>
#   CODES("sct")

# result_relations <- dplyr::bind_rows(tibble::tibble(sourceId = rels_top$code,
#                                       destinationId = "ROOT"),
#                        rels[, c("sourceId", "destinationId")])

result_relations <- rels[, c("sourceId", "destinationId")]

result_descriptions <- CODES(unique(c(result_relations$sourceId, result_relations$destinationId)),
                             "sct",
                             unrecognised_codes = "warning") |>
  suppressWarnings() |>
  dplyr::select(-code_type)

# network_list <- result_descriptions %>%
#   dplyr::mutate(
#     relations = purrr::map(name, \(.x) result_relations %>%
#                              dplyr::filter(sourceId == .x) %>%
#                              dplyr::select(destinationId, same.dept, friendship, advice) %>%
#                              dplyr::group_split(dplyr::row_number())) # Convert each row to a list
#   ) %>%
#   dplyr::select(-name) %>%
#   purrr::transpose()

# hm_result <- data.tree::FromDataFrameNetwork(hm[, c("sourceId", "destinationId")])

# regex to extract "(disorder)$" / "(physical object)$" etc



## TODO try adapting this - data.tree package not required?

# dag <-
#   igraph::graph_from_data_frame(
#     d = result_df,
#     directed = TRUE,
#     vertices = result_descriptions
#   )
#
# data.tree::FromDataFrameNetwork(result_df[, c("sourceId", "destinationId")]) |>
#   data.tree::ToListSimple() |>
#   View()


# From here ---------------------------------------------------------------


## Dummy data --------------------------------------------------------------

nodes_df <- tibble::tibble(
  id = c(
    "root1",
    "root2", "root2/SubListA", "root2/SubListA/leaf1", "root2/SubListA/leaf2", "root2/SubListA/leaf3",
    "root2/SubListB", "root2/SubListB/leafA", "root2/SubListB/leafB",
    "root3", "root3/SubListA", "root3/SubListA/leaf1", "root3/SubListA/leaf2", "root3/SubListA/leaf3",
    "root3/SubListB", "root3/SubListB/leafA", "root3/SubListB/leafB"
  ),
  label = c(
    "root1",
    "root2", "SubListA", "leaf1", "leaf2", "leaf3",
    "SubListB", "leafA", "leafB",
    "root3", "SubListA", "leaf1", "leaf2", "leaf3",
    "SubListB", "leafA", "leafB"
  )
)

edges_df <- tibble::tibble(
  from = c(
    "root2", "root2/SubListA", "root2/SubListA", "root2/SubListA",
    "root2", "root2/SubListB", "root2/SubListB",
    "root3", "root3/SubListA", "root3/SubListA", "root3/SubListA",
    "root3", "root3/SubListB", "root3/SubListB"
  ),
  to = c(
    "root2/SubListA", "root2/SubListA/leaf1", "root2/SubListA/leaf2", "root2/SubListA/leaf3",
    "root2/SubListB", "root2/SubListB/leafA", "root2/SubListB/leafB",
    "root3/SubListA", "root3/SubListA/leaf1", "root3/SubListA/leaf2", "root3/SubListA/leaf3",
    "root3/SubListB", "root3/SubListB/leafA", "root3/SubListB/leafB"
  )
)

build_tree <- function(
                       edges,
                       from_col = "from",
                       to_col = "to",
                       parent = NULL) {
  children <- edges %>%
    dplyr::filter(.data[[from_col]] == !!parent) %>%
    dplyr::pull(dplyr::all_of(to_col))

  if (length(children) == 0) {
    return("")
  }

  purrr::set_names(purrr::map(
    children,
    \(.x) build_tree(
      # nodes = nodes,
      edges = edges,
      from_col = from_col,
      to_col = to_col,
      parent = .x
    )
  ),
  stringr::str_remove(children, paste0("^", parent, "/")))
}

# Identify root nodes (those not appearing in 'to' column)
root_nodes <- setdiff(nodes_df$id, edges_df$to)

# Build the tree structure
tree_list <- purrr::set_names(purrr::map(root_nodes, \(.x) build_tree(edges_df, parent = .x)), root_nodes)

# Print tree list
lobstr::tree(tree_list)


## Try again ---------------------------------------------------------------

build_tree <- function(edges, from_col = "from", to_col = "to", parent = NULL, parent_path = NULL) {
  # Determine the current full path
  current_path <- if (is.null(parent_path)) parent else paste0(parent_path, "/", parent)

  # Find children (without assuming full paths in 'to' column)
  children <- edges %>%
    dplyr::filter(.data[[from_col]] == parent) %>%
    dplyr::pull(.data[[to_col]])

  if (length(children) == 0) {
    return("")
  }

  # Recursively build the tree, passing down the full path
  purrr::set_names(
    purrr::map(children, ~ build_tree(edges, from_col, to_col, .x, parent_path = current_path)),
    children
  )
}

# Example Data:
edges_df <- tibble::tibble(
  from = c("root2", "SubListA", "SubListA", "SubListA", "root2", "SubListB", "SubListB",
           "root3", "SubListA", "SubListA", "SubListA", "root3", "SubListB", "SubListB"),
  to = c("SubListA", "leaf1", "leaf2", "leaf3", "SubListB", "leafA", "leafB",
         "SubListA", "leaf1", "leaf2", "leaf3", "SubListB", "leafA", "leafB")
)

# Identify root nodes (nodes appearing in 'from' but not in 'to')
root_nodes <- setdiff(edges_df$from, edges_df$to)

# Build the tree structure
tree_list <- purrr::set_names(purrr::map(root_nodes, ~ build_tree(edges_df, parent = .x)), root_nodes)

# Print tree
lobstr::tree(tree_list)


## Actual query ------------------------------------------------------------

result_relations_with_descriptions <- result_relations |>
  dplyr::left_join(result_descriptions,
                   by = c("sourceId" = "code")) |>
  tidyr::unite(col = "sourceId",
               dplyr::all_of(c("sourceId", "description")),
               remove = TRUE,
               na.rm = TRUE, sep = ": ") |>
  dplyr::left_join(result_descriptions,
                   by = c("destinationId" = "code")) |>
  tidyr::unite(col = "destinationId",
               dplyr::all_of(c("destinationId", "description")),
               remove = TRUE,
               na.rm = TRUE, sep = ": ") |>
  dplyr::mutate(destinationId = dplyr::case_when(destinationId == "ROOT: " ~ "ROOT",
                                                 TRUE ~ destinationId))

# Identify root nodes (those not appearing in 'to' column, note direction for sct relationship table)
dr_root_nodes <- setdiff(result_descriptions |>
                           tidyr::unite(col = "code",
                                        dplyr::everything(),
                                        remove = TRUE,
                                        na.rm = TRUE, sep = ": ") |>
                           dplyr::pull(code), result_relations_with_descriptions$sourceId)

# TODO - subcategorise by disorder, finding, situation etc

# result_relations_with_descriptions <- result_relations_with_descriptions |>
#   dplyr::mutate(category = stringr::str_extract(sourceId, "\\(([^)]+)\\)") |>
#                   stringr::str_remove_all("[()]") |>
#                   stringr::str_to_sentence()) |>
#   dplyr::mutate(destinationId = dplyr::case_when(destinationId == "ROOT" ~ category,
#                                                  TRUE ~ destinationId)) |>
#   dplyr::select(-category)

dr_tree_list <- purrr::set_names(purrr::map(
  dr_root_nodes,
  \(.x) build_tree(
    result_relations_with_descriptions,
    from_col = "destinationId",
    to_col = "sourceId",
    parent = .x
  ),
  .progress = TRUE
),
dr_root_nodes)

# Build the tree structure
dr_tree_list <- purrr::set_names(purrr::map(
  dr_root_nodes,
  ~ build_tree(
    result_relations_with_descriptions,
    from_col = "destinationId",
    to_col = "sourceId",
    parent = .x
  )
), dr_root_nodes)

lobstr::tree(dr_tree_list)

# saveRDS(dr_tree_list, file = "dr_tree_list.rds")

# TODO --------------------------------------------------------------------

# remove 'active' filter
