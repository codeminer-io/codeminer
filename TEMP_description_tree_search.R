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

# Group by '(disorder)$' etc ----------------------------------------------

result_relations <- rels[, c("sourceId", "destinationId")]

result_descriptions <- CODES(unique(c(result_relations$sourceId, result_relations$destinationId)),
                             "sct",
                             unrecognised_codes = "warning") |>
  suppressWarnings() |>
  dplyr::select(-code_type)


# From here ---------------------------------------------------------------




## Try again ---------------------------------------------------------------

build_tree <- function(edges, from_col = "from", to_col = "to", parent = NULL) {
  # Find children (without assuming full paths in 'to' column)
  children <- edges %>%
    dplyr::filter(.data[[from_col]] == parent) %>%
    dplyr::pull(.data[[to_col]])

  if (length(children) == 0) {
    return("")
  }

  # Recursively build the tree, passing down the full path
  purrr::set_names(
    purrr::map(children, ~ build_tree(edges, from_col, to_col, .x)),
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

# get top level ancestors
rels_top <- rels |>
  dplyr::filter(!destinationId %in% sourceId) |>
  dplyr::pull(destinationId) |>
  unique() |>
  CODES("sct") |>
  dplyr::select(-code_type) |>
  tidyr::unite(col = "code",
               dplyr::everything(),
               remove = TRUE,
               na.rm = TRUE, sep = ": ") |>
  dplyr::mutate(category = stringr::str_extract(code, "\\(([^)]+)\\)") |>
                  stringr::str_remove_all("[()]") |>
                  stringr::str_to_sentence()) |>
  dplyr::rename(sourceId = code,
                destinationId = category)

# TODO - check for nodes not in relation table, and check length(unique) codes in relation table == nrow(result_description)
result_descriptions |>
  tidyr::unite(col = "code",
               dplyr::everything(),
               remove = TRUE,
               na.rm = TRUE, sep = ": ") |>
  dplyr::mutate(category = stringr::str_extract(code, "\\(([^)]+)\\)") |>
                  stringr::str_remove_all("[()]") |>
                  stringr::str_to_sentence()) |>
  View()


result_relations_with_descriptions <- dplyr::bind_rows(rels_top, result_relations_with_descriptions)

# Identify root nodes (those not appearing in 'to' column, note direction for sct relationship table)
dr_root_nodes <- setdiff(result_relations_with_descriptions$destinationId, result_relations_with_descriptions$sourceId)

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
