# Tests to write

# All query concepts are in final relationship table

# Inactive concepts are categorised as 'Inactive'

con <-
  check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"))
db <- ukbwranglr::db_tables_to_list(con)

# Query -------------------------------------------------------------------

# dr <- (DESCRIPTION("diab", "sct") %AND% DESCRIPTION("retin|macul", "sct")) %NOT% DESCRIPTION("absent|without", "sct")

query_result <- DESCRIPTION("diabetic retinopathy", "sct")

# get all child codes for query results
query_result_children <- CHILDREN(query_result, "sct")

# determine which are inactive codes (these will have lost parent-child
# relations)

# query_result_inactive_codes <- get_sct_inactive_codes(query_result$code) |>
#   unite_code_with_description(new_col = "code",
#                               description_col = "description",
#                               code_col = "code") |>
#   dplyr::select(-code_type)

query_result_children_inactive_codes <- get_sct_inactive_codes(query_result_children$code) |>
  unite_code_with_description(new_col = "code",
                              description_col = "description",
                              code_col = "code") |>
  dplyr::select(-code_type)

# Returns too many codes

# dr_plus_immediate_parent <- get_parents_sct(dr, include_self = FALSE, include_ancestors = FALSE) |>
#   dplyr::bind_rows(dr) |>
#   dplyr::distinct()
#
# dr_children_plus_immediate_parent <- CHILDREN(dr_plus_immediate_parent, "sct")

# rels <- db$sct_relationship |>
#   dplyr::filter(sourceId %in% !!query_result_children$code) |>
#   dplyr::filter(destinationId %in% !!query_result_children$code) |>
#   dplyr::filter(typeId == "116680003") |>
#   dplyr::select(dplyr::all_of(c("sourceId", "destinationId"))) |>
#   dplyr::collect()

# subset relationships table for query results and their children
result_relations <- db$sct_relationship |>
  dplyr::filter(sourceId %in% !!query_result_children$code |
                  destinationId %in% !!query_result_children$code) |>
  dplyr::filter(typeId == "116680003") |>
  dplyr::select(dplyr::all_of(c("sourceId", "destinationId"))) |>
  dplyr::distinct() |>
  dplyr::collect()

# append descriptions to all codes
result_descriptions <- CODES(unique(c(result_relations$sourceId, result_relations$destinationId)),
                             "sct",
                             unrecognised_codes = "warning") |>
  suppressWarnings() |>
  dplyr::bind_rows(query_result_children) |>
  dplyr::distinct() |>
  dplyr::mutate(selected = dplyr::case_when(code %in% !!query_result$code ~ TRUE,
                                            TRUE ~ FALSE)) |>
  dplyr::select(-code_type) |>
  unite_code_with_description(new_col = "description",
                              description_col = "description",
                              code_col = "code",
                              remove = FALSE) |>
  update_description_col(description_col = "description",
                         appended_text = "INACTIVE",
                         values_to_update = query_result_children_inactive_codes$code)

result_relations_with_descriptions <- result_relations |>
  dplyr::left_join(result_descriptions, by = c("sourceId" = "code")) |>
  dplyr::select(sourceId = description, destinationId) |>
  dplyr::left_join(result_descriptions, by = c("destinationId" = "code")) |>
  dplyr::select(sourceId, destinationId = description)

# check all codes in relationships table are included in descriptions table
stopifnot(all(
  unique(
    result_relations_with_descriptions$sourceId,
    result_relations_with_descriptions$destinationId
  )
  %in% result_descriptions$description
))

# stopifnot(all(
#   result_descriptions$description %in% unique(
#     result_relations_with_descriptions$sourceId,
#     result_relations_with_descriptions$destinationId
#   )
# ))

# subset(
#   result_descriptions$description, !result_descriptions$description %in% unique(
#     result_relations_with_descriptions$sourceId,
#     result_relations_with_descriptions$destinationId
#   )
# )

# get top level ancestors and categorise
rels_top <- result_relations_with_descriptions |>
  dplyr::filter(!destinationId %in% sourceId) |>
  dplyr::select(sourceId = destinationId) |>
  dplyr::distinct() |>
  mutate_category_from_sct_description(new_col = "destinationId",
                                       description_col = "sourceId")


# get codes from descriptions table that are not in relationships table (e.g. inactive codes)
descriptions_to_append_to_relationships <- result_descriptions |>
  dplyr::filter((
    !description %in% !!result_relations_with_descriptions$sourceId
  ) &
    (
      !description %in% !!result_relations_with_descriptions$destinationId
    )
  ) |>
  dplyr::select(sourceId = description) |>
  mutate_category_from_sct_description(new_col = "destinationId",
                                       description_col = "sourceId")

result_relations_with_descriptions <- list(descriptions_to_append_to_relationships,
                                           rels_top,
                                           result_relations_with_descriptions) |>
  dplyr::bind_rows()

# Identify root nodes (those not appearing in 'to' column, note direction for sct relationship table)
root_nodes <- setdiff(result_relations_with_descriptions$destinationId, result_relations_with_descriptions$sourceId)

# arrange so inactive categories are last

# Identify elements containing "inactive"
inactive_nodes <- root_nodes[grepl("inactive", root_nodes, ignore.case = TRUE)]
active_nodes <- root_nodes[!grepl("inactive", root_nodes, ignore.case = TRUE)]

# Concatenate with "inactive" terms at the end
sorted_root_nodes <- c(sort(active_nodes), sort(inactive_nodes))

# Build the tree structure
tree_list <- purrr::set_names(purrr::map(
  root_nodes,
  ~ build_tree(
    result_relations_with_descriptions,
    from_col = "destinationId",
    to_col = "sourceId",
    parent = .x,
    selected_values = result_descriptions[result_descriptions$selected, ]$description
  ),
  .progress = TRUE
),
root_nodes)

lobstr::tree(tree_list)

# saveRDS(tree_list, file = "tree_list.rds")

# Helper functions --------------------------------------------------------------------

unite_code_with_description <- function(.df,
                                        new_col,
                                        code_col,
                                        description_col,
                                        remove = TRUE,
                                        na.rm = TRUE) {
  .df |>
    tidyr::unite(
      col = !!new_col,
      dplyr::all_of(c(code_col, description_col)),
      remove = remove,
      na.rm = na.rm,
      sep = ": "
    )
}

update_description_col <- function(.df,
                                   description_col,
                                   values_to_update,
                                   appended_text = "INACTIVE") {
  .df |>
    dplyr::mutate(
      !!description_col := dplyr::case_when(
        .data[[description_col]] %in% !!values_to_update &
          stringr::str_detect(.data[[description_col]], "\\(([^)]+)\\)$", negate = FALSE) ~ stringr::str_replace(
            .data[[description_col]],
            pattern = "\\)$",
            replacement = paste0(" ", appended_text, ")")
          ),
        .data[[description_col]] %in% !!values_to_update &
          stringr::str_detect(.data[[description_col]], "\\(([^)]+)\\)$", negate = TRUE) ~ paste0(.data[[description_col]], " (", appended_text, ")"),
        TRUE ~ .data[[description_col]]
      )
    )
}

mutate_category_from_sct_description <- function(.df, new_col, description_col) {
  .df |>
    dplyr::mutate(
      !!new_col := stringr::str_extract(.data[[description_col]], "\\([^()]+\\)$") |>
        stringr::str_remove_all("[()]") |>
        stringr::str_to_sentence()
    )
}

get_sct_inactive_codes <- function(sct_codes) {
  CODES(
    sct_codes,
    code_type = 'sct',
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = 'warning',
    col_filters = list(
      sct_description = list(active_concept = '0', active_description = '1')
    )
  ) |>
    suppressWarnings()
}

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
    purrr::map(children, \(.x) build_tree(edges, from_col, to_col, .x)),
    children
  )
}

build_tree <- function(edges, from_col = "from", to_col = "to", parent = NULL, selected_values = character()) {
  # Find children (without assuming full paths in 'to' column)
  children <- edges %>%
    dplyr::filter(.data[[from_col]] == parent) %>%
    dplyr::pull(.data[[to_col]])

  if (length(children) == 0) {
    return("")
  }

  # Recursively build the tree
  tree <- purrr::set_names(
    purrr::map(children, \(child) build_tree(edges, from_col, to_col, child, selected_values)),
    children
  )

  # Set attributes for selected values
  for (child in children) {
    if (child %in% selected_values) {
      attr(tree[[child]], "stselected") <- TRUE
    }
  }

  # Mark the parent node as opened
  attr(tree, "stopened") <- TRUE

  return(tree)
}

