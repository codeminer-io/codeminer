# Tests to write

# All query concepts are in final relationship table

# Inactive concepts are categorised as 'Inactive'

# Query -------------------------------------------------------------------

# dr <- (DESCRIPTION("diab", "sct") %AND% DESCRIPTION("retin|macul", "sct")) %NOT% DESCRIPTION("absent|without", "sct")

query_result <- DESCRIPTION("diabetic retinopathy", "sct")

# get all child codes for query results
query_result_children <- CHILDREN(query_result, "sct")

query_result_children_inactive_codes <- get_sct_inactive_codes(query_result_children$code) |>
  unite_code_with_description(new_col = "code",
                              description_col = "description",
                              code_col = "code") |>
  dplyr::select(-code_type)

# subset relationships table for query results and their children
result_relations <- filter_sct_relationship(codes = NULL,
                        sourceId_filter = query_result_children$code,
                        destinationId_filter = query_result_children$code,
                        typeId_filter = "116680003",
                        active_only = TRUE,
                        recursive = FALSE,
                        all_lkps_maps = NULL) |>
  dplyr::select(dplyr::all_of(c("sourceId", "destinationId"))) |>
  dplyr::distinct()

# ensure top level nodes are all included in query results (otherwise may add to
# duplicated codes in final tree e.g. a search for 'diabetic retinopathy'
# returns "4855003 << Retinopathy due to diabetes mellitus (disorder) >>", which
# has 2 parents, both of which at this stage would be included in the subsetted
# relationships table, meaning "4855003" and all its children will appear twice
# in the tree, once under each of the 2 parents)
result_relations_top_nodes_to_remove <- result_relations |>
  dplyr::filter(!destinationId %in% sourceId) |>
  dplyr::filter(!destinationId %in% !!query_result_children$code)

result_relations <- result_relations |>
  dplyr::anti_join(result_relations_top_nodes_to_remove,
                   by = dplyr::join_by("sourceId", "destinationId"))

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

# lobstr::tree(tree_list)

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


build_tree <- function(edges,
                       from_col = "from",
                       to_col = "to",
                       parent = NULL,
                       selected_values = character()) {
  # Find children (without assuming full paths in 'to' column)
  children <- edges %>%
    dplyr::filter(.data[[from_col]] == parent) %>%
    dplyr::pull(.data[[to_col]])

  if (length(children) == 0) {
    return("")
  }

  # Recursively build the tree
  tree <- purrr::set_names(purrr::map(
    children,
    \(child) build_tree(edges, from_col, to_col, child, selected_values)
  ), children)

  # Set attributes for selected values. See https://stackoverflow.com/a/56140793
  for (child in children) {
    if (child %in% selected_values) {
      attr(tree[[child]], "stselected") <- TRUE
    }
  }

  # Mark the parent node as opened
  attr(tree, "stopened") <- TRUE

  return(tree)
}
