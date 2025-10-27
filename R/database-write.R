# #' @export
# create_dummy_database <- function() {
#   example_tables <- utils::data("example_ontology")
#   #TODO: write to db
#   build_database()
# }

add_lookup_table <- function(
  table,
  coding_type,
  version = "latest",
  metadata = lookup_metadata(coding_type),
  overwrite = FALSE
) {
  table_name <- metadata$lookup_name
  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  con <- connect_to_db()

  update_lookup_metadata(con, metadata)
  DBI::dbWriteTable(
    con,
    name = table_name,
    value = table,
    overwrite = overwrite
  )
}

# TODO: add this in same Rd as add_lookup_table()
#' Create lookup metadata
#'
#' Generate the required metadata for a lookup table. This is mainly used to
#' generate the necessary metadata when adding a new lookup table to the database.
#'
#' @param coding_type The type of coding system (e.g., ICD-10, SNOMED-CT)
#' @param version The version of the lookup metadata (default: "latest")
#' @param hierarchy_type The type of hierarchy (should be one of `c("lexical", "relational")`)
#' @param lookup_code_col The column name for the lookup code (default: "code")
#' @param lookup_description_col The column name for the lookup description (default: "description")
#' @param lookup_source The source of the lookup metadata (default: `NA_character_`)
#' @param preferred_synonym_col The column name for the preferred synonym (default: `NA_character_`)
#' @param preferred_code The preferred code (default: `NA_character_`)
#'
#' @return A list containing the lookup metadata
#'
#' @export
#' @examples
#' lookup_metadata("ICD-10", version = "2023")
lookup_metadata <- function(
  coding_type,
  version = "latest",
  ...,
  hierarchy_type = c("lexical", "relational"),
  lookup_code_col = "code",
  lookup_description_col = "description",
  lookup_source = NA_character_,
  preferred_synonym_col = NA_character_,
  preferred_code = NA_character_
) {
  rlang::check_dots_empty()

  lookup_name <- paste(coding_type, version, sep = "_")
  hierarchy_type <- rlang::arg_match(hierarchy_type)

  return(list(
    lookup_name = lookup_name,
    coding_type = coding_type,
    lookup_version = version,
    hierarchy_type = hierarchy_type,
    lookup_code_col = lookup_code_col,
    lookup_description_col = lookup_description_col,
    lookup_source = lookup_source,
    preferred_synonym_col = preferred_synonym_col,
    preferred_code = preferred_code
  ))
}

# TODO: should this just be a separate table in the db?
# relationship_metadata <- function(
#   relationship_name,
#   relationship_version,
#   relationship_from_col,
#   relationship_to_col,
#   relationship_type_col,
#   child_parent_relationship_code,
#   relationship_source
# ) {
#   all_args <- as.list(match.call())[-1]
#   missing <- setdiff(required_relationship_metadata_columns(), names(all_args))

#   if (length(missing) > 0) {
#     cli::cli_abort("Missing required arguments: {missing}")
#   }

#   return(tibble::as_tibble(all_args))
# }

add_mapping_table <- function(table, metadata) {}
