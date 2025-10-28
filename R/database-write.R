# #' @export
# create_dummy_database <- function() {
#   example_tables <- utils::data("example_ontology")
#   #TODO: write to db
#   build_database()
# }

#' Add lookup table to database
#'
#' Add a lookup table to the database.
#'
#' @param table The lookup table to add, should be coercible to a `data.frame`
#' @param coding_type The type of coding system (e.g., ICD-10, SNOMED-CT)
#' @param version The version of the lookup table (default: "latest")
#' @param metadata The lookup metadata, as specified by [lookup_metadata()].
#' @param overwrite Boolean, whether to overwrite an existing table (default: `FALSE`)
#'
#' @return `TRUE` invisibly if successful
#'
#' @seealso [lookup_metadata()]
#' @export
#' @examples
#' # Using the example ontology data included in codeminer
#' lookup_table <- example_ontology$lookup_tables$capital_letters_v3
#' lookup_table
#'
#' add_lookup_table(lookup_table, "capital_letters", version = "v3")
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

#' Create lookup metadata
#'
#' Generate the required metadata for a lookup table. This is mainly used to
#' generate the necessary metadata when adding a new lookup table to the database
#' with [add_lookup_table()].
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
#' @seealso [add_lookup_table()]
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

  if (hierarchy_type == "relational") {
    cli::cli_abort("Relational hierarchy type is not supported yet.")
  }

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

add_mapping_table <- function(table, metadata) {}
