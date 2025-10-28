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
  metadata = lookup_metadata(coding_type),
  overwrite = FALSE
) {
  table_name <- metadata$lookup_name
  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  con <- connect_to_db()
  add_lookup_metadata(con, metadata)
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
#' @param version The version of the lookup metadata (default: "v0")
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
  version = "v0",
  ...,
  hierarchy_type = c("lexical", "relational"),
  lookup_code_col = "code",
  lookup_description_col = "description",
  lookup_source = NA_character_,
  preferred_synonym_col = NA_character_,
  preferred_code = NA_character_
) {
  rlang::check_dots_empty()

  lookup_table_name <- paste(coding_type, version, sep = "_")
  hierarchy_type <- rlang::arg_match(hierarchy_type)

  if (hierarchy_type == "relational") {
    cli::cli_abort("Relational hierarchy type is not supported yet.")
  }

  return(list(
    lookup_table_name = lookup_table_name,
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

add_mapping_table <- function(table, metadata, overwrite = FALSE) {}

add_lookup_metadata <- function(con, metadata) {
  validate_lookup_metadata(metadata)
  tbl_name <- codeminer_metadata_table_names$lookup

  # Check for duplicate lookup_table_name
  ids <- metadata$lookup_table_name
  current_metadata <- get_lookup_metadata(con)
  exists <- ids[ids %in% current_metadata$lookup_table_name]

  if (length(exists) > 0) {
    cli::cli_abort(
      c(
        "Metadata for {.field {exists}} already exists.",
        "i" = "Use a different {.arg coding_type} or {.arg version}."
      ),
      call = rlang::caller_env()
    )
  }

  meta_df <- as.data.frame(metadata)
  DBI::dbAppendTable(con, tbl_name, meta_df)
}

validate_lookup_metadata <- function(
  metadata,
  arg = rlang::caller_arg(metadata),
  call = rlang::caller_env()
) {
  required <- required_lookup_metadata_columns()
  missing <- setdiff(required, names(metadata))

  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "The metadata in {.arg {arg}} is incomplete.",
        "x" = "The following entries are missing: {.field {missing}}",
        "i" = "Use {.fun codeminer::lookup_metadata} to construct valid metadata."
      ),
      call = call
    )
  }

  return(invisible(metadata))
}
