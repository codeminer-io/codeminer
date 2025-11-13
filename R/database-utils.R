table_exists <- function(con, tbl_name) {
  existing_tables <- DBI::dbListTables(con)
  return(tbl_name %in% existing_tables)
}

# Helper to check if the database is valid or throw an error
check_database <- function(con) {
  error_msg <- c(
    "The database is not initialised.",
    "i" = "You may need to build the database first with {.fun codeminer::build_database}"
  )
  has_lookup_meta <- table_exists(con, codeminer_metadata_table_names$lookup)
  has_mapping_meta <- table_exists(con, codeminer_metadata_table_names$mapping)

  if (!has_lookup_meta) {
    cli::cli_abort(c(
      error_msg,
      "x" = "The lookup metadata table does not exist in the database."
    ))
  }
  if (!has_mapping_meta) {
    cli::cli_abort(c(
      error_msg,
      "x" = "The mapping metadata table does not exist in the database."
    ))
  }
  return(invisible(TRUE))
}

#' Add metadata to the database
#'
#' @param con A database connection object.
#' @param metadata A data frame containing the metadata to be added.
#' @param type The type of metadata to be added. Can be one of "lookup", "mapping", or "relationship".
#' @return A logical value indicating whether the metadata was successfully added.
#' @noRd
#' @keywords internal
add_metadata_table <- function(
  con,
  metadata,
  type = c("lookup", "mapping", "relationship")
) {
  type <- match.arg(type)
  tbl_name <- codeminer_metadata_table_names[[type]]
  stopifnot(!is.null(tbl_name)) # sanity check, not expected to ever fail

  id_col <- switch(
    type,
    lookup = "lookup_table_name",
    mapping = "mapping_table_name",
    relationship = "relationship_table_name",
    cli::cli_abort("Invalid metadata type: {type}.")
  )
  ids <- metadata[[id_col]]
  if (is.null(ids)) {
    cli::cli_abort("Missing field {id_col} in metadata.")
  }

  current_metadata <- read_table_from_db(con, tbl_name)
  exists <- any(ids %in% current_metadata[[id_col]])

  # Don't allow overwriting existing metadata
  if (exists) {
    return(invisible(FALSE))
  }

  meta_df <- as.data.frame(metadata)
  success <- DBI::dbAppendTable(con, tbl_name, meta_df)
  return(invisible(success))
}


#' Return the lookup metadata table as a data frame
#'
#' @param con A database connection object. Uses the default connection if not provided.
#' @return A data frame containing the lookup metadata.
#' @keywords internal
#' @noRd
get_lookup_metadata <- function(con = connect_to_db()) {
  tbl_name <- codeminer_metadata_table_names$lookup
  read_table_from_db(con, tbl_name)
}

#' Return the mapping metadata table as a data frame
#'
#' @param con A database connection object. Uses the default connection if not provided.
#' @return A data frame containing the mapping metadata.
#' @keywords internal
#' @noRd
get_mapping_metadata <- function(con = connect_to_db()) {
  tbl_name <- codeminer_metadata_table_names$mapping
  read_table_from_db(con, tbl_name)
}

# Helper to read a table from the database as a data.frame
read_table_from_db <- function(con, tbl_name) {
  DBI::dbReadTable(con, tbl_name)
}
