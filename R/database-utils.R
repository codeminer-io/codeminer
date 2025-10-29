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

get_lookup_metadata <- function(con = connect_to_db()) {
  tbl_name <- codeminer_metadata_table_names$lookup
  get_table_from_db(con, tbl_name)
}

get_mapping_metadata <- function(con = connect_to_db()) {
  tbl_name <- codeminer_metadata_table_names$mapping
  get_table_from_db(con, tbl_name)
}

get_table_from_db <- function(con, tbl_name) {
  DBI::dbReadTable(con, tbl_name)
}
