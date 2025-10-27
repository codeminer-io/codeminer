#' @export
create_dummy_database <- function() {
  example_tables <- utils::data("example_ontology")
  #TODO: write to db
}

#' Build the Codeminer database
#'
#' Set up the codeminer database and create the required lookup and
#' mapping metadata tables.
#'
#' @return `TRUE` invisibly if successful.
#'
#' @export
#' @examples
#' \dontrun{
#' build_database()
#' }
build_database <- function() {
  con <- connect_to_db()
  create_lookup_metadata_table(con)
  create_mapping_metadata_table(con)

  invisible(TRUE)
}

get_lookup_metadata_table <- function() {
  con <- connect_to_db()
  dplyr::tbl(con, "_metadata_lookup_tables")
}

get_mapping_metadata_table <- function() {
  con <- connect_to_db()
  dplyr::tbl(con, "_metadata_mapping_tables")
}

#' Create lookup metadata table in database
#'
#' @param con Database connection
#'
#' @return Invisible TRUE on success
#' @noRd
create_lookup_metadata_table <- function(con) {
  lookup_cols <- required_lookup_metadata_columns()
  lookup_col_definitions <- paste(
    lookup_cols,
    "VARCHAR",
    collapse = ",\n      "
  )

  relationship_cols <- required_relationship_metadata_columns()
  relationship_col_definitions <- paste(
    relationship_cols,
    "VARCHAR",
    collapse = ",\n      "
  )

  sql_statement <- paste0(
    "
    CREATE TABLE _metadata_lookup_tables (
      table_type VARCHAR,
      lookup_table_name VARCHAR,
      ",
    lookup_col_definitions,
    ",\n      ",
    relationship_col_definitions,
    "
    )
  "
  )

  DBI::dbExecute(con, sql_statement)
}

#' Create mapping metadata table in database
#'
#' @param con Database connection
#'
#' @return Invisible TRUE on success
#' @noRd
create_mapping_metadata_table <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE _metadata_mapping_tables (
      table_name VARCHAR,
      table_type VARCHAR,
      from_coding_type VARCHAR,
      to_coding_type VARCHAR,
      from_col VARCHAR,
      to_col VARCHAR
    )
  "
  )
}

#' Get required lookup metadata column names
#'
#' @return Character vector of required column names
#' @noRd
required_lookup_metadata_columns <- function() {
  c(
    "lookup_name",
    "lookup_version",
    "coding_type",
    "hierachy_type", # "lexical" (e.g. ICD10) or "relational" (e.g. SNOMED CT)
    "lookup_code_col",
    "lookup_description_col",
    "lookup_source",
    "preferred_synonym_col",
    "preferred_code"
  )
}

#' Get required relationship metadata column names
#'
#' @return Character vector of required column names
#' @noRd
required_relationship_metadata_columns <- function() {
  c(
    "relationship_name",
    "relationship_version",
    "relationship_from_col",
    "relationship_to_col",
    "relationship_type_col",
    "child_parent_relationship_code", # Code for child -> parent relationship (e.g. SNOMED 'is a')
    "relationship_source"
  )
}

connect_to_db <- function(.envir = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path())
  withr::defer(DBI::dbDisconnect(con), envir = .envir)
  return(con)
}

db_path <- function() {
  env_value <- Sys.getenv("CODEMINER_DB_PATH")
  if (env_value != "") {
    return(env_value)
  }

  base <- rappdirs::user_data_dir("codeminer")
  dir.create(base, showWarnings = FALSE) # ensure appdir exists
  return(file.path(base, "ontology.duckdb"))
}
