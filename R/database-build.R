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
build_database <- function(overwrite = FALSE) {
  db_exists <- file.exists(db_path())
  if (db_exists) {
    cli::cli_alert_info("Existing database found at {.file {db_path()}}")
  } else {
    cli::cli_alert_info("Creating new database at {.file {db_path()}}")
  }

  con <- connect_to_db()
  create_lookup_metadata_table(con, overwrite = overwrite)
  create_mapping_metadata_table(con, overwrite = overwrite)

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
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite existing table
#'
#' @return Invisible TRUE on success
#' @noRd
create_lookup_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- "lookup_metadata"
  lookup_cols <- required_lookup_metadata_columns()
  lookup_col_types <- rep("VARCHAR", length(lookup_cols))
  names(lookup_col_types) <- lookup_cols

  relationship_cols <- required_relationship_metadata_columns()
  relationship_col_types <- rep("VARCHAR", length(relationship_cols))
  names(relationship_col_types) <- relationship_cols

  create_table(
    con,
    tbl_name = tbl_name,
    fields = c(
      table_type = "VARCHAR",
      lookup_table_name = "VARCHAR",
      lookup_col_types,
      relationship_col_types
    ),
    overwrite = overwrite
  )
}

#' Create mapping metadata table in database
#'
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite existing table
#'
#' @return Invisible TRUE on success
#' @noRd
create_mapping_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- "mapping_metadata"

  create_table(
    con,
    tbl_name = tbl_name,
    fields = c(
      table_name = "VARCHAR",
      table_type = "VARCHAR",
      from_coding_type = "VARCHAR",
      to_coding_type = "VARCHAR",
      from_col = "VARCHAR",
      to_col = "VARCHAR"
    ),
    overwrite = overwrite
  )
}

create_table <- function(con, tbl_name, fields, overwrite = FALSE) {
  existing_tables <- DBI::dbListTables(con)
  tbl_exists <- tbl_name %in% existing_tables

  if (tbl_exists && overwrite) {
    cli::cli_alert_info("Dropping existing table {tbl_name}")
    DBI::dbRemoveTable(con, tbl_name)
  } else if (tbl_exists) {
    cli::cli_alert_info(
      "Table {tbl_name} already exists and `overwrite = FALSE`. Leaving as is."
    )
    return(invisible(TRUE))
  }
  DBI::dbCreateTable(
    con,
    name = tbl_name,
    fields = fields
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
