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
