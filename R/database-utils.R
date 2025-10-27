table_exists <- function(con, tbl_name) {
  existing_tables <- DBI::dbListTables(con)
  return(tbl_name %in% existing_tables)
}
