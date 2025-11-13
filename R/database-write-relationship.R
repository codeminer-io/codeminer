add_relationship_table <- function(table, metadata) {}

relationship_metadata <- function(
  code_type,
  version = "v0",
  ...,
  from_col = "from",
  to_col = "to",
  type_col = "type",
  child_parent_relationship_code = "is a",
  relationship_source = NA_character_
) {
  rlang::check_dots_empty()

  relationship_table_name <- paste(code_type, version, sep = "_")
  return(list(
    relationship_table_name = relationship_table_name,
    code_type = code_type,
    relationship_version = version,
    from_col = from_col,
    to_col = to_col,
    type_col = type_col,
    child_parent_relationship_code = child_parent_relationship_code,
    relationship_source = relationship_source
  ))
}

add_relationship_metadata <- function(con, metadata) {
  tbl_name <- codeminer_metadata_table_names$lookup

  # Check for duplicate relationship_table_name
  ids <- metadata$relationship_table_name
  current_metadata <- get_relationship_metadata(con)
  exists <- any(ids %in% current_metadata$relationship_table_name)

  # Don't allow overwriting existing metadata
  if (exists) {
    return(invisible(FALSE))
  }

  meta_df <- as.data.frame(metadata)
  success <- DBI::dbAppendTable(con, tbl_name, meta_df)
  return(invisible(success))
}
