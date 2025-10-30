#' @export
add_mapping_table <- function(table, metadata) {
  validate_mapping_metadata(metadata, arg = rlang::caller_arg(metadata))

  table_name <- metadata$mapping_table_name
  if (length(table_name) != 1) {
    cli::cli_abort(
      "`metadata$mapping_table_name` must have length 1, not {length(table_name)}."
    )
  }

  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  con <- connect_to_db()
  check_database(con)

  meta_added <- add_mapping_metadata(con, metadata)
  if (!meta_added) {
    cli::cli_warn(
      c(
        "The mapping table {.field {metadata$mapping_table_name}} already exists.",
        "i" = "Use a different {.arg coding_type} or {.arg version} in {.arg metadata} to add a new mapping table."
      ),
      call = rlang::caller_env()
    )
    return(invisible(FALSE))
  }

  success <- DBI::dbWriteTable(
    con,
    name = table_name,
    value = table,
    overwrite = FALSE
  )
  if (success) {
    cli::cli_alert_success(
      "Mapping table {.field {metadata$mapping_table_name}} added successfully."
    )
  }
  return(invisible(success))
}

#' @export
mapping_metadata <- function(
  from_coding_type,
  to_coding_type,
  version = "v0",
  ...,
  from_col = "from",
  to_col = "to"
) {
  rlang::check_dots_empty()

  mapping_table_name <- paste(
    from_coding_type,
    to_coding_type,
    version,
    sep = "_"
  )

  return(list(
    mapping_table_name = mapping_table_name,
    from_coding_type = from_coding_type,
    to_coding_type = to_coding_type,
    mapping_version = version,
    from_col = from_col,
    to_col = to_col
  ))
}

add_mapping_metadata <- function(con, metadata) {
  tbl_name <- codeminer_metadata_table_names$mapping

  # Check for duplicate mapping_table_name
  ids <- metadata$mapping_table_name
  current_metadata <- get_mapping_metadata(con)
  exists <- any(ids %in% current_metadata$mapping_table_name)

  # Don't allow overwriting existing metadata
  if (exists) {
    return(invisible(FALSE))
  }

  meta_df <- as.data.frame(metadata)
  success <- DBI::dbAppendTable(con, tbl_name, meta_df)
  return(invisible(success))
}

validate_mapping_metadata <- function(
  metadata,
  arg = rlang::caller_arg(metadata),
  call = rlang::caller_env()
) {
  required <- required_mapping_metadata_columns()
  missing <- setdiff(required, names(metadata))

  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "The metadata in {.arg {arg}} is incomplete.",
        "x" = "The following entries are missing: {.field {missing}}",
        "i" = "Use {.fun codeminer::mapping_metadata} to construct valid metadata."
      ),
      call = call
    )
  }

  return(invisible(metadata))
}
