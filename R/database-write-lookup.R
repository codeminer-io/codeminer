#' Add a lookup table to the database
#'
#' Add a lookup table to the database together with its metadata.
#' Note that it is not possible to overwrite an existing lookup table.
#'
#' Lookup tables are indexed by their `coding_type` and `version`, specified in
#' [lookup_metadata()]. This index needs to be unique and is used to identify
#' the lookup table in the database. If a lookup table with the same
#' `coding_type` and `version` already exists, the function will emit a warning
#' and return `FALSE` (invisibly) without any effect. Use a different `version`
#' to add a new version of the lookup table for the given `coding_type`.
#'
#' @param table The lookup table to add, should be coercible to a `data.frame`
#' @param metadata The lookup metadata, as specified by [lookup_metadata()].
#'
#' @return `TRUE` invisibly if successful, `FALSE` invisibly if the lookup table
#' already exists.
#'
#' @seealso [lookup_metadata()] for the specification of the metadata.
#' @export
#' @examples
#' # Using the example ontology data included in codeminer
#' lookup_table <- example_ontology$lookup_tables$capital_letters_v3
#' lookup_table
#'
#' # Using a temporary database
#' Sys.setenv(CODEMINER_DB_PATH = tempfile())
#' build_database()
#' add_lookup_table(lookup_table, lookup_metadata("capital_letters", version = "v3"))
add_lookup_table <- function(table, metadata) {
  validate_lookup_metadata(metadata, arg = rlang::caller_arg(metadata))

  table_name <- metadata$lookup_table_name
  if (length(table_name) != 1) {
    cli::cli_abort(
      "`metadata$lookup_table_name` must have length 1, not {length(table_name)}."
    )
  }

  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  con <- connect_to_db()
  check_database(con)

  meta_added <- add_lookup_metadata(con, metadata)
  if (!meta_added) {
    cli::cli_warn(
      c(
        "The lookup table {.field {metadata$lookup_table_name}} already exists.",
        "i" = "Use a different {.arg coding_type} or {.arg version} in {.arg metadata} to add a new lookup table."
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
      "Lookup table {.field {metadata$lookup_table_name}} added successfully."
    )
  }
  return(invisible(success))
}

#' Create lookup metadata
#'
#' Generate the required metadata for a lookup table. This is mainly used to
#' generate the necessary metadata when adding a new lookup table to the database
#' with [add_lookup_table()].
#'
#' @param coding_type The type of coding system (e.g., ICD-10, SNOMED-CT)
#' @param version The version of the lookup metadata (default: "v0")
#' @inheritParams rlang::args_dots_empty
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
  lookup_code_col = "code",
  lookup_description_col = "description",
  lookup_source = NA_character_,
  preferred_synonym_col = NA_character_,
  preferred_code = NA_character_
) {
  rlang::check_dots_empty()

  lookup_table_name <- paste(coding_type, version, sep = "_")

  return(list(
    lookup_table_name = lookup_table_name,
    coding_type = coding_type,
    lookup_version = version,
    lookup_code_col = lookup_code_col,
    lookup_description_col = lookup_description_col,
    lookup_source = lookup_source,
    preferred_synonym_col = preferred_synonym_col,
    preferred_code = preferred_code
  ))
}

add_lookup_metadata <- function(con, metadata) {
  tbl_name <- codeminer_metadata_table_names$lookup

  # Check for duplicate lookup_table_name
  ids <- metadata$lookup_table_name
  current_metadata <- get_lookup_metadata(con)
  exists <- any(ids %in% current_metadata$lookup_table_name)

  # Don't allow overwriting existing metadata
  if (exists) {
    return(invisible(FALSE))
  }

  meta_df <- as.data.frame(metadata)
  success <- DBI::dbAppendTable(con, tbl_name, meta_df)
  return(invisible(success))
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
