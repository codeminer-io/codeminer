#' Add a lookup table to the database
#'
#' Add a lookup table to the database together with its metadata.
#' Note that it is not possible to overwrite an existing lookup table.
#'
#' Lookup tables are indexed by their `code_type` and `version`, specified in
#' [lookup_metadata()]. This index needs to be unique and is used to identify
#' the lookup table in the database. If a lookup table with the same
#' `code_type` and `version` already exists, the function will emit a warning
#' and return `FALSE` (invisibly) without any effect. Use a different `version`
#' to add a new version of the lookup table for the given `code_type`.
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
#' add_lookup_table(
#'   lookup_table,
#'   lookup_metadata("capital_letters", lookup_version = "v3")
#' )
add_lookup_table <- function(table, metadata) {
  validate_lookup_metadata(
    metadata,
    table,
    metadata_arg = rlang::caller_arg(metadata),
    table_arg = rlang::caller_arg(table)
  )

  table_name <- metadata$lookup_table_name
  if (length(table_name) != 1) {
    codeminer_abort(
      "`metadata$lookup_table_name` must have length 1, not {length(table_name)}."
    )
  }

  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  if (!is.na(metadata$preferred_description_col)) {
    table <- make_preferred_desc_col_boolean(
      table,
      metadata$preferred_description_col,
      metadata$preferred_description_indicator
    )
  }

  con <- connect_to_db(read_only = FALSE)
  check_database(con)

  meta_added <- add_metadata_table(con, metadata, type = "lookup")
  if (!meta_added) {
    codeminer_warn(
      c(
        "!" = "The lookup table {.field {metadata$lookup_table_name}} already exists.",
        "i" = "Use a different {.arg code_type} or {.arg version} in {.arg metadata} to add a new lookup table."
      )
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
    codeminer_inform(
      c(
        "v" = "Lookup table {.field {metadata$lookup_table_name}} added successfully."
      )
    )
    cached <- .codeminer_env$active_versions[["lookup"]][[
      metadata$code_type
    ]]
    if (!is.null(cached) && cached != metadata$lookup_version) {
      codeminer_inform(c(
        "i" = paste0(
          "Currently using version {.val {cached}} for ",
          "{.val {metadata$code_type}} lookups."
        ),
        "i" = paste0(
          "Use {.fun codeminer_clear_versions} or ",
          "{.fun codeminer_set_version} to switch."
        )
      ))
    }
  }
  return(invisible(success))
}

#' Remove a lookup table from the database
#'
#' Removes a lookup table and its metadata entry from the database.
#'
#' @param code_type The coding system type (e.g. `"ICD-10"`).
#' @param lookup_version The version to remove (e.g. `"UKB v4"`).
#'
#' @return `TRUE` invisibly if successful.
#'
#' @seealso [add_lookup_table()], [lookup_metadata()]
#' @export
remove_lookup_table <- function(code_type, lookup_version) {
  table_name <- paste(code_type, lookup_version, sep = "_")

  con <- connect_to_db(read_only = FALSE)
  check_database(con)

  meta <- read_table_from_db(con, codeminer_metadata_table_names$lookup)
  if (!table_name %in% meta$lookup_table_name) {
    codeminer_abort(
      "No lookup table found for {.val {code_type}} version {.val {lookup_version}}."
    )
  }

  remove_table_entry(con, "lookup", table_name)
  cached <- .codeminer_env$active_versions[["lookup"]][[code_type]]
  if (!is.null(cached) && cached == lookup_version) {
    .codeminer_env$active_versions[["lookup"]][[code_type]] <- NULL
  }
  codeminer_inform(c(
    "v" = "Lookup table {.field {table_name}} removed."
  ))
  invisible(TRUE)
}

#' Update lookup table metadata
#'
#' Updates metadata fields for an existing lookup table without re-adding the
#' data. Currently supports updating `col_filters`.
#'
#' @param code_type The coding system type (e.g. `"SNOMED CT"`).
#' @param lookup_version The version to update. Use `"latest"` (default) to
#'   update the most recent version.
#' @inheritParams rlang::args_dots_empty
#' @param col_filters Column filter specification to set. See
#'   [lookup_metadata()] for the format. Use `NULL` to clear existing filters.
#'
#' @return `TRUE` invisibly if successful.
#' @export
#' @family Database management
#' @seealso [lookup_metadata()], [add_lookup_table()]
update_lookup_metadata <- function(
  code_type,
  lookup_version = "latest",
  ...,
  col_filters = NULL
) {
  rlang::check_dots_empty()

  con <- connect_to_db(read_only = FALSE)
  check_database(con)

  # Resolve version
  meta <- read_table_from_db(con, codeminer_metadata_table_names$lookup)
  resolved <- resolve_versioned_metadata(
    meta,
    code_type_val = code_type,
    version_val = lookup_version,
    version_col = "lookup_version",
    pin_type = "lookup",
    type_label = "lookup",
    add_fun_name = "codeminer::add_lookup_table"
  )
  table_name <- resolved$lookup_table_name

  # Validate col_filters columns exist in the data table
  cf_json <- serialise_col_filters(col_filters)
  if (!is.na(cf_json)) {
    table_cols <- DBI::dbListFields(con, table_name)
    validate_col_filters_columns(
      col_filters,
      table_cols = table_cols,
      table_name = table_name
    )
  }

  # Update metadata row
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "UPDATE {`codeminer_metadata_table_names$lookup`}
       SET col_filters = {cf_json}
       WHERE lookup_table_name = {table_name}",
      .con = con
    )
  )

  codeminer_inform(c(
    "v" = "Updated metadata for lookup table {.field {table_name}}."
  ))
  invisible(TRUE)
}

#' Create lookup metadata
#'
#' Generate the required metadata for a lookup table. This is mainly used to
#' generate the necessary metadata when adding a new lookup table to the database
#' with [add_lookup_table()].
#'
#' @param code_type The type of coding system (e.g., ICD-10, SNOMED-CT)
#' @param lookup_version The version of the lookup metadata (default: "v0")
#' @inheritParams rlang::args_dots_empty
#' @param lookup_code_col The column name for the lookup code (default: "code")
#' @param lookup_description_col The column name for the lookup description (default: "description")
#' @param lookup_category_col The column name carrying a per-code semantic
#'   category (e.g. ICD-10 chapter, SNOMED FSN class, BNF chapter). Surfaced as
#'   `category` by [get_lookup_table()] for use by hierarchy / tree-rendering
#'   tooling. Default `NA_character_`, in which case the `category` column is
#'   populated with `NA`.
#' @param lookup_source The source of the lookup metadata (default: `NA_character_`)
#' @param preferred_description_col The name of the column that indicates the
#'   preferred description. This is useful for lookup tables that have multiple
#'   descriptions for the same code (default: `NA_character_`).
#' @param preferred_description_indicator The value in the
#'   `preferred_description_col` column that indicates the preferred description
#'   (default: `NA_character_`)
#' @param col_filters Optional column filter specification. A named list where
#'   each element is a list with `values` (all valid values) and `defaults`
#'   (default filter values). See **Details** for the format. `NULL` (default)
#'   means no column filters.
#'
#' @details
#' The `col_filters` argument specifies which columns in the lookup table are
#' filterable and what the default filter values are. The format is:
#'
#' ```
#' list(
#'   column_name = list(values = c("val1", "val2"), defaults = c("val1"))
#' )
#' ```
#'
#' When `col_filters` is set, query functions like [CODES()] will automatically
#' filter the lookup table to only include rows matching the default values.
#'
#' @return A list containing the lookup metadata
#'
#' @seealso [add_lookup_table()]
#' @export
#' @examples
#' lookup_metadata("ICD-10", lookup_version = "2023")
lookup_metadata <- function(
  code_type,
  lookup_version = "v0",
  ...,

  lookup_code_col = "code",
  lookup_description_col = "description",
  lookup_category_col = NA_character_,
  lookup_source = NA_character_,
  preferred_description_col = NA_character_,
  preferred_description_indicator = NA_character_,
  col_filters = NULL
) {
  rlang::check_dots_empty()

  lookup_table_name <- paste(code_type, lookup_version, sep = "_")

  if (
    !is.na(preferred_description_col) &&
      is.na(preferred_description_indicator)
  ) {
    codeminer_abort(
      "{.arg preferred_description_indicator} must be provided if {.arg preferred_description_col} is not `NA`"
    )
  }

  return(list(
    lookup_table_name = lookup_table_name,
    code_type = code_type,
    lookup_version = lookup_version,
    lookup_code_col = lookup_code_col,
    lookup_description_col = lookup_description_col,
    lookup_category_col = lookup_category_col,
    lookup_source = lookup_source,
    preferred_description_col = preferred_description_col,
    preferred_description_indicator = preferred_description_indicator,
    col_filters = serialise_col_filters(col_filters)
  ))
}

#' Validate lookup metadata
#'
#' Checks that required metadata is present, and that any column names are
#' actually present in the accompanying lookup table.
#'
#' @param metadata A list containing the lookup metadata.
#' @param table The lookup table to add, should be coercible to a `data.frame`
#' @param metadata_arg The metadata argument name. Used to construct error message.
#' @param table_arg The table argument name. Used to construct error message.
#' @param call The calling environment. Used to construct error message.
#' @return A logical value, invisibly, indicating whether the metadata is valid.
#' @keywords internal
#' @noRd
validate_lookup_metadata <- function(
  metadata,
  table,
  metadata_arg = rlang::caller_arg(metadata),
  table_arg = rlang::caller_arg(table),
  call = rlang::caller_env()
) {
  required <- required_lookup_metadata_columns()
  missing <- setdiff(required, names(metadata))

  if (length(missing) > 0) {
    codeminer_abort(
      c(
        "The metadata in {.arg {metadata_arg}} is incomplete.",
        "x" = "The following entries are missing: {.field {missing}}",
        "i" = "Use {.fun codeminer::lookup_metadata} to construct valid metadata."
      ),
      call = call
    )
  }

  missing_colnames <- metadata[stringr::str_ends(names(metadata), "_col")] |>
    purrr::keep(\(x) rlang::is_string(x) && !x %in% names(table))

  if (length(missing_colnames) > 0) {
    codeminer_abort(
      c(
        "Invalid metadata supplied in {.arg {metadata_arg}}.",
        "x" = "These metadata fields refer to columns not present in {.arg {table_arg}}:",
        "!" = "{.field {names(missing_colnames)}} -> {.val {unlist(missing_colnames)}}"
      ),
      call = call
    )
  }

  # Validate col_filters column names exist in the data table
  cf <- deserialise_col_filters(metadata$col_filters)
  validate_col_filters_columns(
    cf,
    table_cols = names(table),
    table_name = metadata$lookup_table_name,
    call = call
  )

  return(invisible(metadata))
}

make_preferred_desc_col_boolean <- function(table, col_name, indicator) {
  is_preferred <- !is.na(table[[col_name]]) & table[[col_name]] == indicator
  table[[col_name]] <- is_preferred
  return(table)
}
