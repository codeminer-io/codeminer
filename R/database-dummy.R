#' Create a dummy database
#'
#' Sets up an example database for codeminer with dummy data
#' and sets the environment variable `CODEMINER_DB_PATH`.
#' Any subsequent `codeminer` actions will use this database.
#'
#' @param db_path Path to the database file. Defaults to a temporary file.
#'   This is to avoid writing the dummy data to an already existing database.
#' @inheritParams rlang::args_dots_empty
#' @param .envir Environment in which to set the `CODEMINER_DB_PATH` variable.
#'   Defaults to the calling environment.
#'
#' @return The path to the created database file, invisibly.
#'
#' @export
#' @examples
#' # Create dummy database in a temporary location
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # This also sets the environment variable `CODEMINER_DB_PATH`
#' Sys.getenv("CODEMINER_DB_PATH")
create_dummy_database <- function(
  db_path = tempfile(fileext = ".duckdb"),
  ...,
  .envir = parent.frame()
) {
  rlang::check_dots_empty()

  withr::local_envvar(
    list("CODEMINER_DB_PATH" = db_path),
    .local_envir = .envir
  )
  build_database(overwrite = TRUE)

  example_data <- get_example_data()
  add_lookup_table(example_data$lookup_table, example_data$lookup_metadata)
  add_mapping_table(example_data$mapping_table, example_data$mapping_metadata)

  cli::cli_alert_success("Dummy database ready to use!")
  return(invisible(db_path))
}

get_example_data <- function() {
  # Workaround for the R CMD check note "no visible binding for global variable 'example_ontology'"
  # and avoid loading it in the user's global environment
  example_data <- codeminer::example_ontology
  lookup_table <- example_data$lookup_tables$capital_letters_v3
  lookup_metadata <- example_data$lookup_metadata |>
    dplyr::filter(.data$lookup_table_name == "capital_letters_v3")

  mapping_table <- example_data$mapping_tables$capital_to_lowercase_v3
  mapping_metadata <- example_data$mapping_metadata |>
    dplyr::filter(.data$mapping_table_name == "capital_to_lowercase_v3")

  return(list(
    lookup_table = lookup_table,
    lookup_metadata = lookup_metadata,
    mapping_table = mapping_table,
    mapping_metadata = mapping_metadata
  ))
}
