#' Create a dummy database
#'
#' Sets up an example database for codeminer with dummy data
#'
#' @export
#' @examples
#' # Create dummy database in a temporary location
#' temp_db <- tempfile(fileext = ".duckdb")
#' Sys.setenv(CODEMINER_DB_PATH = temp_db)
#' create_dummy_database()
create_dummy_database <- function() {
  # Workaround for the R CMD check note "no visible binding for global variable 'example_ontology'"
  # and avoid loading it in the user's global environment
  example_data <- codeminer::example_ontology
  lookup_table <- example_data$lookup_tables$capital_letters_v3
  lookup_metadata <- example_data$lookup_metadata |>
    dplyr::filter(.data$lookup_table_name == "capital_letters_v3")

  build_database()
  add_lookup_table(lookup_table, lookup_metadata)
}

