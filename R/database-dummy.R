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

  dummy_icd10 <- dummy_icd10_lookup()
  dummy_icd10_meta <- dummy_icd10_metadata()
  add_lookup_table(dummy_icd10, dummy_icd10_meta)

  cli::cli_alert_success("Dummy database ready to use!")
  return(invisible(db_path))
}

get_example_data <- function() {
  # Workaround for the R CMD check note "no visible binding for global variable 'example_ontology'"
  # and avoid loading it in the user's global environment
  example_data <- codeminer::example_ontology
  capital_lookup <- example_data$lookup_tables$capital_letters_v3
  capital_lookup_meta <- example_data$lookup_metadata |>
    dplyr::filter(.data$lookup_table_name == "capital_letters_v3")
  lowercase_lookup <- example_data$lookup_tables$lowercase_letters_v3
  lowercase_lookup_meta <- example_data$lookup_metadata |>
    dplyr::filter(.data$lookup_table_name == "lowercase_letters_v3")

  mapping_table <- example_data$mapping_tables$capital_to_lowercase_v3
  mapping_metadata <- example_data$mapping_metadata |>
    dplyr::filter(.data$mapping_table_name == "capital_to_lowercase_v3")

  return(list(
    capital_lookup_table = capital_lookup,
    capital_lookup_metadata = capital_lookup_meta,
    lowercase_lookup_table = lowercase_lookup,
    lowercase_lookup_metadata = lowercase_lookup_meta,
    mapping_table = mapping_table,
    mapping_metadata = mapping_metadata
  ))
}

# Helper to generate dummy ICD-10 Lookup data
dummy_icd10_lookup <- function() {
  raw_path <- system.file(
    "extdata",
    "dummy_all_lkps_maps_v3.xlsx",
    package = "codeminer"
  )
  icd10 <- readxl::read_excel(raw_path, sheet = "icd10_lkp")

  # Some ICD-10 descriptions include a modifier e.g. "E10" = "Type 1 diabetes
  # mellitus", whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With
  # coma" is contained in the modifier columns "MODIFIER-4". See 'S27' for an
  # example code where additional description is contained in the "MODIFER-5"
  # column. There are no codes with a modifier description in
  # both "MODIFIER_4" and "MODIFIER_5".
  icd10_clean <- icd10 |>
    dplyr::mutate(
      "DESCRIPTION" = dplyr::case_when(
        !is.na(MODIFIER_4) ~ paste(DESCRIPTION, MODIFIER_4),
        !is.na(MODIFIER_5) ~ paste(DESCRIPTION, MODIFIER_5),
        TRUE ~ DESCRIPTION
      )
    )

  # Keep only relevant columns
  icd10_lookup_dummy <- icd10_clean |>
    dplyr::select(code = "ALT_CODE", description = "DESCRIPTION")

  return(icd10_lookup_dummy)
}

# Helper to generate metadata for the dummy ICD10 lookup table
dummy_icd10_metadata <- function() {
  lookup_metadata(
    "icd10",
    version = "v0",
    lookup_code_col = "code",
    lookup_description_col = "description"
  )
}
