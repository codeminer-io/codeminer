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

  add_lookup_table(dummy_icd10_lookup(), dummy_icd10_metadata())
  add_relationship_table(
    dummy_icd10_relationship(),
    dummy_icd10_relationship_metadata()
  )

  add_lookup_table(dummy_read3_lookup(), dummy_read3_metadata())

  add_mapping_table(
    dummy_read3_icd10_mapping(),
    dummy_read3_icd10_mapping_metadata()
  )

  cli::cli_alert_success("Dummy database ready to use!")
  return(invisible(db_path))
}

dummy_data_path <- function() {
  system.file("extdata", "dummy_all_lkps_maps_v3.xlsx", package = "codeminer")
}

# Helper to generate dummy ICD-10 Lookup data
dummy_icd10_lookup <- function() {
  icd10 <- readxl::read_excel(dummy_data_path(), sheet = "icd10_lkp")

  # Remove garbage Excel rows
  icd10 <- dplyr::filter(icd10, !is.na(.data$ALT_CODE))

  # Some ICD-10 descriptions include a modifier e.g. "E10" = "Type 1 diabetes
  # mellitus", whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With
  # coma" is contained in the modifier columns "MODIFIER-4". See 'S27' for an
  # example code where additional description is contained in the "MODIFER-5"
  # column. There are no codes with a modifier description in
  # both "MODIFIER_4" and "MODIFIER_5".
  icd10_clean <- icd10 |>
    dplyr::mutate(
      "DESCRIPTION" = dplyr::case_when(
        !is.na(.data$MODIFIER_4) ~ paste(.data$DESCRIPTION, .data$MODIFIER_4),
        !is.na(.data$MODIFIER_5) ~ paste(.data$DESCRIPTION, .data$MODIFIER_5),
        TRUE ~ .data$DESCRIPTION
      )
    ) |>
    dplyr::select(!dplyr::all_of(c("MODIFIER_4", "MODIFIER_5")))

  # Rename relevant columns
  icd10_lookup_dummy <- dplyr::select(
    icd10_clean,
    code = "ALT_CODE",
    description = "DESCRIPTION",
    dplyr::everything()
  )

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

# Helper to generate icd10 relationship data
# icd10 child relationships are encoded directly into the ICD10_CODE
# If a code is of the form `<prefix>.<suffix>` then the parent is the `<prefix>` code
# E.g. `A00.1` is a child of `A00`.
dummy_icd10_relationship <- function() {
  lkp_tbl <- dummy_icd10_lookup()
  icd10_codes <- lkp_tbl$code
  children <- purrr::map(icd10_codes, \(x) {
    # Don't consider code itself as child
    candidates <- icd10_codes[icd10_codes != x]
    is_child <- stringr::str_starts(candidates, x)
    return(candidates[is_child])
  }) |>
    rlang::set_names(icd10_codes)
  # Remove empty entries
  children[lengths(children) == 0] <- NULL
  tbl <- utils::stack(children)
  names(tbl) <- c("from", "to")
  tbl$type <- "is a"
  return(tbl)
}

dummy_icd10_relationship_metadata <- function() {
  relationship_metadata(
    code_type = "icd10",
    version = "v0",
    from_col = "from",
    to_col = "to",
    type_col = "type",
    child_parent_relationship_code = "is a"
  )
}

dummy_read3_lookup <- function() {
  read3 <- readxl::read_excel(dummy_data_path(), sheet = "read_ctv3_lkp")
  read3_lookup <- dplyr::rename(
    read3,
    code = "read_code",
    description = "term_description"
  )
  # Remove NA rows
  read3_lookup <- dplyr::filter(
    read3_lookup,
    !is.na(.data$code),
    !is.na(.data$description)
  )
  return(read3_lookup)
}

dummy_read3_metadata <- function() {
  lookup_metadata(
    "read3",
    version = "v0",
    lookup_code_col = "code",
    lookup_description_col = "description",
    preferred_description_col = "description_type",
    preferred_description_indicator = "P"
  )
}

dummy_read3_icd10_mapping <- function() {
  read3_icd10_raw <- readxl::read_excel(
    dummy_data_path(),
    sheet = "read_ctv3_icd10"
  )

  mapping <- dplyr::rename(
    read3_icd10_raw,
    from = "read_code",
    to = "icd10_code"
  )
  mapping_clean <- dplyr::filter(mapping, !is.na(.data$from), !is.na(.data$to))

  # Keep only codes for which we have lookup data
  read3_lookup <- dummy_read3_lookup()
  icd10_lookup <- dummy_icd10_lookup()
  mapping_clean <- dplyr::filter(
    mapping_clean,
    .data$from %in% read3_lookup$code,
    .data$to %in% icd10_lookup$code
  )

  return(mapping_clean)
}

dummy_read3_icd10_mapping_metadata <- function() {
  mapping_metadata(
    "read3",
    "icd10",
    version = "v0",
    from_col = "from",
    to_col = "to"
  )
}
