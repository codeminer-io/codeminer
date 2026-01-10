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

  add_ukb_resource_592(path = dummy_ukb_resource_592_path())

  codeminer_inform(c("v" = "Dummy database ready to use!"))
  return(invisible(db_path))
}

#' Get path to dummy UK Biobank Resource 592 file
#'
#' @description Returns the file path to a subset of UK Biobank Resource 592
#' (`all_lkps_maps_v4.xlsx`, [resource
#' 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) included in the
#' package for testing and examples.
#'
#' This is a **subset** of the full UKB Resource 592 file, containing a reduced
#' set of codes suitable for unit tests and documentation examples. For
#' production use, download the full file using [get_ukb_resource_592()].
#'
#' @returns Character string with the path to the dummy Excel file in the
#'   package's `extdata` directory.
#'
#' @seealso
#' * [get_ukb_resource_592()] to download the full UKB Resource 592 file
#' * [read_ukb_resource_592()] to read the file
#'
#' @export
#' @examples
#' # Get path to dummy file
#' dummy_ukb_resource_592_path()
#'
#' # Use in read_ukb_resource_592()
#' result <- read_ukb_resource_592(
#'   path = dummy_ukb_resource_592_path(),
#'   sheets = "icd10_lkp"
#' )
dummy_ukb_resource_592_path <- function() {
  system.file("extdata", "all_lkps_maps_v4.xlsx", package = "codeminer")
}


#' Get full path to the dummy SNOMED CT GPS RF2 files
#'
#' @description
#' This function returns the full path to the dummy SNOMED CT GPS dataset
#' included in the `codeminer` package for testing and examples.
#'
#' The dummy dataset is based on the SNOMED CT GPS (General Practitioner Subset)
#' release and contains a minimal set of concepts, descriptions, relationships,
#' and mappings suitable for unit tests and documentation examples.
#'
#' @section Data Source and License:
#'
#' * **Source**: SNOMED International GPS (General Practice Subset) Release
#' * **URL**: <https://www.snomed.org/gps>
#' * **License**: Creative Commons Attribution 4.0 International (CC BY 4.0)
#' * **Modifications**: This dummy dataset is a subset of the GPS release with additional
#' mock concepts, descriptions, relationships, and mappings added for testing purposes.
#' Made-up codes follow the pattern `000xxx000` (e.g., `000001000`) and made-up
#' descriptions are marked with tildes (e.g., `~description~`).
#'
#' @return A character string with the full path to the dummy SNOMED CT GPS folder.
#'
#' @seealso [read_snomed_ct_uk_monolith()] to read SNOMED CT data
#'
#' @export
#' @examples
#' dummy_snomed_ct_uk_monolith_path()
dummy_snomed_ct_uk_monolith_path <- function() {
  system.file(
    "extdata",
    "SnomedCT_GPS_PRODUCTION_20251015T120000Z",
    package = "codeminer"
  )
}
