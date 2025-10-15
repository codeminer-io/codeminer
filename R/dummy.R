# PUBLIC ------------------------------------------------------------------

## all_lkps_maps ------------

#' Dummy UK Biobank codings file path
#'
#' Returns the file path to a dummy [UK Biobank
#' codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
#' tsv file.
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_ukb_codings_path()
dummy_ukb_codings_path <- function() {
  system.file("extdata", "dummy_Codings.tsv", package = "codemapper")
}

#' Dummy UK Biobank resource 592 file path
#'
#' Returns the file path to a dummy [UK Biobank resource
#' 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) excel spreadsheet.
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_all_lkps_maps_path()
dummy_all_lkps_maps_path <- function() {
  system.file("extdata", "dummy_all_lkps_maps_v3.xlsx", package = "codemapper")
}

#' Read dummy UK Biobank codings into R
#'
#' Reads a dummy [UK Biobank
#' codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
#' tsv file into R.
#'
#' @return A data frame.
#' @export
#' @family Dummy data
#' @examples
#' read_ukb_codings_dummy()
read_ukb_codings_dummy <- function() {
  readr::read_tsv(
    dummy_ukb_codings_path(),
    progress = FALSE,
    col_types = readr::cols(.default = "c")
  )
}

#' Read dummy UK Biobank resource 592 into R
#'
#' Reads a dummy [UK Biobank resource
#' 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) excel spreadsheet
#' into R.
#'
#' @return A named list of tibbles.
#' @export
#' @family Dummy data
#' @examples
#' read_all_lkps_maps_dummy()
read_all_lkps_maps_dummy <- function() {
  read_all_lkps_maps(path = dummy_all_lkps_maps_path())
}

#' Create a dummy all_lkps_maps
#'
#' A thin convenience wrapper around [build_all_lkps_maps()], using dummy data
#' included with this package.
#'
#' @return A named list of tibbles.
#' @export
#'
#' @family Dummy data
#' @seealso [build_all_lkps_maps()]
#' @examples
#' build_all_lkps_maps_dummy()
build_all_lkps_maps_dummy <- function() {
  suppressMessages(build_all_lkps_maps(
    all_lkps_maps = read_all_lkps_maps_dummy(),
    ukb_codings = read_ukb_codings_dummy(),
    bnf_lkp = NULL,
    bnf_dmd = NULL,
    self_report_med_to_atc_map = NULL,
    snomed_ct_uk_monolith = NULL,
    phecode_1_2_lkp = dummy_phecode_lkp_path(),
    icd10_phecode_1_2 = dummy_icd10_phecode_map_path(),
    icd9_phecode_1_2 = NULL
  ))
}

#' Set up a dummy all_lkps_maps database
#'
#' @param db_path Path to the database file. Defaults to a temporary file.
#' @return Returns the \code{db_path} invisibly
#' @export
#' @family Dummy data
#' @examples
#' dummy_all_lkps_maps_db()
dummy_all_lkps_maps_db <- function(db_path = tempfile(fileext = ".db")) {
  maps <- build_all_lkps_maps_dummy()
  all_lkps_maps_to_db(all_lkps_maps = maps, db_path = db_path)
}

## CALIBER -----------------------------------------------------------------

#' Dummy CALIBER repository
#'
#' Returns the file path to a dummy data
#' [CALIBER](https://github.com/spiros/chronological-map-phenotypes) repository.
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_caliber_dir_path()
dummy_caliber_dir_path <- function() {
  system.file("extdata", "test_caliber_repo", package = "codemapper")
}


## Phecodes ----------------------------------------------------------------

#' Dummy Phecode definitions file path
#'
#' Returns the file path to a dummy Phecode definitions 1.2 csv file (full
#' version may be downloaded from
#' [phewascatalog.org](https://phewascatalog.org/phecodes_icd10)).
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_phecode_lkp_path()
dummy_phecode_lkp_path <- function() {
  system.file(
    "extdata",
    "dummy_phecode_definitions1.2.csv",
    package = "codemapper"
  )
}

#' Dummy Phecode Map 1.2 with ICD-10 codes (beta) file path
#'
#' Returns the file path to a dummy Phecode Map 1.2 with ICD-10 codes (beta) csv
#' file (full version may be downloaded from
#' [phewascatalog.org](https://phewascatalog.org/phecodes_icd10)).
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_icd10_phecode_map_path()
dummy_icd10_phecode_map_path <- function() {
  system.file(
    "extdata",
    "dummy_Phecode_map_v1_2_icd10_beta.csv",
    package = "codemapper"
  )
}


#' Read dummy Phecode definitions file into R
#'
#' Reads a dummy Phecode definitions 1.2 csv file into R (full version may be
#' downloaded from
#' [phewascatalog.org](https://phewascatalog.org/phecodes_icd10))
#'
#' @return A data frame.
#' @export
#' @family Dummy data
#' @examples
#' read_phecode_lkp_dummy()
read_phecode_lkp_dummy <- function() {
  readr::read_csv(
    dummy_phecode_lkp_path(),
    progress = FALSE,
    col_types = readr::cols(.default = "c")
  )
}

#' Read dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R
#'
#' Reads a dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R (full
#' version may be downloaded from
#' [phewascatalog.org](https://phewascatalog.org/phecodes_icd10))
#'
#' @return A data frame.
#' @export
#' @family Dummy data
#' @examples
#' read_icd10_phecode_map_dummy()
read_icd10_phecode_map_dummy <- function() {
  readr::read_csv(
    dummy_icd10_phecode_map_path(),
    progress = FALSE,
    col_types = readr::cols(.default = "c")
  )
}

## UKB clinical events -----------------------------------------------------

#' Dummy UK Biobank clinical events, tidied
#'
#' A dummy UK Biobank data frame, as returned by
#' [ukbwranglr::tidy_clinical_events()].
#'
#' @return A data frame.
#' @export
#' @family Dummy data
#'
#' @examples
#' dummy_clinical_events_tidy()
dummy_clinical_events_tidy <- function() {
  tibble::tribble(
    ~eid,
    ~source,
    ~index,
    ~code,
    ~date,
    1,
    "f40001",
    "0_0",
    "I10",
    "1917-10-08",
    1,
    "f40002",
    "0_0",
    "E109",
    "1955-02-11",
    1,
    "f41271",
    "0_0",
    "4019",
    "1910-02-19",
    1,
    "gpc1_r2",
    "1",
    "C10..",
    "1965-08-08",
    1,
    "gpc1_r2",
    "2",
    "C10..",
    "1917-10-08",
    1,
    "gpc3_r3",
    "3",
    "XaIP9",
    "1917-10-08",
    1,
    "gpc3_r3",
    "3",
    "XE0Uc",
    "1917-10-08"
  )
}
