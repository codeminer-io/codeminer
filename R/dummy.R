# PUBLIC ------------------------------------------------------------------

## UKB codings ----------------------------------------------------------------

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
  system.file("extdata", "dummy_Codings.tsv", package = "codeminer")
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
    package = "codeminer"
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
    package = "codeminer"
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
