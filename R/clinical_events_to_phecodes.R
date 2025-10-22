# CONSTANTS ---------------------------------------------------------------

CLINICAL_EVENTS_SOURCES_MAPPED_TO_PHECODES <- c(
  # icd10
  "f40001",
  "f40002",
  "f20002_icd10",
  "f40006",
  "f41270",

  # icd9
  "f40013",
  "f41271",

  # gp read 3
  "gpc1_r3",
  "gpc2_r3",
  "gpc3_r3",
  "gpc4_r3",

  # gp read 2
  "gpc1_r2",
  "gpc2_r2",
  "gpc3_r2",
  "gpc4_r2"
)

# PUBLIC ----------------------------------------------------------------


# PRIVATE -----------------------------------------------------------------



#' Filter UK Biobank clinical events for selected data sources
#'
#' @param clinical_events A clinical events table (data frame or `tbl_dbi`
#'   object) created by [ukbwranglr::tidy_clinical_events()].
#' @param sources A character vector of data sources. Must be listed under
#'   `source` in [ukbwranglr::clinical_events_sources()].
#' @param allow_missing_sources If `FALSE` (default), an error is raised if any
#'   values for `sources` are not present in `clinical_events`. If `TRUE`, a
#'   warning is raised instead.
#'
#' @return A data frame
#' @noRd
get_clinical_events_source <- function(
  clinical_events,
  sources,
  allow_missing_sources = FALSE
) {
  # validate args
  assertthat::assert_that(all(
    sources %in% ukbwranglr::clinical_events_sources()$source
  ))
  ukbwranglr:::validate_clinical_events_and_check_type(clinical_events)

  # check selected sources are present
  check_sources <- clinical_events %>%
    dplyr::filter(.data[["source"]] %in% !!sources) %>%
    dplyr::distinct(
      dplyr::across(tidyselect::all_of("source")),
      .keep_all = FALSE
    ) %>%
    dplyr::collect()

  # error/warning if any sources are not present
  missing_sources <-
    subset(sources, !sources %in% check_sources$source)

  if (length(missing_sources) != 0) {
    missing_sources_msg <-
      "The following sources are not present in `clinical events`: "
    if (allow_missing_sources) {
      warning(paste0(
        "Warning! ",
        missing_sources_msg,
        stringr::str_c(missing_sources, sep = "", collapse = ", ")
      ))
    } else {
      stop(paste0(
        "Error!",
        missing_sources_msg,
        stringr::str_c(missing_sources, sep = "", collapse = ", ")
      ))
    }
  }

  # filter clinical events table for selected sources
  result <- clinical_events %>%
    dplyr::filter(.data[["source"]] %in% !!sources) %>%
    dplyr::collect()

  # return result
  return(result)
}
