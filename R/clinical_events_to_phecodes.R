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


#' Map codes in a UKB clinical events table
#'
#' Joins the existing `code` column to a mapping data frame created by
#' [get_mapping_df()], then filters out any missing values for the new code
#' type. A column is appended containing the newly mapped codes.
#'
#' The `clinical_events` data frame can contain multiple sources, but an error
#' is raised by default if these do not all use the same data coding (see
#' [ukbwranglr::clinical_events_sources()] for recognised data sources and their
#' respective data codings).
#'
#' @param clinical_events A clinical events data frame created by
#'   [ukbwranglr::tidy_clinical_events()]
#' @param from Coding type being mapped from
#' @param to Coding type being mapped to
#' @param from_colname Name of column containing codes to be mapped from. If
#'   `NULL` (default), this is assumed to be named 'code'.
#' @param to_colname Name of new column containing mapped codes. If `NULL`
#'   (default), this will equal the value for argument `to`.
#' @param all_lkps_maps Named list of Duckdb database with lookup/mapping tables
#' @param col_filters See [default_col_filters()]
#' @noRd
#'
#' @return A clinical events data frame.
map_codes_ukb_clinical_events <- function(
  clinical_events,
  from,
  to,
  from_colname = NULL,
  to_colname = NULL,
  all_lkps_maps = "all_lkps_maps.db",
  col_filters = default_col_filters()
) {
  # validate args
  check_mapping_args(
    from = from,
    to = to
  )

  if (!is.null(to_colname)) {
    assertthat::is.string(to_colname)
  }

  if (!is.null(from_colname)) {
    assertthat::is.string(from_colname)
  }

  # all_lkps_maps
  ## connect to database file path
  create_db_connection(all_lkps_maps)

  # create mapping df
  mapping_df <- get_mapping_df(
    from = from,
    to = to,
    all_lkps_maps = all_lkps_maps,
    rename_from_to = NULL,
    na.rm = TRUE,
    reverse_mapping = "error",
    col_filters = col_filters
  )

  # map
  if (is.null(from_colname)) {
    from_colname <- "code"
  }

  join_by <- from
  names(join_by) <- from_colname

  result <- clinical_events %>%
    dplyr::inner_join(
      mapping_df,
      by = join_by,
      relationship = "many-to-many"
    ) %>%
    # remove duplicated rows
    dplyr::distinct()

  # rename newly appended column (optionally)
  if (!is.null(to_colname)) {
    result <- ukbwranglr:::rename_cols(
      df = result,
      old_colnames = to,
      new_colnames = to_colname
    )
  }

  # return result
  return(result)
}

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
