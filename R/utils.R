# PUBLIC ------------------------------------------------------------------

## Set ops -----------------------------------------------------------------

#' Infix set operations
#'
#' Infix versions of [dplyr::setops]. `%AND%`, `%OR%` and `%NOT%` are the
#' equivalents of [dplyr::intersect()], [dplyr::union()] and [dplyr::setdiff()]
#' respectively.
#'
#' @inheritParams dplyr::setops
#'
#' @name infix_setops
#' @examples
#' df1 <- tibble::tibble(x = 1:3)
#' df2 <- tibble::tibble(x = 3:5)
#'
#' # equivalent of dplyr::intersect(df1, df2)
#' df1 %AND% df2
#'
#' # equivalent of dplyr::union(df1, df2)
#' df1 %OR% df2
#'
#' # equivalent of dplyr::setdiff(df1, df2). Note that argument order matters
#' df1 %NOT% df2
#' df2 %NOT% df1
NULL

#' @rdname infix_setops
#' @export
`%AND%` <- function(x, y) {
  dplyr::intersect(x, y)
}

#' @rdname infix_setops
#' @export
`%OR%` <- function(x, y) {
  dplyr::union(x, y)
}

#' @rdname infix_setops
#' @export
`%NOT%` <- function(x, y) {
  dplyr::setdiff(x, y)
}


# CONSTANTS ---------------------------------------------------------------

UPDATE_CODE_SELECTION_MATCHING_VARS <- c("disease", "code_type", "code")

# PRIVATE -----------------------------------------------------------------

read_excel_to_named_list <- function(
  path,
  to_include = NULL,
  to_exclude = NULL,
  col_types = "text",
  ...
) {
  # validate args
  if (!is.null(to_include) & !is.null(to_exclude)) {
    stop("Error! One or both of `to_include` and `to_exclude` must be `NULL`")
  }

  # get sheet names
  sheet_names <- path %>%
    readxl::excel_sheets()

  # choose sheets
  if (!is.null(to_include)) {
    sheet_names <- subset(
      sheet_names,
      sheet_names %in% to_include
    )
  } else if (!is.null(to_exclude)) {
    sheet_names <- subset(sheet_names, !(sheet_names %in% to_exclude))
  }

  # read sheets into named list
  sheet_names %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = path, col_types = col_types, ...)
}

#' Remove footer rows from a table in UKB resource 592
#'
#' Metadata for each table in UKB resource 592 is recorded in one or more footer
#' rows (in column 1), separated from the main table by an empty row. This
#' function removes these rows for a single table. Raises an error if more than
#' 3 rows would be removed.
#'
#' @param df A data frame from UK Biobank resource 592 frames.
#' @param footer_metadata_col_idx Integer. Index for column containing footer
#'   metadata.
#'
#' @return A data frame
#' @noRd
rm_footer_rows_all_lkps_maps_df <- function(df, footer_metadata_col_idx = 1) {
  # get colname for column containing footer metadata (column 1 by default)
  footer_metadata_col_colname <- names(df)[footer_metadata_col_idx]

  # get rowids for rows with `NA` in column containing footer metadata
  df <- df %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(
      "rowid" = ifelse(
        !is.na(.data[[footer_metadata_col_colname]]),
        yes = NA_character_,
        no = .data[["rowid"]]
      )
    )

  # get max rowid (for rows with `NA` in column 1)
  max_rowid <- as.character(max(as.integer(df$rowid), na.rm = TRUE))

  # error if more than 3 rows will be removed
  assertthat::assert_that(
    as.integer(max_rowid) >= (nrow(df) - 2),
    msg = paste0(
      "Attempted to remove all rows after row number ",
      max_rowid,
      ". `df` has ",
      nrow(df),
      " rows"
    )
  )

  # convert rowid col to NA, unless rowid equals `max_rowid`. Then, fill
  # downwards, and remove these rows
  df %>%
    dplyr::mutate(
      "rowid" = ifelse(
        .data[["rowid"]] == !!max_rowid,
        yes = .data[["rowid"]],
        no = NA_character_
      )
    ) %>%
    tidyr::fill(tidyselect::all_of("rowid"), .direction = "down") %>%
    dplyr::filter(is.na(.data[["rowid"]])) %>%
    dplyr::select(-tidyselect::all_of("rowid"))
}

make_lkp_from_ukb_codings <- function(
  ukb_codings,
  Coding,
  Value_col_new_name,
  Meaning_col_new_nae = "description"
) {
  result <- ukb_codings[ukb_codings$Coding == Coding, -1]

  result <- ukbwranglr:::rename_cols(
    df = result,
    old_colnames = c("Value", "Meaning"),
    new_colnames = c(Value_col_new_name, Meaning_col_new_nae)
  )

  return(result)
}

#' Pre-populate a list of codes with category labels from an existing codelist
#'
#' To be used with \code{\link{runCodeMapper}}. The data frame returned by this
#' app is for a single disease and should have an empty 'category' column. If
#' the user uploads a (valid) clinical code list then the 'categories' will be
#' lifted over, matching on 'disease', 'code_type' and 'code'.
#'
#' @param current_selection A data frame, as returned by
#'   \code{\link{runCodeMapper}}.
#' @param previous_codelist A data frame of clinical codes. Must meet the
#'   requirements of \code{\link}
#'
#' @return A data frame.
#' @noRd
update_code_selection <- function(current_selection, previous_codelist) {
  # validate previous_codelist
  ukbwranglr::validate_clinical_codes(previous_codelist)

  # copy over previous categories and mark these codes as selected
  current_selection %>%
    dplyr::left_join(
      previous_codelist,
      # match on these columns (NB, does not include 'description' or author')
      by = UPDATE_CODE_SELECTION_MATCHING_VARS,
      suffix = c("", "_TOREMOVE")
    ) %>%
    dplyr::mutate(
      "category" = .data[["category_TOREMOVE"]],
      "selected" = dplyr::case_when(
        (!is.na(.data[["category"]])) |
          (.data[["category"]] != "") ~ "Yes",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(-tidyselect::ends_with("_TOREMOVE"))
}

#' Download a file
#'
#' First checks if the file already exists at the download path. If so, the file
#' path is returned invisibly without re-downloading.
#'
#' @param download_url Character
#' @param path Character
#'
#' @return File path to downloaded file
#' @noRd
download_file <- function(download_url, path = tempfile()) {
  if (file.exists(path)) {
    invisible(path)
  } else {
    utils::download.file(
      url = download_url,
      destfile = path,
      mode = "wb"
    )
    invisible(path)
  }
}

# TODO --------------------------------------------------------------------

validate_all_lkps_maps <- function(all_lkps_maps) {
  TRUE
}
