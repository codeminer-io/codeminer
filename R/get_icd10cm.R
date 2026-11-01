#' Get ICD-10-CM release files
#'
#' Downloads an ICD-10-CM "code descriptions in tabular order" release from the
#' National Center for Health Statistics (NCHS), part of the Centers for Disease
#' Control and Prevention (CDC). ICD-10-CM is the US clinical modification of
#' ICD-10, released for each US fiscal year (effective 1 October) with
#' occasional mid-year updates.
#'
#' @param dir_path Directory path to download to. Defaults to `tempdir()`.
#' @param release Character string specifying which release to download. Either:
#'   * `"latest"` (default) — the most recent release known to the package.
#'   * An 8-digit effective date (`"YYYYMMDD"`), e.g. `"20261001"` for the
#'     FY2027 release. See Details for the available releases.
#' @param overwrite Logical. If `TRUE`, re-downloads and overwrites an existing
#'   file. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded release (invisibly). Depending on how
#'   NCHS published that release this is either a `.zip` containing the order
#'   file or the order `.txt` file itself; either is accepted by
#'   [read_icd10cm()]. The file is named `icd10cm_order_<release>.<ext>` so the
#'   release date can be recovered from the file name.
#'
#' @details
#' NCHS does not name release files consistently from year to year, so the
#' package carries a table of verified download URLs keyed by effective date.
#' Releases known to the package (effective date, fiscal year), generated from
#' that table when the documentation is built:
#'
#' `r icd10cm_releases_doc()`
#'
#' Releases are keyed by their full effective date, so a fiscal year with a
#' mid-year update has two entries (e.g. `20221001` and `20230401`). The April
#' 2024, April 2025 and April 2026 updates are deliberately absent: NCHS
#' re-issued the previous October's code descriptions unchanged (only the
#' guidelines, index and addenda documents differed), so they would download
#' an identical code set under a second version label. A release newer than
#' this table can be downloaded by hand from the NCHS listing and passed to
#' [read_icd10cm()] or [add_icd10cm()] with an explicit `version`.
#'
#' @section Attribution:
#' ICD-10-CM is published by NCHS/CDC and is available free of charge at
#' <https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files does
#' not imply endorsement by CDC, HHS or the US Government.
#'
#' @seealso [read_icd10cm()], [add_icd10cm()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_icd10cm()
#' read_icd10cm(path)
#'
#' # A specific release
#' get_icd10cm(release = "20250401")
#' }
get_icd10cm <- function(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
) {
  if (!is.character(release) || length(release) != 1 || release == "") {
    cli::cli_abort(c(
      "x" = "{.arg release} must be a non-empty character string",
      "i" = "Use {.val latest} for the most recent release, or an 8-digit
             effective date such as {.val 20261001}."
    ))
  }

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  releases <- icd10cm_releases()

  if (release == "latest") {
    selected <- releases[nrow(releases), ]
  } else {
    selected <- releases[releases$version == release, ]
    if (nrow(selected) == 0) {
      cli::cli_abort(c(
        "x" = "Unknown ICD-10-CM release {.val {release}}.",
        "i" = "Available releases: {.val {releases$version}}",
        "i" = "For a newer release, download it from
               {.url https://www.cdc.gov/nchs/icd/icd-10-cm/files.html} and
               pass the file to {.fun read_icd10cm} with an explicit
               {.arg version}."
      ))
    }
  }

  target_file <- file.path(
    dir_path,
    paste0("icd10cm_order_", selected$version, ".", fs::path_ext(selected$file))
  )

  if (file.exists(target_file) && !overwrite) {
    msg(c(
      "v" = "{.file {basename(target_file)}} already exists.",
      "i" = "Set {.code overwrite = TRUE} to redownload."
    ))
    return(invisible(target_file))
  }

  msg(
    "Downloading ICD-10-CM release {.val {selected$version}} to {.file {dir_path}}..."
  )

  tryCatch(
    utils::download.file(
      selected$url,
      target_file,
      mode = "wb",
      quiet = quiet
    ),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to download ICD-10-CM release {.val {selected$version}}",
        "i" = "URL: {.url {selected$url}}",
        "i" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  msg(c("v" = "ICD-10-CM release downloaded to {.file {target_file}}"))

  invisible(target_file)
}

# Verified NCHS download locations for each ICD-10-CM release, keyed by the
# release's effective date. File names on the NCHS site vary from year to year
# and some folders hold two releases (the loose `.txt` in `2022/` is the April
# 2022 update; the zip is the October 2021 release), so the paths are listed
# explicitly rather than derived. Rows are in ascending date order; the last
# row is "latest".
icd10cm_releases <- function() {
  base_url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/"

  # fmt: skip
  tibble::tribble(
    ~version   , ~file                                                               ,
    "20151001" , "2016/ICD10CM_FY2016_code_descriptions.zip"                          ,
    "20161001" , "2017/icd10cm_order_2017.txt"                                        ,
    "20171001" , "2018/2018-ICD-10-Code-Order-Descriptions.zip"                       ,
    "20181001" , "2019/icd10cm_order_2019.txt"                                        ,
    "20191001" , "2020/icd10cm_order_2020.txt"                                        ,
    "20201001" , "2021/icd10cm_order_2021.txt"                                        ,
    "20210101" , "2021/icd10cm-codes-order-Jan-2021.zip"                              ,
    "20211001" , "2022/Code Descriptions zip.zip"                                     ,
    "20220401" , "2022/icd10cm_order_2022.txt"                                        ,
    "20221001" , "2023/icd10-Order-CodeFiles2023.zip"                                 ,
    "20230401" , "April-1-2023-Update/icd10cm-code descriptions- April 1 2023.zip"    ,
    "20231001" , "2024/icd10cm-CodesDescriptions-2024.zip"                            ,
    "20241001" , "2025/ICD10-CM Code Descriptions 2025.zip"                           ,
    "20251001" , "2026/icd10cm-Code Descriptions-2026.zip"                            ,
    "20261001" , "2027/icd10cm-code-descriptions-2027.zip"
  ) |>
    dplyr::mutate(
      url = paste0(base_url, purrr::map_chr(.data$file, utils::URLencode))
    )
}

# Markdown bullet list of the known releases, evaluated inline in the roxygen
# for `get_icd10cm()` so the documentation cannot drift from the table. The
# US fiscal year starts on 1 October; any other effective month is a mid-year
# update to the fiscal year already in force.
icd10cm_releases_doc <- function() {
  versions <- icd10cm_releases()$version
  year <- as.integer(stringr::str_sub(versions, 1, 4))
  month <- as.integer(stringr::str_sub(versions, 5, 6))
  fiscal_year <- ifelse(month >= 10, year + 1, year)
  update <- ifelse(
    month == 10,
    "",
    paste0(" ", month.name[month], " update")
  )
  paste0("* `", versions, "` (FY", fiscal_year, update, ")", collapse = "\n")
}
