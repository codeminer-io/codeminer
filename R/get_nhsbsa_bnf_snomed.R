#' Download the NHSBSA BNF-SNOMED mapping file
#'
#' Downloads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D
#' mapping file from the NHSBSA website.
#'
#' @param dir_path Directory to download the file to. Defaults to `tempdir()`.
#' @param overwrite Logical. If `TRUE`, re-downloads the file even if it
#'   already exists. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded zip file (invisibly).
#'
#' @seealso [read_nhsbsa_bnf_snomed()], [add_nhsbsa_bnf_snomed()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_nhsbsa_bnf_snomed()
#' }
get_nhsbsa_bnf_snomed <- function(
  dir_path = tempdir(),
  overwrite = FALSE,
  quiet = FALSE
) {
  zip_name <- "BNF_Snomed_Mapping.zip"
  download_url <- "https://www.nhsbsa.nhs.uk/sites/default/files/2025-01/BNF%20Snomed%20Mapping%20data%2020250120.zip"

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  target_file <- file.path(dir_path, zip_name)

  if (file.exists(target_file) && !overwrite) {
    msg(c(
      "v" = "{.file {zip_name}} already exists at {.file {dir_path}}.",
      "i" = "Set {.code overwrite = TRUE} to redownload if needed."
    ))
    return(invisible(target_file))
  }

  msg("Downloading NHSBSA BNF-SNOMED mapping file")

  tryCatch(
    utils::download.file(download_url, target_file, mode = "wb", quiet = quiet),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to download NHSBSA BNF-SNOMED mapping file",
        "i" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  msg("NHSBSA BNF-SNOMED mapping file available at {.file {target_file}}")

  invisible(target_file)
}
