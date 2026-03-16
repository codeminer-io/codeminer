#' Download the UK Biobank codings file
#'
#' Downloads the UK Biobank `Codings.tsv` file, which contains lookup tables
#' for OPCS-4 and UKB self-reported condition/medication/operation codes.
#'
#' @param dir_path Directory to download the file to. Will error if it does not
#'   exist. Defaults to `tempdir()`.
#' @param overwrite Logical. If `TRUE`, re-downloads the file even if it
#'   already exists. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded `Codings.tsv` (invisibly).
#'
#' @seealso [read_ukb_codings()], [add_ukb_codings()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_ukb_codings()
#' path <- get_ukb_codings(dir_path = "~/ukb_data", overwrite = TRUE)
#' }
get_ukb_codings <- function(
  dir_path = tempdir(),
  overwrite = FALSE,
  quiet = FALSE
) {
  file_name <- "Codings.tsv"
  download_url <- "https://raw.githubusercontent.com/rmgpanw/ukb_backup_files/main/Codings.tsv"

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  target_file <- file.path(dir_path, file_name)

  if (file.exists(target_file) && !overwrite) {
    msg(c(
      "v" = "{.file {file_name}} already exists at {.file {dir_path}}.",
      "i" = "Set {.code overwrite = TRUE} to redownload if needed."
    ))
    return(invisible(target_file))
  }

  msg("Downloading UKB codings file to {.file {dir_path}}")

  tryCatch(
    utils::download.file(download_url, target_file, mode = "wb", quiet = quiet),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to download UKB codings file",
        "i" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  msg("UKB codings file available at {.file {target_file}}")

  invisible(target_file)
}
