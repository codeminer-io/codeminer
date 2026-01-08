#' Get UK Biobank resource 592 directly from UK Biobank website
#'
#' Downloads the UK Biobank code mappings file (`all_lkps_maps_v4.xlsx`,
#' [resource 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) directly
#' from the UKB website.
#'
#' @param dir_path Directory path to download to. Will be created if it doesn't
#'   exist.
#' @param overwrite Logical. If `TRUE`, re-downloads and overwrites existing
#'   file. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to downloaded `all_lkps_maps_v4.xlsx` (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # download UKB resource 592, returning file path invisibly
#' file_path <- get_ukb_resource_592()
#'
#' # download to specific directory, overwriting if exists
#' file_path <- get_ukb_resource_592(
#'   dir_path = tempdir(),
#'   overwrite = TRUE
#' )
#'
#' # view path to downloaded file
#' file_path
#' }
get_ukb_resource_592 <- function(
  dir_path = tempdir(),
  overwrite = FALSE,
  quiet = FALSE
) {
  # Constants
  excel_filename <- "all_lkps_maps_v4.xlsx"
  zip_url <- "https://biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip"
  zip_filename <- "primarycare_codings.zip"

  # Helper for conditional messaging
  msg <- function(...) if (!quiet) cli::cli_inform(...)

  msg("Getting UKB resource 592")

  # Check whether target directory exists
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  target_file <- file.path(dir_path, excel_filename)

  # Handle existing file
  if (file.exists(target_file)) {
    if (!overwrite) {
      # We use warn here because we are aborting the download/extraction
      msg(c(
        "v" = "{.file {excel_filename}} already exists at {.file {dir_path}}.",
        "i" = "Set {.code overwrite = TRUE} to redownload if needed."
      ))
      return(invisible(target_file))
    } else {
      # We use inform here because the function is proceeding as requested
      msg(c(
        "i" = "{.file {excel_filename}} exists, but {.code overwrite = TRUE} is set.",
        "v" = "Redownloading and overwriting."
      ))
    }
  }

  # Setup zip file path in tempdir
  zip_filepath <- file.path(tempdir(), zip_filename)

  # Download zip file if not already cached in tempdir
  if (!file.exists(zip_filepath) || overwrite) {
    msg("Downloading zipped UKB resource 592 to tempdir")

    tryCatch(
      utils::download.file(
        zip_url,
        zip_filepath,
        mode = "wb",
        quiet = quiet
      ),
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Failed to download UKB resource 592",
          "i" = "Error: {conditionMessage(e)}"
        ))
      }
    )
  } else {
    msg("Using cached zip file from tempdir")
  }

  # Extract Excel file from zip
  msg("Extracting {.file {excel_filename}} from zip file to {.file {dir_path}}")

  tryCatch(
    utils::unzip(
      zip_filepath,
      files = excel_filename,
      exdir = dir_path,
      overwrite = overwrite
    ),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to extract {.file {excel_filename}} from zip file",
        "i" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  msg("UKB resource 592 is availabe at {.file {target_file}}")

  invisible(target_file)
}
