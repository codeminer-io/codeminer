#' Download the Latest SNOMED CT Release from NHS TRUD
#'
#' This function automates downloading and extracting the most recent release of a SNOMED CT item from the
#' NHS TRUD (Technology Reference data Update Distribution) service.
#'
#' ## Prerequisites
#' Access to SNOMED CT data via the NHS TRUD service is required. Follow the steps below to configure access.
#'
#' ### Step 1: Create an NHS TRUD Account
#' - Sign up for a free NHS TRUD account:
#'   https://isd.digital.nhs.uk/trud/users/guest/filters/0/account/form
#'
#' - Obtain your **API key** from your TRUD profile.
#'
#' - Set your API key in your `.Renviron` file. You can open it using:
#'   ```r
#'   usethis::edit_r_environ()
#'   ```
#'   Then add a line like this (replace `<key>` with your API key):
#'   ```
#'   TRUD_API_KEY=<key>
#'   ```
#'
#' - Verify that your key is set correctly:
#'   ```r
#'   Sys.getenv("TRUD_API_KEY")
#'   ```
#'
#' ### Step 2: Subscribe to SNOMED CT Data
#' - After logging into NHS TRUD, subscribe to the SNOMED CT items you need:
#'   https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/1/items/1799/releases
#'
#' Ensure that your API key and subscriptions are active before running this function.
#'
#' ## Function Description
#'
#' `download_latestversion_of_snomed_item()` downloads and extracts the most recent release
#' of a specified SNOMED CT item.
#' By default, it retrieves the UK Clinical Edition Monolith (item 1799).
#'
#' @param item_number Numeric. TRUD item number (default: `1799` for SNOMED CT UK Monolith).
#' @param directory_to_extract_files Character. Directory where the files should be extracted (default: `"."`).
#'
#' @return A named list containing:
#' \describe{
#'   \item{release_id}{The ID of the latest release downloaded.}
#'   \item{file_path}{The full path to the downloaded ZIP file.}
#'   \item{extracted_dir}{The directory where the files were extracted.}
#' }
#'
#' @details
#' The function uses the [`trud`](https://cran.r-project.org/package=trud) package to interact with the NHS TRUD API.
#' Ensure you have installed and configured it correctly before use.
#'
#' @seealso
#' [NHS TRUD Documentation](https://isd.digital.nhs.uk/trud/),
#' [`trud::download_item()`](https://docs.ropensci.org/trud/)
#'
#' @examples
#' \dontrun{
#' # Download the default SNOMED CT UK Monolith
#' result <- download_latestversion_of_snomed_item()
#'
#' # Download a specific SNOMED CT item by number
#' result <- download_latestversion_of_snomed_item(item_number = 1234)
#'
#' # Check the extracted directory
#' result$extracted_dir
#' }
#'
#' @export
download_latestversion_of_snomed_item <- function(
  item_number = 1799,
  directory_to_extract_files = "."
) {
  rlang::check_installed("trud")

  if (
    !is.numeric(item_number) || length(item_number) != 1 || item_number <= 0
  ) {
    cli::cli_abort(
      "{.arg item_number} must be a single positive numeric value."
    )
  }

  if (!dir.exists(directory_to_extract_files)) {
    msg <- sprintf(
      "The directory '%s' does not exist.",
      directory_to_extract_files
    )
    cli::cli_alert_danger(msg)
    stop(msg)
  }

  cli::cli_alert_info("Retrieving metadata for item {.field {item_number}} ...")

  metadata <- tryCatch(
    trud::get_item_metadata(item_number, release_scope = "all"),
    error = function(e) {
      cli::cli_alert_danger("Failed to retrieve metadata: {e$message}")
      stop(e)
    }
  )

  releases <- metadata$releases
  if (is.null(releases) || length(releases) == 0) {
    msg <- sprintf("No releases found for item number %s.", item_number)
    cli::cli_alert_danger(msg)
    stop(msg)
  }

  latest_release <- releases[[1]]
  latest_release_id <- latest_release$id

  cli::cli_alert_info(
    "Latest release found: {.field {latest_release_id}} (Date: {.val {latest_release$releaseDate %||% 'unknown'}})"
  )

  cli::cli_alert_info("Downloading release ...")
  zipfile_path <- tryCatch(
    trud::download_item(
      item = item_number,
      directory = tempdir(),
      release = latest_release_id,
      overwrite = TRUE
    ),
    error = function(e) {
      cli::cli_alert_danger("Download failed: {e$message}")
      stop(e)
    }
  )

  if (!file.exists(zipfile_path)) {
    cli::cli_abort(
      "Download failed or file not found at: {.path {zipfile_path}}"
    )
  }

  cli::cli_alert_success("Download complete: {.path {zipfile_path}}")

  extracted_dir <- file.path(
    normalizePath(directory_to_extract_files, mustWork = TRUE),
    paste0("snomed_item_", item_number, "_", latest_release_id)
  )

  dir.create(extracted_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zipfile_path, exdir = extracted_dir)

  cli::cli_alert_success(
    "Extracted contents to {.path {directory_to_extract_files}} ..."
  )

  invisible(list(
    release_id = latest_release_id,
    file_path = zipfile_path,
    extracted_dir = extracted_dir
  ))
}
