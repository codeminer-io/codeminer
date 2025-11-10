#' Using `database-snomed.R`
#'
#' This script requires access to SNOMED CT data via the NHS TRUD service.
#' Follow the steps below to configure access.
#'
#' **Step 1: Create an NHS TRUD account**
#'
#' - Sign up for a free NHS TRUD account:
#'  https://isd.digital.nhs.uk/trud/users/guest/filters/0/account/form
#'
#' - Obtain your **API key** from your TRUD profile.
#'
#' - Set your API key in your `.Renviron` file. You can open it using:
#'   ```r
#'   usethis::edit_r_environ()
#'   ```
#'   Then add a line like this (replacing `<key>` with your own API key):
#'   ```
#'   TRUD_API_KEY=<key>
#'   ```
#'
#' - You can verify that your key is set correctly with:
#'   ```r
#'   Sys.getenv("TRUD_API_KEY")
#'   ```
#'
#' **Step 2: Subscribe to SNOMED CT data**
#'
#' - After logging in to NHS TRUD, subscribe to the SNOMED CT items you need:
#'   https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/1/items/1799/releases
#'
#' Once your subscriptions are approved, you can access and use the SNOMED CT data within this script.
#'
#' @note Make sure your API key and subscriptions are active before running the script.
#'
#' @seealso [NHS TRUD Documentation](https://isd.digital.nhs.uk/trud/)

#' Get SNOMED Available Items
#'
#' Retrieves the list of SNOMED CT data items available from the NHS TRUD service.
#'
#' This function checks that the `trud` package is installed, then calls
#' `trud::trud_items()` to fetch the available SNOMED CT items.
#'
#' @return A data frame or list (depending on `trud::trud_items()` output)
#'   containing information about available SNOMED CT items.
#' @examples
#' \dontrun{
#' get_snomed_available_items()
#' }
#' @export
get_snomed_available_items <- function() {
  rlang::check_installed("trud")
  available_items <- trud::trud_items()
  return(available_items)
}

#' Download the Latest Version of a SNOMED CT Item Locally
#'
#' Downloads the most recent release of a SNOMED CT item from TRUD.
#' By default, downloads the UK Clinical Edition Monolith (item 1799).
#'
#' @param item_number Numeric. TRUD item number (default: 1799 for SNOMED CT UK Monolith)
#' @param directory_to_extract_files Character. Directory where the files should be extracted (default: ".")
#'
#' @return A named list containing:
#' \describe{
#'   \item{release_id}{The ID of the latest release downloaded.}
#'   \item{file_path}{The path to the downloaded ZIP file.}
#'   \item{extracted_dir}{The directory where the files were extracted.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the default SNOMED CT UK Monolith
#' result <- download_locally_latestversion_of_snomed_item()
#'
#' # Download a specific item
#' result <- download_locally_latestversion_of_snomed_item(1234)
#' }
download_locally_latestversion_of_snomed_item <- function(
  item_number = 1799,
  directory_to_extract_files = "."
) {
  # Input validation
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

  # Get metadata for all releases
  cli::cli_alert_info("Retrieving metadata for item {.field {item_number}} ...")

  metadata <- tryCatch(
    trud::get_item_metadata(item_number, release_scope = "all"),
    error = function(e) {
      cli::cli_alert_danger("Failed to retrieve metadata: {e$message}")
      stop(e)
    }
  )

  cli::cli_alert_info("Validating metadata ...")

  releases <- metadata$releases
  if (is.null(releases) || length(releases) == 0) {
    msg <- sprintf("No releases found for item number %s.", item_number)
    cli::cli_alert_danger(msg)
    stop(msg)
  }

  # Identify latest release
  # TRUD metadata is usually sorted with most recent first, but ensure explicitly
  latest_release <- releases[[1]]
  latest_release_id <- latest_release$id
  release_date <- latest_release$releaseDate %||% "unknown"

  cli::cli_alert_info(
    "Latest release found: {.field {latest_release_id}} (Date: {.val {release_date}})"
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
    stop(msg)
  }

  cli::cli_alert_success("Download complete: {.path {zipfile_path}}")

  cli::cli_alert_info("Extracting contents ...")

  extracted_dir <- file.path(
    normalizePath(directory_to_extract_files, mustWork = TRUE),
    paste0("snomed_item_", item_number, "_", latest_release_id)
  )

  dir.create(extracted_dir, showWarnings = FALSE, recursive = TRUE)

  utils::unzip(zipfile_path, exdir = extracted_dir)

  cli::cli_alert_success(
    "Extracting contents to {.path {directory_to_extract_files}} ..."
  )

  # Return result
  invisible(list(
    release_id = latest_release_id,
    file_path = zipfile_path,
    extracted_dir = extracted_dir
  ))
}
