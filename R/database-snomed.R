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

#' Download Locally the Latest Version of SNOMED CT Item
#'
#' Downloads the most recent release of a SNOMED CT item from TRUD.
#' By default, downloads the UK Clinical Edition Monolith (item 1799).
#'
#' @param item_number TRUD item number (default: 1799 for SNOMED CT UK Monolith)
#'
#' @return Path to the downloaded file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download default SNOMED CT UK Monolith
#' file_path <- download_locally_latestversion_of_snomed_item()
#'
#' # Download specific item
#' file_path <- download_locally_latestversion_of_snomed_item(1234)
#' }
download_locally_latestversion_of_snomed_item <- function(item_number = 1799) {
  # Input validation
  if (
    !is.numeric(item_number) || length(item_number) != 1 || item_number <= 0
  ) {
    stop("item_number must be a positive numeric value")
  }

  # Get metadata for all releases
  metadata <- trud::get_item_metadata(item_number, release_scope = "all")

  # Check if releases exist
  if (is.null(metadata$releases) || length(metadata$releases) == 0) {
    stop(sprintf("No releases found for item number: %s", item_number))
  }

  # Extract latest release ID
  latest_release_id <- metadata$releases[[1]]$id

  # Download the item
  zipfile_and_path <- trud::download_item(
    item = item_number,
    directory = tempdir(),
    release = latest_release_id,
    overwrite = TRUE
  )

  # Verify download was successful
  if (!file.exists(zipfile_and_path)) {
    stop(sprintf("Download failed for item number: %s", item_number))
  }

  message(sprintf(
    "Successfully downloaded item %s (release: %s) to: %s",
    item_number,
    latest_release_id,
    zipfile_and_path
  ))

  # Return both values as a named list
  return(list(
    release_id = latest_release_id,
    file_path = zipfile_and_path
  ))
}
