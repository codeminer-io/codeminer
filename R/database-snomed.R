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
#' download_latestversion_of_snomed_item() downloads and extracts the most recent release
#' of a specified SNOMED CT item.
#' By default, it retrieves the UK Clinical Edition Monolith (item 1799).
#'
#' @param item_number Numeric. The TRUD item number identifying the SNOMED CT UK data package.
#'   Defaults to 1799, which corresponds to the SNOMED CT UK Clinical Edition (Monolith).
#'
#' @param release_index Numeric. The index position of the TRUD metadata release to download.
#'   Use 1 for the most recent release (e.g., "uk_sct2mo_41.1.0_20251022000001Z.zip").
#'   Valid indices typically range from 1 (latest) to 48 (oldest, e.g., "Release 32.11.0").
#'   **Note:** These index ranges are subject to change as new releases become available on TRUD.
#'
#' @param path_destination Character. Path to the directory where downloaded files
#'   should be extracted. Defaults to the current working directory (".").
#'
#' @return A named list containing:
#' \describe{
#'   \item{release_id}{The ID of the latest release downloaded.}
#'   \item{file_path}{The full path to the downloaded ZIP file.}
#'   \item{extracted_dir}{The directory where the files were extracted.}
#' }
#'
#' @details
#' The function uses the [trud](https://cran.r-project.org/package=trud) package to interact with the NHS TRUD API.
#' Ensure you have installed and configured it correctly before use.
#'
#' @seealso
#' [NHS TRUD Documentation](https://isd.digital.nhs.uk/trud/),
#' [trud::download_item()](https://docs.ropensci.org/trud/)
#'
#' @examples
#' \dontrun{
#' # Download the default SNOMED CT UK Monolith
#' result <- download_latestversion_of_snomed_item()
#'
#' # Download a specific earlier release and extract to a custom folder
#' download_latestversion_of_snomed_item(
#'   item_number = 1799,
#'   release_index = 5,
#'   path_destination = "data/snomed_releases"
#' )
#'
#' # Check the extracted directory
#' result$extracted_dir
#' }
#'
#' @export
download_latestversion_of_snomed_item <- function(
  item_number = 1799,
  release_index = 1,
  path_destination = "."
) {
  rlang::check_installed("trud")

  if (Sys.getenv("TRUD_API_KEY") == "") {
    cli::cli_abort(
      c(
        "TRUD API key not found.",
        "i" = "Please set the {.envvar TRUD_API_KEY} environment variable.",
        "i" = "See documentation for setup instructions."
      )
    )
  }

  if (
    !is.numeric(item_number) ||
      length(item_number) != 1 ||
      item_number <= 0 ||
      item_number != as.integer(item_number)
  ) {
    cli::cli_abort(
      "{.arg item_number} must be a single positive integer value."
    )
  }

  if (!dir.exists(path_destination)) {
    cli::cli_abort(
      "The directory {.path {path_destination}} does not exist."
    )
  }

  cli::cli_alert_info("Retrieving metadata for item {.field {item_number}} ...")

  metadata <- tryCatch(
    trud::get_item_metadata(item_number, release_scope = "all"),
    error = function(e) {
      cli::cli_abort("Failed to retrieve metadata: {e$message}")
    }
  )

  releases <- metadata$releases
  if (
    !is.numeric(release_index) ||
      length(release_index) != 1 ||
      release_index <= 0 ||
      release_index != as.integer(release_index) ||
      release_index > length(releases)
  ) {
    cli::cli_abort(
      "{.arg release_index} must be a positive integer between 1 and {length(releases)}."
    )
  }

  latest_release <- releases[[release_index]]
  latest_release_id <- latest_release$id

  cli::cli_alert_info(
    "Release {release_index} found: 
    {.field {latest_release_id}} (Date: {.val {latest_release$releaseDate}})"
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
      cli::cli_abort("Download failed: {e$message}")
    }
  )

  if (!file.exists(zipfile_path)) {
    cli::cli_abort(
      "Download failed or file not found at: {.path {zipfile_path}}"
    )
  }

  cli::cli_alert_success("Download complete: {.path {zipfile_path}}")

  extracted_dir <- file.path(
    normalizePath(path_destination, mustWork = TRUE),
    paste0("snomed_item_", item_number, "_", latest_release_id)
  )

  dir.create(extracted_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zipfile_path, exdir = extracted_dir)

  cli::cli_alert_success(
    "Extracted contents to {.path {extracted_dir}} ..."
  )

  invisible(list(
    release_id = latest_release_id,
    file_path = zipfile_path,
    extracted_dir = extracted_dir
  ))
}


#' Read the SNOMED CT UK Monolith Edition into R
#'
#' @description
#' Reads the Terminology and Refset snapshot tables from a local copy of the
#' **SNOMED CT UK Monolith Edition** release files.
#' This function imports the key data files as data.table objects, performs basic
#' normalization and constructs joined tables for concept descriptions and ICD-10 mappings.
#'
#' For more information on the SNOMED CT UK Monolith Edition, see:
#' <https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases>.
#'
#' @param path_destination A character string giving the path to the unzipped
#'   SNOMED CT UK Monolith Edition directory.
#'   This directory must contain the subdirectories:
#'   - Snapshot/Terminology
#'   - Snapshot/Refset
#'
#' @details
#' The function reads all .txt files within the Terminology and Refset snapshot folders
#' using data.table::fread(), with all columns imported as character type to preserve data fidelity.
#' It constructs:
#' - A joined table of SNOMED CT concept and description data.
#' - The relationship table of concept relationships.
#' - The ICD-10 map subset from the Extended Map refset.
#'
#' The function automatically normalizes file names by removing suffixes like `_GB_<date>.txt`.
#'
#' @return
#' A named list with three elements:
#' \describe{
#'   \item{sct_description}{A data.table combining SNOMED CT concepts and descriptions.}
#'   \item{sct_relationship`}{A data.table containing concept relationships.}
#'   \item{sct_icd10}{A data.table containing SNOMED CT-to-ICD-10 map entries.}
#' }
#'
#' @examples
#' \dontrun{
#' # Read a local SNOMED CT UK Monolith Edition
#' snomed <- read_snomed_ct_uk_monolith("~/SNOMEDCT_Release_UK")
#'
#' # View available datasets
#' names(snomed)
#'
#' # Inspect a few rows of each
#' head(snomed$sct_description)
#' head(snomed$sct_relationship)
#' head(snomed$sct_icd10)
#' }
#'
#' @export
read_snomed_ct_uk_monolith <- function(path_destination) {
  # Define expected structure
  expected_dirs <- file.path(
    path_destination,
    "Snapshot",
    c(
      "Refset/Content",
      "Refset/Language",
      "Refset/Map",
      "Refset/Metadata",
      "Terminology"
    )
  )

  # Check the main directory first
  if (!dir.exists(path_destination)) {
    cli::cli_abort("Directory does not exist: {.path {path_destination}}")
  }

  # Identify which expected subdirectories are missing
  missing_dirs <- expected_dirs[!dir.exists(expected_dirs)]

  if (length(missing_dirs) > 0) {
    cli::cli_abort(c(
      "!" = "The following required directories are missing under {.path {path_destination}}:",
      "•" = paste(missing_dirs, collapse = "\n• ")
    ))
  } else {
    cli::cli_inform(
      "All required subdirectories exist under {.path {path_destination}}"
    )
  }

  snomed_monolith_terminology <-
    file.path(path_destination, "Snapshot", "Terminology") |>
    list.files(full.names = TRUE) |>
    purrr::set_names(nm = fs::path_file) |>
    purrr::map(
      ~ data.table::fread(
        .x,
        sep = "\t",
        colClasses = "character",
        quote = ""
      )
    )

  snomed_monolith_refset <- file.path(
    path_destination,
    "Snapshot",
    "Refset"
  ) |>
    list.files(full.names = TRUE) |>
    purrr::set_names(nm = fs::path_file) |>
    purrr::map(
      ~ .x |>
        list.files(full.names = TRUE) |>
        purrr::set_names(nm = fs::path_file) |>
        purrr::map(
          ~ data.table::fread(
            .x,
            sep = "\t",
            colClasses = "character",
            quote = ""
          )
        )
    )

  names(snomed_monolith_terminology) <-
    stringr::str_replace(
      string = names(snomed_monolith_terminology),
      pattern = "_GB_[0-9]+\\.txt",
      ".txt"
    )

  valid_slots <- intersect(
    c("Content", "Language", "Map", "Metadata"),
    names(snomed_monolith_refset)
  )
  for (slot in valid_slots) {
    if (!is.null(snomed_monolith_refset[[slot]])) {
      names(snomed_monolith_refset[[slot]]) <-
        stringr::str_replace(
          names(snomed_monolith_refset[[slot]]),
          "_GB_[0-9]+\\.txt",
          ".txt"
        )
    }
  }

  sct_description <- snomed_monolith_terminology$`sct2_Description_MONOSnapshot-en.txt` |>
    dplyr::rename_with(\(x) paste0(x, "_description")) |>
    dplyr::full_join(
      snomed_monolith_terminology$sct2_Concept_MONOSnapshot.txt |>
        dplyr::rename_with(\(x) paste0(x, "_concept")),
      by = c("conceptId_description" = "id_concept")
    ) |>
    dplyr::rename("conceptId" = "conceptId_description")

  sct_relationship <- snomed_monolith_terminology$sct2_Relationship_MONOSnapshot.txt

  sct_icd10 <- snomed_monolith_refset$Map$der2_iisssciRefset_ExtendedMapMONOSnapshot.txt |>
    dplyr::filter(.data[["refsetId"]] == "999002271000000101") |>
    dplyr::filter(stringr::str_detect(
      .data[["mapTarget"]],
      "#",
      negate = TRUE
    ))

  return(
    list(
      sct_description = sct_description,
      sct_relationship = sct_relationship,
      sct_icd10 = sct_icd10
    )
  )
}
