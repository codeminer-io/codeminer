#' Get SNOMED CT UK Monolith Edition from NHS TRUD
#'
#' Downloads the SNOMED CT UK Monolith Edition (item 1799) from NHS TRUD.
#'
#' @param dir_path Directory path to download to. Defaults to `tempdir()`.
#' @param release Character string specifying which release to download. Can be:
#'   * `"latest"` (default) - Downloads the most recent release
#'   * A specific release identifier string (e.g., `"uk_sct2mo_41.3.0_20251217000001Z.zip"`)
#' @param overwrite Logical. If `TRUE`, re-downloads and overwrites existing
#'   files. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded SNOMED CT UK Monolith zip file
#'   (invisibly).
#'
#' @details
#' This function requires a valid NHS TRUD API key set as an environment
#' variable `TRUD_API_KEY`. You must also be subscribed to item 1799 on the
#' NHS TRUD website.
#'
#' By default, the latest release is downloaded. The function checks if the
#' target zip file already exists and skips the download if so (unless
#' `overwrite = TRUE`).
#'
#' The returned zip file path can be passed directly to
#' [read_snomed_ct_uk_monolith()], which will extract the contents on-demand.
#'
#' @seealso
#' * [read_snomed_ct_uk_monolith()] to read the downloaded data into R
#' * [trud::download_item()] for the underlying download function
#' * [trud::get_item_metadata()] to retrieve available release identifiers
#'
#' @export
#' @examples
#' \dontrun{
#' # Download latest SNOMED CT UK Monolith to tempdir
#' snomed_zip <- get_snomed_ct_uk_monolith()
#'
#' # Get available releases
#' metadata <- trud::get_item_metadata(1799, release_scope = "all")
#' available_releases <- purrr::map_chr(metadata$releases, "id")
#'
#' # Download specific release
#' snomed_zip <- get_snomed_ct_uk_monolith(
#'   release = "uk_sct2mo_41.3.0_20251217000001Z.zip"
#' )
#'
#' # Download to specific directory for persistent storage
#' snomed_zip <- get_snomed_ct_uk_monolith(
#'   dir_path = "~/data/snomed",
#'   overwrite = TRUE
#' )
#'
#' # Read the downloaded data (extracts on-demand)
#' snomed <- read_snomed_ct_uk_monolith(snomed_zip)
#' }
get_snomed_ct_uk_monolith <- function(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
) {
  # Validate release argument
  if (!is.character(release) || length(release) != 1 || release == "") {
    cli::cli_abort(c(
      "x" = "{.arg release} must be a non-empty character string",
      "i" = "Use {.val latest} for the most recent release or a specific release ID"
    ))
  }

  # Constants
  trud_item <- 1799

  # Helper for conditional messaging
  msg <- function(...) if (!quiet) cli::cli_inform(...)

  msg("Getting SNOMED CT UK Monolith Edition (TRUD item {trud_item})")

  # Check whether target directory exists
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  # Get metadata to determine expected filename
  msg("Fetching release metadata from NHS TRUD...")

  # Determine release_scope for metadata query
  release_scope <- if (release == "latest") "latest" else "all"

  metadata <- trud::get_item_metadata(trud_item, release_scope = release_scope)

  # Select the requested release
  if (release == "latest") {
    selected_release <- metadata$releases[[1]]
  } else {
    # Find matching release
    release_ids <- purrr::map_chr(metadata$releases, "id")
    match_idx <- which(release_ids == release)

    if (length(match_idx) == 0) {
      cli::cli_abort(c(
        "x" = "Release {.val {release}} not found for TRUD item {trud_item}",
        "i" = "To see available releases, run:",
        " " = "{.code metadata <- trud::get_item_metadata({trud_item}, release_scope = 'all')}",
        " " = "{.code purrr::map_chr(metadata$releases, 'id')}"
      ))
    }

    selected_release <- metadata$releases[[match_idx]]
  }

  # Determine target zip file path
  archive_name <- selected_release$archiveFileName
  target_zip <- file.path(dir_path, archive_name)

  # Handle existing zip file
  if (file.exists(target_zip)) {
    if (!overwrite) {
      msg(c(
        "v" = "SNOMED CT UK Monolith already exists at {.file {target_zip}}.",
        "i" = "Set {.code overwrite = TRUE} to redownload if needed."
      ))
      return(invisible(target_zip))
    } else {
      msg(c(
        "i" = "File exists, but {.code overwrite = TRUE} is set.",
        "v" = "Redownloading and overwriting."
      ))
    }
  }

  # Download zip file
  msg("Downloading {.file {archive_name}} to {.file {dir_path}}...")
  zip_filepath <- trud::download_item(
    trud_item,
    directory = dir_path,
    release = if (release == "latest") NULL else release,
    overwrite = overwrite
  )

  msg(c(
    "v" = "SNOMED CT UK Monolith downloaded to {.file {zip_filepath}}"
  ))

  invisible(zip_filepath)
}
