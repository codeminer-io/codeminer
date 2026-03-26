#' Get Read 3 (CTV3) coding system files from NHS TRUD
#'
#' Downloads the Read Codes Version 3 (CTV3) release (TRUD item 19) from NHS
#' TRUD.
#'
#' @param dir_path Directory path to download to. Defaults to `tempdir()`.
#' @param release Character string specifying which release to download. Can be:
#'   * `"latest"` (default) — downloads the most recent release
#'   * A specific release identifier string
#' @param overwrite Logical. If `TRUE`, re-downloads and overwrites existing
#'   files. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded Read 3 zip file (invisibly).
#'
#' @details
#' This function requires a valid NHS TRUD API key set as the environment
#' variable `TRUD_API_KEY`. You must also be subscribed to item 19 on the NHS
#' TRUD website.
#'
#' @seealso [read_read3_trud()], [add_read3_trud()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_read3_trud()
#' read_read3_trud(path)
#' }
get_read3_trud <- function(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
) {
  if (!is.character(release) || length(release) != 1 || release == "") {
    cli::cli_abort(c(
      "x" = "{.arg release} must be a non-empty character string",
      "i" = "Use {.val latest} for the most recent release or a specific release ID"
    ))
  }

  trud_item <- 19L

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  msg("Getting Read Codes Version 3 (CTV3) (TRUD item {trud_item})")

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  release_scope <- if (release == "latest") "latest" else "all"
  metadata <- trud::get_item_metadata(trud_item, release_scope = release_scope)

  if (release == "latest") {
    selected_release <- metadata$releases[[1]]
  } else {
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

  archive_name <- selected_release$archiveFileName
  target_zip <- file.path(dir_path, archive_name)

  if (file.exists(target_zip) && !overwrite) {
    msg(c(
      "v" = "Read 3 already exists at {.file {target_zip}}.",
      "i" = "Set {.code overwrite = TRUE} to redownload if needed."
    ))
    return(invisible(target_zip))
  }

  msg("Downloading {.file {archive_name}} to {.file {dir_path}}...")
  zip_filepath <- trud::download_item(
    trud_item,
    directory = dir_path,
    release = if (release == "latest") NULL else release,
    overwrite = overwrite
  )

  msg("Read 3 downloaded to {.file {zip_filepath}}")

  invisible(zip_filepath)
}
