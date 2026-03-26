#' Get Read 2 coding system files from NHS TRUD
#'
#' Downloads the NHS Data Migration release (TRUD item 9), which contains the
#' Read V2 lookup and Read 2/CTV3 cross-mapping files.
#'
#' This is a convenience wrapper around [get_nhs_data_migration()], since both
#' the Read 2 files and the NHS Data Migration mapping tables come from the same
#' TRUD item.
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
#' @return File path to the downloaded NHS Data Migration zip file (invisibly).
#'
#' @details
#' This function requires a valid NHS TRUD API key set as the environment
#' variable `TRUD_API_KEY`. You must also be subscribed to item 9 on the NHS
#' TRUD website.
#'
#' @seealso [read_read2_trud()], [add_read2_trud()], [get_nhs_data_migration()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_read2_trud()
#' read_read2_trud(path)
#' }
get_read2_trud <- function(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
) {
  get_nhs_data_migration(
    dir_path = dir_path,
    release = release,
    overwrite = overwrite,
    quiet = quiet
  )
}
