#' @title Start Plumber API
#' @description Exposes API endpoints for dummy clinical events and code descriptions.
#'
#' @param host The host IP (default: "0.0.0.0")
#' @param plumber_file Path to plumber.R
#' @param port The port number (default: 8000)
#'
#' @export
start_api <- function(host = "0.0.0.0", port = 8000) {

    # Get the path to plumber.R inside the installed package
    plumber_file <- system.file("plumber/plumber.R", package = "codemapper")

    if (plumber_file == "") {
      stop("Could not find plumber.R. Ensure it is in inst/plumber/ inside the codeminer package.")
    }

  pr <- plumber::plumb(plumber_file)  # Load the Plumber API file
  pr$run(host = host, port = port)
}
