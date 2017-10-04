#' Function to launch ascr shniy application.
#' Looks for the ascr app within the package directory.
#' No arguments are required.

#' @importFrom shiny runApp
#' @export
launch.ui <- function() {
  appDir <- system.file("shiny-examples", "ascr", package = "ascr")
  if (appDir == "") {
    stop("Looks like the ascr app is not available. Try re-installing the `ascr`package." , call. = FALSE)
  }

  runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
