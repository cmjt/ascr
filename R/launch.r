#' Function to launch ascr shniy application.
#' looks for the ascr app within the package directory,
#' no arguments required.

#' @importFrom shiny runApp
#' @export
launch <- function() {
  appDir <- system.file("shiny-examples", "ascr", package = "ascr")
  if (appDir == "") {
    stop("Looks like the ascr app is not available. Try re-installing the `ascr`package." , call. = FALSE)
  }

  runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
