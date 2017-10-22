#' Function to launch ascr shniy application.
#' Looks for the ascr app within the package directory.
#' No arguments are required.


#' @export
launch.ui <- function() {
  appDir <- system.file("shiny-examples", "ascr", package = "ascr")
  if (appDir == "") {
    stop("Looks like the ascr app is not available. Try re-installing the `ascr`package." , call. = FALSE)
  }
  if(!require(shiny) | !require(rmarkdown) | !require(shinyjs) |
     !require(shinycssloaders) | !require(shinythemes) | !require(animation)){
      stop("To run the ascr user interface please ensure the following packages are installed: shiny, shinyjs, shinythemes, shinycssloaders, rmarkdown, animation")
  }

  runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
