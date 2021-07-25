# Launcher definition for the TownforgeR web interface
# cheat-sheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

#' Launch TownforgeR shiny app
#'
#' Description
#'
#' @export
shinyTF <- function(url.townforged = "http://127.0.0.1:18881/json_rpc") {
  appDir <- system.file("shiny-app", package = "TownforgeR")
  if (appDir == "") {
    stop("Could not find TownforgeR app. Try re-installing 'TownforgeR'.", call. = FALSE)
  }
  
  shiny::shinyOptions(url.townforged = url.townforged)
  # So the argument url.townforged given to launch the Shiny app is passed down to the server as url.townforged
  # Passes url argument down to the server function
  # Thanks to https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  shiny::shinyOptions(usecairo = TRUE)
  
  thematic::thematic_shiny(font = "auto")
  # ^ This is crucial to make the dark mode work for the plots
  
  shiny::runApp(appDir, display.mode = "normal")
}
