#' This is an example of a shiny package
#'
#'
#' Get into reactive programming with shiny
#' @return A shiny app for descriptive and inferential statistics
#' @export
#'
#' @examples
#' shinyapp()
shineyapp <- function(){
  shiny::runApp(system.file("shiny", package="Statpackage"),launch.browser = TRUE)
}
