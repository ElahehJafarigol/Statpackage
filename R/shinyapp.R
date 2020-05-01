#' This is an example of a shiny package
#'
#'
#' Get into reactive programming with shiny
#' @return A shiny app for descriptive and inferential statistics
#' @export
#'
#' @examples
#' shineyapp()
shineyapp <- function(){
  shiny::runApp(system.file("app", package="Statpackage"),launch.browser = TRUE)
}
