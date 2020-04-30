#' A function for descriptive statistics
#' The first step after creating the data or reading data from an csv file is descriptive analysis.
#' This function provides the user with information such as mean, standard deviation, and etc.
#' x = rnorm (n, mean, sd)
#' y = rnorm (m, mean, sd)
#'
#' @param x vector of sample data from the population
#'
#' @return The function returns mean,median,25th and 75th quartiles,min,max
#' @export
#'
#' @examples
#' mydata(x = rnorm (20,5,10))
mydata <- function(x){
  myx <- summary(x)
  return(myx)
}
