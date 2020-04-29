#' A function for descriptive statistics
#'
#'
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
