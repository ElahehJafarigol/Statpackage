#' A function to plot x, y
#' This plot, shows the distribution, mean and standard deviation of the data in seperate box plots.
#' importFrom("graphics", "boxplot", "par")
#'
#'
#' @param x vector of sample data from population 1
#' @param y vector of sample data from population 2
#'
#' @return 2 plots, for sample one and two
#' @export
#'
#'
#' @examples
#' x = rnorm(30,10,12); y = rnorm(40,20,15); myplot(x,y)
library(ggplot2)

myplot <- function(x, y){
  plot(x, type = "p", xlab = "Sample Data of population 1", col = "dark red")
  par(new=FALSE)
  plot(y, type = "p", xlab = "Sample Data of population 2", col = "dark blue")
}


