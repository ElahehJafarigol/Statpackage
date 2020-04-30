library(vioplot)
library(zoo)
#' A function to create a violin plot to compare the two samples
#'
#' @param x : vector of sample data from population 1
#' @param y : vector of sample data from population 2
#'
#' @return A violin
#' @export
#'
#' @examples
violinplot <- function(x, y){
  title <- ("Violin plot of sample x and y")
  vioplot(x, y, names=c("Sample x", "Sample y"), main = title, col="gold")
}

