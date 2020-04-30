#' The box plot value
#'
#' @param x vector of sample data from population 1
#'
#' @return This function returns a list of 6 components:
#' n: the number of observation the boxplot is drawn with
#'
#' conf: upper/lower extremes of the notch, out-value of the outliers
#'
#' group: a vector of the same length that
#' indicate to which group the outlier belongs to.
#'
#' names: a vector of names for the groups.
#'
#'
#' @export
#'
#' @examples
#' myboxplotvalue (x)
boxvalue <- function(x){
  xvalue <- boxplot(x, type = "p", xlab = "Sample Data of population 1",
                    col = "dark red", horizontal = TRUE)

  return(xvalue)
}
