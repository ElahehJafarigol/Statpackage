#' A function for t tests
#' importFrom("stats", "rnorm", "t.test", "var.test")
#'
#'
#' The constructor function is based on t-test, that evaluates the NUll hypothesis H0.
#' H0 (The Null Hypothesis) claims that two populations have the same mean.
#'
#' x and y have underlying normal distribution.
#'
#' @param x vector of sample data from population 1
#' @param y vector of sample data from population 2
#'
#' @param paired TRUE or FALSE,
#' paired = FALSE by default
#' The samples are paired = TRUE, if each experimental unit is measured twice.
#' In that case the first experiments creates sample x,
#' and the second experiment creates sample y.
#' The two-sided t-test is used for paired sapmles.
#'
#' The test returns an error if the samples have different size.
#'
#' @param alpha alpha level is always between 0,1,
#' which determines the confidence interval for the P-value.
#'
#'
#' @return The test return a list containing the data, and t test object.
#' @export
#'
#' @examples
#' myttest(x = rnorm(30,10,12), y = rnorm(40, 7, 10))
myttest <- function(x, y, paired=FALSE, alpha=0.05){
  if(paired == "FALSE"){ #If the samples are not paired then use a t-test.
    vt = var.test(x,y)

    if(vt$p.value > alpha){
      tt <- t.test(x, y, var.equal = TRUE, conf.level = 1-alpha)
    }
    else{
      tt <- t.test(x, y, var.equal = FALSE, conf.level = 1-alpha)
    }
  }
  else{ #If the samples are paired then check the lengths.
    #If the lengths are different the algorithms stops.

    if (length(x) != length(y)){
      print("The sample sizes are not the same.")
    }
    if (length(x) == length(y)){
      tt <- t.test(x, y, paired=TRUE, conf.level=1-alpha, alternative = "two.sided")
    }

  }

  data = c(x,y)
  v = rep(c("x","y"), c(length(x),length(y))) # Creation of qual var
  df = data.frame(data=data, v=v)
  Rttest = list(ttest=tt, df=df, paired = paired)
  class(Rttest) <- "mytt" #New class
  print (Rttest)

}
