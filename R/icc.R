#' @title Calculates the Intraclass correlation
#'
#' @description The function calculates the Intraclass correlation
#' based on the results of the `aov` function
#' @param depvar dependent variable, must be numeric
#' @param indvar independent variable, must be categorical
#'
#' @return returns the Intraclass correlation
#' @export
#'
#' @examples
#' icc(depvar = iris$Sepal.Length, indvar = iris$Species)
icc <- function(depvar, indvar) {
  ## Function to calculate the Intraclass correlation based on an
  #   Analysis of Variance (aov)

  # depvar should be measurement level scale
  # indvar should be measurement level categorical
  formulaVar <- stats::formula(paste("depvar"," ~ ", "indvar"))
  sumSq <- stats::anova(stats::aov(formulaVar))$`Sum Sq`
  icc <- base::sqrt(sumSq[1]/sum(sumSq))
  return(icc)
  }

