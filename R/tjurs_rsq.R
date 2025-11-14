#' Tjur's R-squared - a measure of fit for logistic regression models
#'
#' @param x a glm logit model
#'
#' @return numeric - Tjur's r squared, expressed as a percentage
#' @export
#'
#' @examples
#' # Fit glm model
#' model <- glm( Sex ~ Age, data = survey,
#'               family = binomial( link = "logit" ) )
#' tjurs_rsq(  model )
#'
tjurs_rsq <- function(x) {
  #Tjur's Coefficient of Determination (after Allison, 2014)
  df <- data.frame(y= x$model[,1], p=x$fitted.values)
  tmp <- stats::aggregate(p ~ y, data=df, FUN=mean)
  return( abs(tmp$p[1] - tmp$p[2]) * 100 )
}
