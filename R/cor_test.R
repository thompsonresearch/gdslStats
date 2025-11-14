#' Calculate a pair-wise correlation and its associated p-value, significance and Confidence Interval
#'
#' @param x a numeric, character or factor vector (one of the two sets of values being correlated)
#' @param y a numeric, character or factor vector (one of the two sets of values being correlated)
#' @param method a character string indicating the type of correlation to be calculated. One of "pearson", "spearman" or "cramer".
#' @param p_value logical (TRUE/FALSE), indicating whether or not p-value should be calculated. Default is FALSE.
#' @param sig logical (TRUE/FALSE), indicating whether or not the sigificance of p-value should be calculated. Default is FALSE.
#' @param ci default is NULL. Otherwise a numeric value between 0 and 100 indicating the type of Confidence Interval required (e.g. 95% CI).
#' @param useNA a character string, one of "no" and "ifany" indicating whether NA should be treated as a category when calculating Cramer's V. Default is "no".
#'
#' @return a one-row data.frame containing the pairwise correlation value, plus p_value, sig and ci if requested.
#' @export
#'
#' @examples
#' # A simple pairwise correlation
#' cor_test( survey$Income, survey$Height, method = "pearson" )
#'
#' # Same correlation with added information
#' cor_test( survey$Income, survey$Height, method = "pearson",
#'           p_value = TRUE, sig = TRUE, ci = 95 )
#'
#' # Correlation of two categorical variables, ignoring any NA values
#' df <- survey
#' df[ 1:100 , "Sex"] <- NA
#' cor_test( df$Age, df$Sex, method = "cramer" )
#'
#' # Correlation of two categorical variables, treating NA as a valid category
#' cor_test( df$Age, df$Sex, method = "cramer", useNA = "ifany" )
#'
cor_test <- function( x, y,
                      method,
                      p_value = FALSE,
                      sig = FALSE,
                      ci = NULL,
                      useNA = "no" ) {

  ## Declare required internal function
  spearman_CI <- function(x, y, ci = 95 ){
    alpha <- ( 100 - ci ) / 100
    rs <- stats::cor(x, y, method = "spearman", use = "pairwise.complete.obs" )
    n <- sum( stats::complete.cases(x, y) )
    sort( tanh( atanh(rs) +
                  c(-1,1)*sqrt((1+rs^2/2) / (n-3)) *
                  stats::qnorm(p = alpha/2)
    )
    )
  }

  ### Calculate correlation and, as required, associated p-value, sig and CIs

  ## For Cramer's V, all calculations handled via a call to
  ## gdslStats::cramersV_test

  if ( method == "cramer" )
    result <- gdslStats::cramersV_test( x, y,
                                        p_value = p_value, sig = sig, ci = ci,
                                        useNA = useNA )

  ## For Pearson's stats::cor.test provides values of r, p and CIs
  ## For Spearman's stats::cor.test provides values of r and p, but not CIs.
  ## Therefore Spearman's CIs calculated using internal function spearman_CI
  ## For both Pearson and Spearman, sig is calculated on-the-fly from p-values.

  if ( method %in% c("pearson", "spearman") ) {

    # Convert any ordered factors to numeric
    if ( is.ordered( x ) ) x <- as.numeric( x )
    if ( is.ordered( y ) ) y <- as.numeric( y )

    tmp <- stats::cor.test( x, y, method = method,
                            na.omit = "na.action", exact = FALSE )

    result <- data.frame( r = tmp$estimate )
    names( result ) <- method

    if ( p_value == TRUE ) result$p.value <- tmp$p.value

    if ( sig == TRUE ) {
      if (tmp$p.value <= 0.01) {
        result$sig <- "**"
      } else if(tmp$p.value <= 0.05) {
        result$sig <- "*"
      } else {
        result$sig <- ""
      }
    }

    if ( !is.null(ci) ) {

      lb <- paste0( ci, "pct.ci.lb" )
      ub <- paste0( ci, "pct.ci.ub" )

      if (method=="pearson") {

        result[ , lb] <- tmp$conf.int[[1]]
        result[ , ub] <- tmp$conf.int[[2]]

      } else if (method == "spearman") {

        result[ , lb] <-
          spearman_CI( x , y )[1]
        result[ , ub] <-
          spearman_CI( x, y )[2]

      }

    } # if !is.null(ci)

  } # if pearsons or spearman

  ### Return result as a one-row data.frame, dropping any acquired rownames

  rownames( result ) <- NULL
  return( result )

}
