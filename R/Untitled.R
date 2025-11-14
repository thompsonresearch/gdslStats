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
    n <- sum(complete.cases(x, y))
    sort(tanh(atanh(rs) + c(-1,1)*sqrt((1+rs^2/2)/(n-3))*qnorm(p = alpha/2)))
  }

  ### Calculate correlation and, as required, associated p-value, sig and CIs

  ## For Cramer's V, all calculations handled via a call to
  ## gdslStats::cramersV_test

  if ( method == "cramer" )
    result <- cramersV_test( x, y,
                             p_value = p_value, sig = sig, ci = ci,
                             useNA = useNA )

  ## For Pearson's stats::cor.test provides values of r, p and CIs
  ## For Spearman's stats::cor.test provides values of r and p, but not CIs.
  ## Therefore Spearman's CIs calculated using internal function spearman_CI
  ## For both Pearson and Spearman, sig is calculated on-the-fly from p-values.

  if ( method %in% c("pearson", "spearman") ) {

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
