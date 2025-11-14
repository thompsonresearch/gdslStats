#' Calculate Cramer's V and associated p-value for a variable pair
#'
#' @param x a factor, character, logical or numeric vector (one of variables being correlated).
#' @param y a factor, character, logical or numeric vector (one of variables being correlated).
#' @param p_value logical (TRUE or FALSE), indicating whether results should include p-values. Default is FALSE.
#' @param sig logical (TRUE or FALSE), indicating whether results should include asterisks flagging 'significant' values (i.e. those with p-values less than 0.01, 0.05 and 0.1). Defaut is FALSE.
#' @param ci NULL (default) or a numeric value in the range 0 to 100 indicating what % confidence intervals are required. (e.g 95 for 95% CIs) NOTE: THIS FEATURE NOT YET ENABLED.
#' @param useNA a character string, one of "no" and "ifany" indicating whether NA should be treated as a category when calculating Cramer's V. Default is "no".
#' @param NaN_warning logical (default TRUE), to give warning if a Cramer's V value of NaN arises from an empty row/column in the cross-tabulation of x by y.
#'
#' @return a one-row data.frame containing the correlation plus any requested associated measures.
#' @export
#'
#' @examples
#' cramersV_test( survey$Age, survey$Sex, p_value = TRUE, sig = TRUE )
#'
cramersV_test <- function(x, y,
                          p_value = TRUE,
                          sig = FALSE,
                          ci = NULL,
                          useNA = "no",
                          NaN_warning = TRUE) {

  ### PART ONE:  Check validity of supplied values for function parameters
  ### [this needs to be expanded to cover all inputs]

  if ( tolower( useNA) %in% c("no", "ifany")  == FALSE )
    stop( "For CramersV useNA can only take take the values no or ifany",
          call. = F )
  useNA <- tolower( useNA )

  if ( is.numeric( ci ) )
    warning( "Confidence Intervals not yet implemented as a feature in ",
             "cramersV_test.", call. = F )

  ### PART TWO: Convert all x and y inputs to factors, so that they can be
  #             tabulated for use in chi-square

  #If x or y is NOT a factor, convert into a factor
  if ( !is.factor(x) ) {
    x <- as.factor(x)
  }

  if ( !is.factor(y) ) {
    y <- as.factor(y)
  }

  ### PART THREE: Calculate Cramer's V and associated p-value

  t <- table( x, y, useNA= useNA)

  if ( sum(t) > 0 & nrow( t ) > 1 ) {

    chisq.results <- suppressWarnings( stats::chisq.test( t, correct = FALSE ) )

    tol = .Machine$double.eps^0.5

    if ( any( abs(chisq.results$expected - 0) < tol) ) {

      if ( NaN_warning == TRUE  )
        warning( "The cross-tabulation of this variable pair ",
                 "produces a table containing one or more row or column ",
                 "totals of zero. Therefore a Cramer's V of NaN is returned.",
                 call. = F)

    } # 1+ expected val(s) = 0

    chisq <- chisq.results$statistic
    p.value <- chisq.results$p.value
    cramer <- sqrt( chisq / ( sum(t) * ( min( nrow(t), ncol(t) ) - 1) ) )
    # Can't use min(dim(t)) because doesn't count NA rows/cols in t

  } else {

    cramer <- NaN
    p.value <- NaN

    if ( sum(t) < .Machine$double.eps^0.5 )
      if ( NaN_warning == TRUE  )
        warning( "The cross-tabulation of this variable pair produces ",
                 "a table with a total of zero. Therefore Cramer's V of NaN ",
                 "is returned.", call. = F )

    if ( nrow( t )  < 2 )
      warning( "One or both variables have only one category. ",
               "Cramer's V cannot be calculated, so NA returned.",
               call. = F )

  }


  ### PART FOUR: Assemble required results, adding in sig if needed

  df <- data.frame( cramer = cramer )

  if ( p_value == TRUE ) df$p.value <- p.value

  if ( sig == TRUE ) {
    if ( is.nan( df$p.value ) == FALSE & is.na( df$p.value ) == FALSE ) {

      if (df$p.value <= 0.01) {
        df$sig <- "**"
      } else if(df$p.value <= 0.05) {
        df$sig <- "*"
      } else {
        df$sig <- ""
      }

    } else {
      df$sig <- ""
    }
  }

  # Need to add code to calculate boot-strapped CIs if !is.null(ci)

  ## PART FIVE: return results, setting any acquired row names to NULL

  row.names(df) <- NULL

  return(df)

}
