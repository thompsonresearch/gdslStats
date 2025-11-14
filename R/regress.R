#' OLS and Logistic Regression
#'
#' @param x a data.frame or tibble
#' @param f a regression formula
#' @param method character. Regression method - one of "ols" or "logistic". Default is "ols".
#'
#' @return The output from an OLS regression model fitted using lm( ), of from a logistic regression model fitted using glm( family=binomial(link = "logit" ) )
#' @export
#'
#' @examples
#' # Convert Sex from a two-category factor variable to a logical variable,
#' # so that it can be used as outcome for OLS regression
#' survey$Female <- dplyr::if_else( survey$Sex == "Female", TRUE, FALSE )
#'
#' # Fit default regression model (OLS)
#' survey |> regress( Female ~ Height )
#'
#' # Fit a logistic regression model
#' survey |> regress( Female ~ Height, "logistic" )
#'
regress <- function( x, f, method = "ols" ) {

  # Check that x is a data.frame or tibble
  if ( "data.frame" %in%  class( x ) == FALSE )
    stop( "x is not a data.frame or tibble", call. = F )

  # Check that f is a formula
  if ( purrr::is_formula( f ) == FALSE )
    stop( "f is not a formula", call. = F )

  # If method is OLS AND outcome variable is character or factor,
  # give a warning that only two category logical variables acceptable
  if( class( x[ , all.vars(f)[1]  ] ) %in% c( "numeric", "logical" ) == FALSE )
    stop( "Variable on left-hand side of formula must be numeric or logical" )

  method <- tolower(method)
  if ( method %in% c("ols", "logistic") == FALSE )
    stop( "method must be either ols or logistic", call. = F )

  if ( method == "ols" )
    res <- stats::lm( f, data = x )

  if ( method == "logistic" )
    res <- stats::glm( f, data = x, family = stats::binomial( link = "logit" ) )

  return( res )

}
