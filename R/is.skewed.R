#' Check whether a vector is skewed ( abs(skew) < threshold )
#'
#' @param x a numeric vector
#' @param threshold a numeric value (default 0.5).
#'
#' @return logical (TRUE or FALSE). TRUE if absolute value of skew is greater than the threshold; else FALSE.
#' @export
#'
#' @examples
#' # Check to see whether a given variable is skewed
#' is.skewed( survey$Income )
#'
#' # Use is.skewed to select only the skewed variables in a given data.frame
#' survey |> dplyr::select_if( is.skewed ) |> names( )
#'
is.skewed <- function( x, threshold = 0.5 ) {

  if ( is.null( dim(x) == FALSE ) ) stop( "x is not a vector", call. = FALSE )

  if ( is.numeric(x) == FALSE ) {
    FALSE
  } else {
    abs( gdslStats::skew(x, na.rm=TRUE) ) > threshold
  }

}
