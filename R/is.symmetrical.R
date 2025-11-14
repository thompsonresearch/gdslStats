#' Check whether a vector is skewed ( abs(skew) < threshold )
#'
#' @param x a numeric vector
#' @param threshold a numeric value (default 0.5).
#'
#' @return logical (TRUE or FALSE). TRUE if absolute value of skew is less than or equal to the threshold; else FALSE.
#' @export
#'
#' @examples
#' # Check to see whether a given variable is symmetrical
#' is.symmetrical( survey$Height )
#'
#' #Use is.symmetrical to select only the skewed variables in a given data.frame
#' survey |> dplyr::select_if( is.symmetrical ) |> names( )
#'
is.symmetrical <- function( x, threshold = 0.5 ) {

  if ( is.null( dim(x) == FALSE ) ) stop( "x is not a vector", call. = FALSE )

  if ( is.numeric(x) == FALSE ) {
    FALSE
  } else {
    abs( gdslStats::skew(x, na.rm=TRUE) ) <= threshold
  }

}
