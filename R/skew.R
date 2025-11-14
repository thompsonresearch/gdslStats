#' @title Skew
#'
#' @param x A \emph{numeric} vector.
#' @param na.rm If \code{TRUE} observations with a missing value of \code{x} are removed before applying the test. Default is \code{FALSE}.
#'
#' @description Calculates the skew of a set of \emph{numeric} values (assumed to be from a sample).
#'
#' @note A near direct copy of solution posted by Wolfgang Koller at \url{https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html}.
#'
#' @return \emph{Numeric}. Returns the value of the calculated skew.
#' @export
#'
#' @examples
#' # Calculate the skew of a sample
#' skew( survey$Height )
#'
skew <-  function(x, na.rm=FALSE) {
  #Near direct copy of solution posted by Wolfgang Koller
  #https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html

  # If data is one column data.frame, convert to vector; report error if
  #  data is a 2+ column data.frame
  if ( is.data.frame( x) ) {
    if ( ncol( x ) > 1 )
      stop("Only vectors or single column data.frames/tibbles accepted as ",
           "as valid data input")
    # Use dplyr::pull to ensure that any single column data.frames supplied
    # via dplyr::select( ) are treated as vectors instead
    data <- dplyr::pull( x )
  }

  # Strip out any missing values
  if ( na.rm == TRUE ) {
    x <- x[ !is.na(x) ]
  }

  # Only proceed if data are numeric
  if( is.numeric(x) == FALSE )
    stop( "Skew can only be caclulated for numeric data.", call. = F )

  # Calculate skew
  m3 <- mean( ( x - mean( x ) )^3 )
  skew <- m3 / ( sd( x )^3 )

  # Return result
  return( skew )

}
