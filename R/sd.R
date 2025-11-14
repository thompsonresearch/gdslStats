#' @title Sample or Population Standard Deviation
#'
#' @param x A \emph{numeric} vector.
#' @param na.rm If \code{FALSE} (default) observations with a missing value of \code{x} are NOT removed before applying the test. If \code{TRUE}, missing values of \code{x} ARE removed before applying test.
#' @param sample If \code{TRUE} (default), calculation uses \emph{n}-1 in the denominator. If \code{FALSE}, uses \emph{n} instead, as per standard statistical theory.
#'
#' @description Calculates the standard deviation of a set of \emph{numeric} values, making an appropriate adjustment if using population rather than sample data. (The value returned by [stats::sd] only supports division by \emph{n}-1)
#'
#' @return \emph{Numeric}. Returns the value of the calculated standard deviation.
#' @export
#'
#' @examples
#' # Calculate standard deviation of a sample (default)
#' sd( survey$Height )
#'
#' # For a sample, std.dev returns same result as stats::sd function
#' sd( survey$Height ) == stats::sd( survey$Height )
#'
#' # Calculate standard deviation of a population
#' sd( survey$Height, sample = FALSE )
#'
sd <- function( x, na.rm = FALSE, sample = TRUE) {

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

  # Only proceed if data are numeric
  if( is.numeric(x) == FALSE )
    stop( "Standard deviation can only be caclulated for numeric data.",
          call. = F )

  # Strip out any missing values
  if ( na.rm == TRUE ) {
    x <- x[!is.na(x)]
  }

  # Calculate standard error
  SE <- (x - mean( x ) )^2

  # Find n, the number of observations
  n <- length( x )

  # If data are a sample, reduce size of n by 1
  if ( sample == TRUE ) {
    n <- n - 1
  }

  # Calcualte variance (MSE)
  MSE <- sum( SE ) / n

  # Calculate standards deviation (RMSE)
  RMSE <- MSE^0.5

  # Return calculated value
  return( RMSE )

}
