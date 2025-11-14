#' @title Median value of a numeric or ordinal variable
#'
#' @author Paul Williamson
#'
#' @param x A \emph{numeric}, \emph{factor} or \emph{character} vector.
#' @param na.rm If \code{FALSE} (default) observations with a missing value of \code{x} are NOT removed before applying the test. If \code{TRUE}, missing values of \code{x} ARE removed before applying test.
#'
#' @note A wrapper for the \code{\link{stats::median}} function supplied as part of the base R \code{stats} package, adding the ability to handle categorical (factor) vectors.
#'
#' @description Calculates the median of a set of \emph{numeric} or \emph{categorical} values, first converting any \emph{character} variable into a factor
#'
#' @return  Returns the calculated median value (\emph{Numeric}) or category (\emph{character}).
#' @export
#'
#' @examples
#' # Median of numeric vector
#' median( survey$Income )
#'
#' # Median of an ordinal vector (ordered factor)
#' median( survey$Age )
#'
#' # Median of a nominal vector (unordered factor) which includes missing values
#' median( survey$WorkStatus, na.rm = TRUE )
median <- function( x, na.rm = FALSE ) {

  na_rm <- na.rm

  # If x is one column data.frame, covert to vector; report error if
  # x is a 2+ column data.frame
  if ( is.data.frame( x ) ) {
    if ( ncol( x ) > 1 )
      stop("Only vectors or single column data.frames/tibbles accepted as ",
           "as valid data input")
    # Use dplyr::pull to ensure that any single column data.frames supplied
    # via dplyr::select( ) are treated as vectors instead
    data <- dplyr::pull( x )
  }


  if( is.numeric( x ) ) {
    res <- stats::median( x, na.rm = na_rm )
  } else {
    if( is.factor( x ) ) {
      med <- stats::median( as.numeric(x), na.rm = na_rm )
      res <- levels( x )[ med ]
      if ( !is.ordered(x) )
        warning( "x may be a nominal variable (unordered factor), in which ",
                 "case calculating a median value is not appropriate.",
                 call. = F )
    } else {
      if ( is.character( x ) )
        stop( "The median( ) function requires x to be either numeric or ",
              "a factor", call. = F )
    }
  }

  return( res )

}
