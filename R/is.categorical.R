#' Check whether vector is categorical
#'
#' @param x a vector
#' @param num numeric (default = 20). Numeric vectors with fewer than num unique values will be classed as categorical.
#'
#' @return TRUE if vector is factor, character, logical, or a numeric vector with less than num unique values; else FALSE.
#' @export
#'
#' @examples
#' # Applied to a factor vector
#' is.categorical( survey$Age )
#'
#' # Applied to a logical vector
#' is.categorical( c( TRUE, FALSE, TRUE, TRUE, FALSE ) )
#'
#' # Applied to a character vector
#' is.categorical( c( "A", "B", "C", "A", "B", "C" ) )
#'
#' # Applied to a numeric vector with less than num = 20 unique values
#' is.categorical( c( 1, 2, 3, 4, 5 ) )
#'
#' # Applied to a numeric vector with less than num = 0 unique values
#' is.categorical( c( 1, 2, 3, 4, 5 ), num = 0 )
#'
#' # Used to select the categorical variables in a data.frame
#' survey |> dplyr::select_if( is.categorical ) |> names( )
#'
is.categorical <- function( x, num = 20 ) {

  if ( is.null( dim(x) == FALSE ) ) stop( "x is not a vector", call. = FALSE )

  if ( is.numeric(x) == TRUE ) {
    if ( length( unique(x) ) <= num ) {
      TRUE
    } else {
      FALSE
    }
  } else if ( is.factor(x) | is.logical(x) | is.character(x) ) {
    TRUE
  } else {
    FALSE # e.g. because is array or data.frame etc
  }

}
