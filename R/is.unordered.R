#' Check to see if vector is unordered
#'
#' @param x a vector
#'
#' @return TRUE if vector is logical, character, or a factor that is not ordered; else FALSE.
#' @export
#'
#' @examples
#' is.unordered( survey$Sex )
#' is.unordered( survey$Age )
#' survey |> dplyr::select_if( is.unordered ) |> names( )
#'
is.unordered <- function( x ) {

  if ( is.null( dim(x) == FALSE ) ) stop( "x is not a vector", call. = FALSE )

  if ( is.numeric(x) == TRUE ) {
    FALSE
  } else if ( is.ordered(x) ) {
    FALSE
  } else if ( is.factor(x) | is.logical(x) | is.character(x) ) {
    TRUE
  } else {
    FALSE # e.g. because is array or data.frame etc
  }
}
