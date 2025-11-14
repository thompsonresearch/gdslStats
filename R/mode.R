#' Mode (modal value)
#'
#' @param x A vector or one-column data.frame.
#' @param na.rm If TRUE (default) observations with a missing value of \code{x} are removed before applying the test.
#'
#' @description Returns the modal value of a set of numerical or categorical values.
#'
#' @note Modified version of \url{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode} that is able to handled tied modal categories.
#'
#' @return Returns the modal value (category)
#' @export
#'
#' @examples
#' # Find modal category of a categorical variable
#' mode(survey$Age)
#'
#' # Tied modal values
#' mode(c(1, 2, 2, 3, 3, 4))
#'
#' # Default handling of missing values
#' mode( c(1, 2, 3, 3, NA, NA, NA) )
#'
#' # Mode excluding missing values
#' mode( c(1, 2, 3, 3, NA, NA, NA), na.rm=TRUE)
#'
mode <- function(x, na.rm = FALSE) {
  #Function to find and return the modal cateogry(ies)
  #with an option to include/exclude NAs
  #Adding the capability of handling tied modal categories to Gregor's suggested function at
  #http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode

  # If x is one column data.frame, convert to vector; report error if
  #  x is a 2+ column data.frame
  if ( is.data.frame( x) ) {
    if ( ncol( x ) > 1 )
      stop("Only vectors or single column data.frames/tibbles accepted as ",
           "as valid data input")
    # Use dplyr::pull to ensure that any single column data.frames supplied
    # via dplyr::select( ) are treated as vectors instead
    data <- dplyr::pull( x )
  }

  if(na.rm){
    x <- x[ !is.na(x) ]
  }

  ux <- unique(x)

  tx <- tabulate( match( x, ux ) )

  return( ux[ which( tx == max( tx ) ) ] )

}
