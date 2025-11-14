#' @title Gelman standardisation
#'
#' @param x A numeric vector containing at least two elements
#'
#' @details A function to standardise a numeric variable to have a mean of 0 and an sd of 0.5, by subtracting the mean of variable and then dividing by 2 x standard deviation, ignoring any NA values. Based on the standardisation approach suggested by Gelman A and Hill J (2007) Data analysis using regression and multilevel/hierarchical models, Cambridge:Cambridge University Press, pp. 54-57.
#'
#' @return A numeric vector containing Gelman standardised values of x.
#' @export
#'
#' @examples
#' z_gelman( c(1:3, NA, 5:7, NA, 9:10) )
#'
#'#' # Create Gelman standardisd version of Height
#' survey |>
#'   dplyr::mutate( z_Height = z_gelman( Height ) ) |>
#'   dplyr::select( Height, z_Height ) |>
#'   dplyr::slice( 1:10 )
#'
z_gelman <- function(x) {

  # Check that supplied value is numeric
  if ( is.numeric( x) == FALSE )
    stop( "x is not numeric.", call. = F )

  # Check that supplied value is a vector containing at least two elements
  if ( length( x) < 2 )
    stop( "x contains fewer than 2 elements", call. = F )

  return( ( x - mean( x, na.rm = TRUE) ) /
             ( 2 * sd( x, na.rm = TRUE ) )
  )

}
