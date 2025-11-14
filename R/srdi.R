#' Standardised Reciprocal Diversity Index
#'
#' @param x A \emph{character} or \emph{factor} vector. If one or more categories lack observations, see Details.
#' @param na.rm If \code{FALSE} (default) observations with a missing value of \code{x} are NOT removed. If \code{TRUE}, missing values of \code{x} ARE removed.
#'
#' @details   Calculates the value of the SRDI assuming that there are no categories without observations. If there are, supply the data as a factor which includes the empty categories as named factor levels.
#'
#' @return \emph{Numeric}. Returns the value of the calculated Standardised Reciprocal Diversity Index
#' @export
#'
#' @examples
#' # Calculate SRDI when values evenly spread across categories
#' x <- factor( c("A", "A", "B", "B", "C", "C"), levels = LETTERS[1:3] )
#' srdi( x )
#'
#' # Calculate SRDI when values are unevenly spread across categories
#' x <- factor( c("A", "B", "C", "C", "C", "C"), levels = LETTERS[1:3] )
#' srdi( x )
#'
#'# Calculate SRDI when all values are maximally concentrated
#' # (Note the need to declare the empty categories as factor levels)
#' x <- factor( c("A", "A", "A", "A", "A", "A"), levels = LETTERS[1:3] )
#' srdi( x )
#'
srdi <- function(x, na.rm=TRUE) {

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

  # Check that the variable supplied is categorical (i.e. factor or character)
  if ( !is.factor( x ) & !is.character( x ) ) {
    stop( "Error: SRDI can only be calculated for categorical variables ",
          "(i.e. factor or character variables.", call. = F )
  }

  # If removal of NA values requested, drop NA values from x
  if ( na.rm == TRUE ) {
    x <- x[ !is.na( x ) ]
  }

  # If inclusion of NA values requested, convert NA values to level
  if (na.rm==FALSE) {
    x <- forcats::fct_na_value_to_level( x, level = NA )
  }

  # Convert supplied variable to a factor if necessary
  if ( !is.factor( x ) ) {
    x <- factor( x )
  }

  # Find the number of categories (K) in the resulting factor variable

  K <- length( levels(x) )

  # Find the frequency distribution, including
  # for empty categories (which is why the variable needs
  # to be a factor)

  f <- table( x )


  # Calculate the squared proportions of these frequencies
  # [some people calls this Simpson's Index of Diversity;
  # Agresti calls 1 - p_sq Simpson's Index of Diversity]

  p_sq <- (  f/ sum( f ) )^2


  # Calculate the Reciprocal Diversity Index

  RDI <- 1 / sum( p_sq )


  # Calculate the standardised RDI
  #
  #  Max value of RDI = number of categories; min value = 1.
  #  Subtracting 1 from RDI gives range 0 to K-1
  #  Dividing by the new max value (K-1) gives RDI as proportion
  #  of max value
  #  Multiplying by 100 converts into a percentage with range 0 to 100

  SRDI <- ( RDI - 1 ) / ( K - 1 ) * 100

  return( SRDI )

}
