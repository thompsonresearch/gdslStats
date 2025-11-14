#' @title Visualisation of the impact of sample size on the 95% CIs of percentages
#'
#' @param pct_name Character. Name of percentage - eg. Unemployed.
#' @param p Numeric. A percentage - e.g. 34
#' @param y_min Numeric. Minimum y-axis value. Default value of `NULL` results in `y_min` being set to the lowest confidence interval value, rounded down to the nearest integer.
#' @param y_max Numeric. Maximum y-axis value. Default value of `NULL` results in `y_max` being set to the highest confidence interval value, rounded up to the nearest integer.
#'
#' @return A ggplot graph visualisng the 95% Confidence Interval for the supplied value of `p` for a set of sample sizes ranging from 250 to 10,000.
#' @export
#'
#' @examples
#' # Default y-axis limits
#' sample_size_and_CIs( "Unemployed", 34 )
#'
#' # User-defined y-axis limits
#' sample_size_and_CIs( "Unemployed", 34, 0, 100 )
sample_size_and_CIs <- function( pct_name, p, y_min = NULL, y_max = NULL ) {

  # Create a set of sampling sizes
  n <- seq(250, 10000, 250)
  size_n <- length(n)

  # Declare data.frame ready to store sample-size specific results
  df <- data.frame( n = NULL,
                    std_error = NULL,
                    CI_95pct_lower_bound = NULL,
                    CI_95pct_upper_bound = NULL )

  # For each sample size, calculate and store key sampling results
  for (i in 1:size_n) {
    df[i, "n"] <- n[i]
    df[i, "std_error"] <- ( (p*(100-p)) / n[i] )^0.5
    df[i, "CI_95pct_lower_bound"] <- p - (1.96 * df$std_error[i])
    df[i, "CI_95pct_upper_bound"] <- p + (1.96 * df$std_error[i])
  }

  # Add a variable alternating colour between sample sizes
  df$LineColour = rep( c("A", "B"), 20 )

  # Identify min and max y-axis limits, if not pre-supplied by user
  if ( is.null( y_min ) )
    y_min <- min( df$CI_95pct_lower_bound )

  if ( is.null( y_max ) )
    y_max <- max( df$CI_95pct_upper_bound )

  # Round y_min down to to nearest integer
  y_min_integer <- floor( y_min )

  # Round y_max up to nearest integer
  y_max_integer <- ceiling( y_max )

  # Calculate y-axis range
  y_range_integer <- y_max_integer - y_min_integer

  # Calculate sets of possible break values, based on
  # differing interval values between tick-marks
  y_breaks <- list( seq( y_min_integer, y_max_integer, 10 ),
                    seq( y_min_integer, y_max_integer, 5 ),
                    seq( y_min_integer, y_max_integer, 2 ),
                    seq( y_min_integer, y_max_integer, 1 ),
                    seq( y_min_integer, y_max_integer, 0.5 ),
                    seq( y_min_integer, y_max_integer, 0.2 ),
                    seq( y_min_integer, y_max_integer, 0.1 ) )

  # Extract a vector recording the length of each set of breaks
  y_br <- unlist( lapply( y_breaks, length ) )

  # Select the set of breaks which has a length closest to 14
  n_y_br <- which( abs(y_br - 14) == min( abs(y_br - 14)) )

  # If multiple sets of breaks have the same length, select the first
  # of these (i.e. the one with the largest tick mark interval)
  if (length( n_y_br) > 1 ) n_y_br <- n_y_br[1]

  # Extract the breaks from the chosen set of breaks
  y_breaks <- y_breaks[[ n_y_br]]

  # Set type of line end to use in graph
  line_end <- grid::arrow( angle = 90, type= "closed",
                           length = grid::unit( 0.1, "cm" ) )

  # Create graph
  plot1 <- ggplot2::ggplot( data = df ) +
    ggplot2::geom_segment( ggplot2::aes(
      x = n, xend = n,
      y = p, yend = CI_95pct_lower_bound,
      colour = LineColour ),
      arrow = line_end ) +
    ggplot2::geom_segment( ggplot2::aes(
      x = n, xend = n,
      y = p, yend = CI_95pct_upper_bound,
      colour = LineColour ),
      arrow = line_end ) +
    ggplot2::geom_point( ggplot2::aes( x = n, y = p ), colour = "red" ) +
    ggplot2::ylab( paste( "% ", pct_name ) ) +
    ggplot2::xlab( "Denominator (Sample Size)" ) +
    ggplot2::coord_cartesian( ylim = c( y_min_integer, y_max_integer ) ) +
    ggplot2::scale_y_continuous( breaks = y_breaks ) +
    ggplot2::scale_x_continuous( breaks = seq( 0, 10000, 500 ) ) +
    ggplot2::scale_colour_manual( values = c( "A" = "cadetblue",
                                              "B" = "Black") ) +
    ggplot2::theme_light( ) +
    ggplot2::theme( axis.text.x =
                      ggplot2::element_text( angle = 335, hjust = 0 ) ) +
    ggplot2::theme( legend.position = "none" )

  # Return resulting graph
  return( plot1 )
}
