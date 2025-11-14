#' Scatterplot matrix with specified variable on y-axis
#'
#' @param df A data.frame or tibble
#' @param outcome_name Character. The name of the variable to be plotted on the y-axis, in quotes.
#'
#' @return A list containing a series of scatterplots, one fewer than the number of variables in the supplied data.frame, with each plot having the variable identified by outcome_name on the y-axis and one of the other variables in the supplied data.frame on the x-axis. For scatterplots involving numeric x and y variables, the scatterplot includes an added linear line of best-ft.
#' @export
#'
#' @examples
#' survey |>
#'   outcome_plots( "Height" ) |>
#'   patchwork::wrap_plots( )
#'
outcome_plots <- function( df, outcome_name ) {

  # Check that supplied inputs are valid

  if ( "data.frame" %in% class(df) == FALSE )
    stop( "df needs to be a data.frame or tibble.", call. = F )

  if ( is.character( outcome_name ) == FALSE )
    stop( "outcome_name must be supplied within quote marks as a character string.", call. = F )

  if ( outcome_name %in% names( df ) == FALSE )
    stop( paste0( outcome_name, " is not a variable present in the supplied data.frame" ), call. = F )

  # Declare function used to create the scatterplot of y ~ x

  plot_data_column <- function( df, column_name, outcome_name ) {

    p <- ggplot2::ggplot( data = df,
                          ggplot2::aes( x = .data[[column_name]],
                                        y = .data[[outcome_name]] ) ) +
      ggplot2::geom_point( ) +
      ggplot2::xlab( paste( "%", column_name ) ) +
      ggplot2::ylab( paste( "%", outcome_name ) ) +
      ggplot2::theme_classic( )

    if ( is.numeric( df[ , column_name ] ) ) {

      p <- p + ggplot2::geom_smooth( method = "lm",
                                     colour = "red",
                                     linetype = "dashed",
                                     se = FALSE )
    }

    return( p )

  }

  # Create a list of plots, one plot per variable in the supplied data.frame

  my_plots <- lapply( stats::setNames( colnames( df ), colnames( df ) ),
                      plot_data_column,
                      df = df,
                      outcome = outcome_name
  )

  # Return the created list of plots, excluding the plot of y ~ y,
  # which is of no interest

  return( my_plots[ -which( colnames( df ) == outcome_name )] )

}
