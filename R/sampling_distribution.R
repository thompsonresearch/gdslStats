#' @title Simulation of sampling distribution and Confidence Intervals for percentages
#'
#' @param x A character or factor vector or single-column data-frame or tibble.
#' @param category Character. Name of one of the categories/levels in \emph{x}.
#' @param no_of_samples Numeric. The number of samples to be taken from \emph{x}.
#' @param sample_size Numeric. The size of sample to be taken from \emph{x}.
#'
#' @details Calculates the percentage of the values of `x` that fall in the specified `category`.
#' Then takes `no_of_samples` of ` sample_size` from the supplied vector \emph{x}. For each sample,
#' calculates the percentage of the sample values which fall in the specified `category`, and calculates
#' the associated standard error (of proportions) and 95% confidence interval. Finally, summarises the
#' results in a series of plots and summary statistics. The purpose of the function is to allow the user
#' to explore how `no_of_samples` and `sample_size` interact to influence the accuracy of the
#' sampling distribution mean, plus the sample-specific calculated confidence intervals,
#' and the overall 95% CI coverage rates.
#'
#' @return A list comprising the following set of objects:
#'
#' * `samples`  a data-frame storing the sample-specific results
#' * `distribution_stats` sampling distribution summary statistics
#' * `distribution` a histogram of the sampling distribution, including the % of `category` observed in `x` and mean of the sample-based estimates of this quantity
#' * `coverage_graph` a plot illustrating the calculated CIs, and flagging in red those that do NOT cross include the observed value in `x`
#' * `coverage_rate` the 95% CI coverage rate (% of 95% CIs that include th observed value in `x`)
#'
#' @export
#'
#' @examples
#' # Generate a vector comprising values of "M" and "F"
#' x <- sample( c("M", "F"), 1000, replace = TRUE )
#'
#' # Find the sampling distribution for the percentage of values that fall in
#' # category "F", using 50 random samples of size 20
#' output <- sampling_distribution( x, "F", no_of_samples = 50, sample_size = 20 )
#'
#' # First few rows of the samples data.frame
#' output$samples |> head( )
#'
#' # Summary stats for sampling distribution
#' output$distribution_stats
#'
#' # Histogram of sampling distribution
#' output$distribution
#'
#' # Coverage graph
#' output$coverage_graph
#'
#' # Coverage rate
#' output$coverage_rate
#'
sampling_distribution <- function( x, category, no_of_samples, sample_size) {

  # Check input parameters are valid

  # Is the input data a data.frame or tibble?
  # If data is one column data.frame, convert to vector; report error if
  #  data is a 2+ column data.frame
  if ( is.data.frame( x ) ) {
    if ( ncol( x ) > 1 )
      stop("Only vectors or single column data.frames/tibbles accepted as ",
           "as valid data input", call. = F )
    # Use dplyr::pull to ensure that any single column data.frames supplied
    # via dplyr::select( ) are treated as vectors instead
    x <- dplyr::pull( x )
  }

  # If x is a string, convert variable from string to factor
  if ( is.character( x ) ) {
    x <- factor( x )
  }

  # Strip out any NA values (but not any "NA" values)
  x <- x[ is.na(x) == FALSE ]

  # Is category a valid level of supplied variable?
  if ( tolower( category ) %in% tolower( levels(x) ) == FALSE )
    stop( paste0( "The supplied category, ", category ,
                  ", does not match any of the available categories: ",
                  paste0( levels( x ), collapse = ' ') ),
          call. = F )

  # Create a data.frame to store sampling results
  df <- data.frame( sample_no = NULL,
                    n_category = NULL,
                    n_values = NULL,
                    pct_category = NULL,
                    se = NULL,
                    CI_95pct_lower_bound = NULL,
                    CI_95pct_upper_bound = NULL )

  # Calculate % in category for input data, x
  data_n_category <- sum( x == category, na.rm=TRUE )
  data_n_values <- sum( is.na( x ) == FALSE )
  data_pct_category <- data_n_category / data_n_values *100

  # Sampling loop
  for (i in 1:no_of_samples) {
    # Take random sample from x (with replacement)
    sample_data <- sample( x, sample_size, replace = TRUE )

    # Calculate sample-specific metrics
    n_category <- sum( sample_data == category, na.rm=TRUE )
    n_values <- sum( is.na( sample_data ) == FALSE )
    pct_category <- ( n_category / n_values ) * 100
    p <- pct_category / 100
    se <- (p * (1-p ) / n_values)^0.5 * 100
    CI_95pct_upper_bound <- pct_category + (1.96 * se)
    CI_95pct_lower_bound <- pct_category - (1.96 * se)

    # Save sample-specific metrics to df
    df[i, "sample_no"] <- i
    df[i, "sample_size"] <- n_values
    df[i, "sample_category_n"] <- n_category
    df[i, "sample_pct"] <- pct_category
    df[i, "se"] <- se
    df[i, "CI_95pct_lower_bound"] <- CI_95pct_lower_bound
    df[i, "CI_95pct_upper_bound"] <- CI_95pct_upper_bound
    df[i, "population_pct"] <- data_pct_category
  }

  # Calculate mean of sampling distribution
  mean_sample_mean <- mean( df$sample_pct )

  # Save summary stats describing the sampling distribution
  df2 <-
    data.frame( no_of_samples,
                population_pct = data_pct_category,
                sampling_mean = mean( df$sample_pct ),
                sampling_skew = gdslStats::skew( df$sample_pct ),
                sampling_std_dev =
                  gdslStats::sd( df$sample_pct, sample = TRUE )
    )

  # Identify samples for which 95% CI covers the value observed in x
  df$within_95pct_CI <- ifelse(
    ( df$population_pct >= df$CI_95pct_lower_bound)  &
      ( df$population_pct <= df$CI_95pct_upper_bound) ,
    TRUE, FALSE)
  sum( df$within_95pct_CI )
  sum( df$within_95pct_CI ) / no_of_samples * 100

  # Make initial histogram of sampling distribution
  distribution <- ggplot2::ggplot( data = df ) +
    ggplot2::geom_histogram( ggplot2::aes( x = sample_pct ),
                             binwidth = 0.5, alpha = 0.25 ) +
    ggplot2::xlab( paste0("% ",category ) ) +
    ggplot2::ylab( "Frequency" )

  # Calculate max height of histogram
  y_max <- max( ggplot2::ggplot_build( distribution )$data[[1]][["count"]] )

  # Set up data.frame saving start and end point of vertical line segments
  # used to flag observed value and mean of sampling distribution
  graph_df <-
    data.frame( x_obs = data_pct_category,
                x_sample = df2$sampling_mean,
                y_max )

  # Supplement histogram with vertical lines marking observed value and
  # mean of sampling distribution
  distribution <- distribution +
    ggplot2::geom_segment( ggplot2::aes( x = x_obs, xend = x_obs,
                                         y = 0, yend = y_max,
                                         colour = "Population %" ),
                           linewidth = 1,
                           data = graph_df ) +
    ggplot2::geom_segment( ggplot2::aes( x = x_sample, xend = x_sample,
                                         y = 0, yend = y_max,
                                         colour = "Sampling Mean" ),
                           linetype = "dashed",
                           linewidth = 1,
                           data = graph_df ) +
    ggplot2::scale_color_discrete( name="" ) +
    ggplot2::theme_classic( )

  # Calculate coverage rate + create associated graph label
  coverage_rate <- sum( df$within_95pct_CI ) / no_of_samples * 100
  coverage_rate_label <- paste0( "Coverage rate: ", coverage_rate, "%" )

  # Create coverage graph
  coverage_graph <- ggplot2::ggplot( data = df ) +
    ggplot2::geom_segment( ggplot2::aes( y = sample_no,
                                         yend = sample_no,
                                         x = CI_95pct_lower_bound,
                                         xend = CI_95pct_upper_bound,
                                         colour = within_95pct_CI ) ) +
    ggplot2::geom_vline( ggplot2::aes( xintercept = data_pct_category ),
                         linetype = "dashed" ) +
    ggplot2::geom_point( ggplot2::aes( x = sample_pct, y = sample_no ),
                         size = 1 ) +
    ggplot2::xlab( paste0("% ", category) ) +
    ggplot2::ylab( "Sample number" ) +
    ggplot2::scale_colour_manual(
      name = "Confidence Interval\nincludes true value",
      values = c( `TRUE` = "Cyan", `FALSE` = "Red" ),
      drop=FALSE ) +
    ggplot2::labs( title = coverage_rate_label ) +
    ggplot2::theme_classic() +
    ggplot2::theme( plot.title = ggplot2::element_text( face = "bold",
                                                        size = 11,
                                                        hjust = 1 ) )
  # Return function outputs
  return( list( samples=df,
                distribution_stats = df2,
                distribution = distribution,
                coverage_graph = coverage_graph,
                coverage_rate = coverage_rate ) )
}
