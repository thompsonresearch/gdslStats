#' Plot a correlogram
#'
#' @param x a list supplying a set of correlations and p-values, each in a 'matrix' style data.frame, as produced by the gdsdlStats::cor function when matrix = TRUE and p_value = TRUE.
#' @param style a character string specifying correlogram style: one of "circle" (the default) and "heatmap".
#' @param sig logical (TRUE/FALSE) indicating whether statistically significant correlations should be flagged using asterisks (for the p<0.01, p<0.05 and p<0.1 levels). Default is FALSE.
#' @param sig_col a character string specifying the colour of any asterisks. Default is "black". For other options run grDevices::colors().
#' @param sig_size a numeric value (default 2) indicating the size of any asterisks.
#' @param label_size a numeric value (default 1) indicating the size of the text used to label variables associated with each row and column.
#'
#' @details
#' A wrapper for the `corrplot( )` function from the `corrplot` package.
#'
#' @return a correlogram in corrplot format
#' @export
#'
#' @examples
#' # Default cor_plot
#' survey |>
#'   dplyr::select_if( is.numeric ) |>
#'   cor( "pearson", p_value = TRUE, matrix = TRUE) |>
#'   cor_plot( )
#'
#' # cor_plot in  heatmap format with significant correlations flagged
#' # and changes to text and asterisk size
#' survey |>
#'   dplyr::select_if( is.numeric ) |>
#'   cor( "pearson", p_value = TRUE, matrix = TRUE) |>
#'   cor_plot( style = "heatmap", sig = TRUE )
#'
cor_plot <- function( x, style = "circle",
                      sig = FALSE, sig_col = "black", sig_size = 2,
                      label_size = 1
) {

  if ( "corrplot_input" %in% class(x) == FALSE )
    stop("Input must be matrix output from cor.R")

  method <- attributes(x)$method

  if ( is.null(x$r) )
    stop( "Input does not contain results for r" )

  if ( is.null(x$p) )
    stop( "Input does not contain results for p" )

  # Check that sig is valid; convert to logical if necessary
  if ( is.null(sig) == TRUE ) {
    warning( "sig reset from NULL to TRUE" )
    sig <- TRUE
  }
  sig <- toupper(sig)
  if ( sig %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE ) {
    sig <- TRUE
    warning( "Invalid value supplid for sig. Reset to TRUE.", call. = FALSE )
  }
  if ( sig == "TRUE" ) sig <- TRUE
  if ( sig == "FALSE" ) sig <- FALSE

  if ( sig == TRUE ) {
    sig <- "label_sig"
  } else {
    sig <- "n"
  }

  if ( is.null( style ) == TRUE ) {
    style <- "circle"
    warning( "style options are circle or heatmap. Reset to circle", call. = F )
  }

  style <- tolower( style )
  if ( style %in% c("circle", "heatmap")  == FALSE ) {
    style <- "circle"
    warning( "style options are circle or heatmap. Reset to circle", call. = F )
  }

  if ( style == "heatmap" ) style <- "color"

  if ( is.null( sig_col ) ) {
    sig_col <- "black"
    warning( "sig_col reset from NULL to black", call. = F )
  }
  sig_col <- tolower( sig_col )
  if ( sig_col %in% grDevices::colors() == FALSE ) {
    sig_col <- "black"
    warning( "sig_col not a valid colour. Reset to black.", call. = F )
  }

  if ( is.null( sig_size ) ) {
    sig_size <- 2
    warning( "sig_size reset from NULL to 2", call. = F )
  }
  if ( is.numeric( sig_size) == FALSE ) {
    sig_size <- 2
    warning( "label_size must be a numeric value. Reset to 2.", call. = F )
  }

  if ( is.null( label_size ) ) {
    label_size <- 1
    warning( "label_size reset from NULL to 1", call. = F )
  }
  if ( is.numeric( label_size) == FALSE ) {
    label_size <= 1
    warning( "label_size must be a numeric value. Reset to 1.", call. = F )
  }

  r <- as.matrix( x$r )

  p <- as.matrix( x$p )

  # If Cramers V, change col.lim to 0,1 from default of -1,1
  if ( attributes(x)$method == "cramer" ) {
    col.lim <- c(0,1)
  } else {
    col.lim <- c(-1,1)
  }

  p1 <- corrplot::corrplot(
    corr = r,
    p.mat = p,
    method = style,
    tl.srt = 45, # Slant column headers 45 degrees
    tl.col = "black", # Change font colour to black
    tl.cex = label_size,
    type = "lower", # Show lower triangle only,
    tl.pos = "ld",
    diag = FALSE, # excluding diagonal
    col.lim = col.lim,
    insig = sig,
    sig.level = c( 0.01, 0.05, 0.1),
    pch.col = sig_col,
    pch.cex = sig_size

  )

  return( p1 )

}
