#' Tool to explore how best-fit line works in OLS regression
#'
#' @param df an optional data.frame (default = NULL). If supplied, first column treated as X variable and second column as Y variable.
#' @param a  numeric. Height of intercept on y-axis when x = 0.
#' @param b  numeric. Slope of line.
#'
#' @return a ggplot2 graph showing fit of line with intercept a and slope b to data plotted in graph. Also reports formula for line plus Total Sum of Squares to console.
#' @export
#'
#' @examples
#' best_fit_line( a = 10, b = 0 )
#'
best_fit_line <- function(df=NULL, a, b) {

  #require(ggplot2)

  if (!is.null (df)) {
    var.name.x <- names(df)[1]
    var.name.y <- names(df)[2]
    names(df) <- c("x","y")
  } else {
    var.name.x <- "X"
    var.name.y <- "Y"
  }

  plot.points <- TRUE
  if ( is.null(df) )  {
    x <- seq(1:100)
    y <- seq(1:100)
    df <- data.frame(x=x, y=y)
    plot.points <- FALSE
  }

  df$best.fit.line <- a + (b*df$x)

  TSS <- sum((df$y - df$best.fit.line)^2)

  graph <- ggplot2::ggplot(data=df)

  if (plot.points)  {

    graph <- graph + ggplot2::geom_point( ggplot2::aes(x=x, y=y) ) +
      ggplot2::labs(x=var.name.x, y=var.name.y) +
      ggplot2::theme_bw()

  }

  graph <- graph +
    ggplot2::geom_line( ggplot2::aes(x=x, y= best.fit.line), colour="blue")

  if (!plot.points)  {

    graph <-
      graph +
      ggplot2::labs(x="X", y="Y") +
      ggplot2::ylim(0,100) +
      ggplot2::xlim(0,100) +
      ggplot2::theme_bw()

  }

  print( graph )

  cat (paste( var.name.y, "= ", a, " + ", b, "(", var.name.x, ")" ) )
  cat("\n","\n")
  cat( paste( "Total Sum of Squares = ", base::round(TSS,1) ) )

}
