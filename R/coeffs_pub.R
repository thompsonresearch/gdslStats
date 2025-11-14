#' Publication-standard table of regression coefficients
#'
#' @param df a data.frame containing regression model results produced by the gdslStats::coeff function.
#' @param line_width numeric. Thickness of horizontal lines in table.
#'
#' @return a flextable object.
#' @export
#'
#' @examples
#' model <- lm( Income ~ Height, data = survey )
#' coeffs( model ) |> coeffs_pub( )
#'
coeffs_pub <- function( df, line_width = 1.2 ) {

  n_rows <- nrow(df)
  n_fit <- attributes(df)$n_fit

  df <-
    df |>
    flextable::flextable( ) |>
    flextable::hline_top( part = "header",
                          border = officer::fp_border( width = line_width ) ) |>
    flextable::hline( part = "header",
                      i = c(1:1),
                      border = officer::fp_border( width = line_width ) ) |>
    flextable::hline( part = "body", i = n_rows,
                      border = officer::fp_border( width = line_width ) )

  if ( n_fit > 0 ) {
    df <-
      df |>
      flextable::hline( i = n_rows - n_fit ,
                        border = officer::fp_border( width = line_width ),
                        part = "body" )
  }

  return(df)

}
