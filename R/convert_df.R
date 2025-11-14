#' Convert a data.frame from matrix to long format and retain results only for specified variable(s)
#'
#' @param df a data.frame containing the results from a correlation matrix or similar
#' @param statistic a character string naming the column to be used for storing values.
#' @param var_name a character string giving the name of a variable in df for which all pair-wise results are required. Default is NULL, which results in the return of all possible pairwise results.
#'
#' @return a 'long' data.frame containing the columns x, y and statistic.
#' @export
#'
#' @examples
#' # Create a matrix-style data.frame of correlation results
#' #df <- survey |>
#' #  dplyr::select_if( is.numeric ) |>
#' #  cor( "pearson" )
#' #df
#'
#' # Convert to long-format
#' #df |> convert_df( statistic = "r" )
#'
#' # Convert to long-format, retaining only results pertaining to var_name
#' #df |> convert_df( statistic = "r", var_name = "Height" )
#'
convert_df <- function( df, statistic, var_name = NULL ) {

  # Convert from wide to long format, with col names of x, y and statistic name
  res <- df |>
    tibble::rownames_to_column( "x") |>
    tidyr::pivot_longer( !x, names_to = "y", values_to = statistic ) |>
    as.data.frame( )

  # If a var_name is specified, filter out all except variable pairs
  # containing this variable, then swap values in columns x and y so that
  # the chosen variable name always appears in the first column.
  # Then remove duplicates.

  if ( !is.null( var_name ) ) {
    res <-
      res |>
      dplyr::filter( x == var_name | y == var_name ) |>
      dplyr::mutate( y = dplyr::if_else( y == var_name, x, y),
              x = var_name ) |>
      dplyr::distinct( x, y, .keep_all = TRUE )

  }

  return( as.data.frame( res ) )

}
