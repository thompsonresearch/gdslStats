#' Sort data.frame containing results from the cor function
#'
#' @param df a data.frame containing results from the cor function.
#' @param sort_by a character string indicating on which column the output should be sorted by. Options are "x", "y", "r", "p", "s", "ci.lb" and "ci.ub". Default is "r".
#' @param sort_abs logical (TRUE or FALSE). Default is FALSE. If TRUE, output sorted on a column containing numeric values will be sorted on the absolute value of thise numeric values (i.e. ignoring the sign (+/-) of the values).
#'
#' @return a sorted data.frame
#' @export
#'
#' @examples
#' # Create data.frame capturing outputs from gdslStats::cor function
#' #df <- survey |>
#' #  dplyr::select_if( is.numeric ) |>
#' #  cor( "pearson", p_value = TRUE, sig = TRUE, ci = 95 )
#'
#'  # Use sort_df to sort by p_value
#' #  df |> sort_df( sort_by = "p" )
#'
sort_df <- function( df, sort_by = "r", sort_abs = FALSE ) {

  # If sorting is requested, sort by value of chosen column, sorting
  # by absolute value when requested to so, if possible
  # For numeric columns, sort in descending order.
  # For all other types of column, sort in alphabetical order

  if ( is.null( sort_by ) ){
    warning( "sort_df called, but no value supplied for sort_by. ",
             "data.frame left unsorted.", call. = F )
    return( df )
  }

  if ( sort_by %in% names(df) == FALSE) {
    stop( "Variable name supplied via sort_by not present in data.frame. ",
          "Available variables are:\n ", names( df ) )
  }

  if ( is.numeric( df[ , sort_by ] ) ) {
    if ( sort_abs == TRUE )
      df <- df |> dplyr::arrange( dplyr::desc( abs( dplyr::across( tidyselect::all_of( sort_by ) ) ) ) )
    if ( sort_abs == FALSE )
      df <- df |> dplyr::arrange( dplyr::desc( dplyr::across( tidyselect::all_of( sort_by ) ) ) )
  }

  if ( !is.numeric( df[ , sort_by ] ) )
    df <- df |> dplyr::arrange( dplyr::across( tidyselect::all_of( sort_by ) ) )

  return( as.data.frame( df ) )

}
