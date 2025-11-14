#' Round numerical data
#'
#' @param data_in The data to be rounded, supplied as a (vector of) numeric or logical; or as a data object of class table, matrix, data.frame or tibble (any of which can contain non-numeric columns). Note: tables or matrices with 4 or more dimensions not supported.
#' @param dp The number of decimal places to round to (default is 2), supplied as either as single number to be applied to all numeric values, or as a series vector of values (one per column of the input data object, in column order, using NA for factor/string columns)
#' @param text_table If TRUE (default), and data_in is of class table, and column-specific dp values are supplied, returns a table in which all numeric values have been converted to strings, so that the dp per column used for display matches that used for rounding
#'
#' @return An updated version of the supplied data object containing rounded numeric values
#' @export
#'
#' @examples
#' # Create toy example for rounding demo
#' survey2 <- survey |>
#'   dplyr::select( Tenure, Height, Income, Household_w0, Person_w0 ) |>
#'   dplyr::slice( 1:5 )
#'
#' # gdslStats::round as a replacement for base::round
#' round( 1.1234 )
#' round( 1.1234, dp = 1 )
#'
#' # Rounding of a data.frame including a non-numeric column
#' round( data_in = survey2 )
#'
#' # Rounding each numeric column differently
#' round( data_in = survey2, dp = c(NA, 1, 2, 3, 4) )
#'
#' # Rounding each column of a tibble differently
#' round( tibble::as_tibble( survey2), dp = c(NA, 4, 1, 3, 2) )
#'
#' # Rounding each column of a table differently
#' # Note the need to use text_table = TRUE to display result as expected
#' survey |> tab( Age ~ Sex, measure = "row_pct" ) |> round( dp = c(1,3) )
#' survey |> tab( Age ~ Sex, measure = "row_pct" ) |>
#'  round( dp = c(1,3), text_table = FALSE )
round <- function( data_in, dp = 2, text_table = TRUE ) {

  data_in_class <- class(data_in)

  data_length <- NULL

  dp_length <- length( dp )

  was_tibble <- FALSE

  # Stop if supplied input is a publish_tab data object
  if ( "publish_tab" %in% class( data_in ) )
    stop( "Apply rounding BEFORE calling publish_tab( )", call. = F )

  # For vector data of any length from 1 to n
  # [Vector data recognised by possession of only one class,
  #  limited to the classes numeric, character, logical and factor]

  # If vector, stop if data supplied as character or factor
  if ( is.null( dim( data_in ) ) ) {

    if ( "character" %in% data_in_class )
      stop( "Supplied value(s) are character, not numeric", call. = F )

    if( is.factor(data_in) )
      stop("Supplied data is a factor. Rounding cannot be applied to factors.",
           call. = F )
  }

  # Given type of input data, calculate appropriate length of data-in
  # (= vector length or number of cols in a matrix/table/data.frame/tibble)
  # and apply appropriate rounding approach

  # Vectors
  if ( is.null( dim( data_in ) ) &
       all( data_in_class %in% c("numeric", "logical") == TRUE) ) {

    data_length <- length( data_in )

    # Apply rounding for vectors
    data_out <- base::round( data_in, dp[1] )
    if ( dp_length > 1 )
      warning(
        "It is not possible to round each value in a vector to different dp,\n",
        "so first supplied value for dp has been applied to all.",
        call. = F )
  }

  # For other valid data types, find number of columns in dataset,
  # check that dp_length matches data_length, and take corrective
  # action if required

  if ( is.table(data_in) | is.matrix(data_in) |
       is.data.frame(data_in) | tibble::is_tibble(data_in) ) {

    # Temporarily convert any tibbles to data.frames to avoid problem with
    # change to column descriptor in resulting tibble. (Outcome changed
    # back to a tibble at end of function.)
    if (tibble::is_tibble( data_in ) ) {
      data_in <- as.data.frame( data_in )
      was_tibble <- TRUE
    }

    data_length <- ncol( data_in )

    if ( dp_length !=1 & data_length != dp_length ) {
      warning( "Number of dp values supplied does not match number",
             " of columns\nin data being rounded. Therefore results ",
             "might not be as intended.", call. = F )

      # if dp has too few values, extend as required with default value of 2
      # and update dp_length accordingly
      if ( dp_length < data_length ) {
        dp <- c( dp, rep(2, data_length - dp_length) )
        dp_length <- data_length
      }
    }
  }

  # tables and matrices
  if ( is.table(data_in) | is.matrix(data_in) ) {

    if ( is.numeric(data_in) ) {

      n_dims <- length( dim( data_in ) )

      if ( n_dims > 3 )
        stop( "gdslStats::round does not support rounding of tables or ",
              "matrices with 4 or more dimensions" )

      if ( dp_length == 1) {

        data_out <- base::round( data_in, dp )

      } else {

        data_out <- data_in

        if ( text_table == FALSE | is.table( data_in ) == FALSE ) {

          n_dims <- length( dim( data_in ) )

          # Return numeric values that are correctly rounded,
          # but display with same number of dp in each column
          for (i in 1:ncol( data_in )) {

            # If a 2-d table/matrix...
            if (  n_dims == 2 ) {
              data_out[ , i ] <- round( data_in[ , i ], dp[i] )
            # if a 3d table
            } else if ( n_dims == 3 ) {
              for ( j in 1:dim( data_in[ 3 ]) )
                data_out[ , i, j ] <- round( data_in[ , i, j ], dp[i] )
            }

          }

        } else {

          # Return string values that are correctly rounded,
          # and display requested number of dp in each column
          for (i in 1:ncol( data_in )) {

            # If a 2-d table/matrix...
            if (  n_dims == 2 ) {
              data_out[ , i ] <- format( base::round( data_in[ , i ], dp[i] ),
                                         nsmall = dp[i] )
            } else if ( n_dims == 3 ) {
              for ( j in 1:dim( data_in) [ 3 ] )
                data_out[ , i , j ] <-
                  format( base::round( data_in[ , i, j ], dp[i] ),
                                       nsmall = dp[i] )
            }
          }

        } # text_table = FALSE/TRUE

      } # dp_length = 1 or 1+


    } else {
      # Not numeric, so provide relevant warning mesage

      if ( is.matrix( data_in) )
        stop("Supplied matrix not numeric, so cannot be rounded.",
             call. = F )

      if ( is.table( data_in) )
        stop("Supplied table not numeric, so cannot be rounded.",
             call. = F )

    } # is.numeric

  } # is table or matrix

  # data.frames
  if ( is.data.frame( data_in ) ) {

    data_length <- ncol(data_in)

    # Apply rounding to data.frames
    if ( dp_length == 1 ) {
      #apply same rounding to all numeric columns in data.frame
      data_out <- data_in |>
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                               ~ base::round( .x, dp) ) )
    } else {
      data_out <- data_in
      for ( i in 1:ncol( data_in ) ) {
        if ( is.numeric( data_in[ , i ] ) )
          data_out[ , i ] <- base::round( data_in[,i], dp[i] )
      }
    }
  } # is.data.frame

  # tibbles
  if ( tibble::is_tibble( data_in ) ) {

    data_length <- ncol(data_in)

    if ( dp_length == 1 ) {
      #apply same rounding to all numeric columns in tibble
      data_out <- data_in |>
        # Change dp data are stored to
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                                      ~ base::round( .x, dp) ) ) |>
        # Change dp used to display data
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                                      ~ tibble::num( .x, digits = dp) ) )
    } else {
      data_out <- data_in
      for ( i in 1:ncol( data_in ) ) {
        if ( is.numeric( data_in[[i]]) ) {
          # Change dp data are stored to
          data_out[, i] <- base::round( data_in[ , i], dp[i] )
          # Change dp used to display data
          data_out[, i] <- tibble::num( data_out[[i]], digits = dp[i] )
        }
      }
    }
  } # is_tibble


  # If data_length undefined at this stage, input data is not one of
  # supported data types

  if ( is.null( data_length ) )
    stop( paste0("Supplied data is of type (class) ",
                 class( data_in )," \n",
                 "Supported data types are:\n",
                 "numeric and logial vectors\n",
                 "matrix, table, data.frame and tibble"),
          call. = F )

  # Convert back to a tibble if was a tibble originally
  if ( was_tibble == TRUE ) data_out <- tibble::as_tibble( data_out )

  return( data_out )

} # round
