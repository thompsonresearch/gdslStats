#' Correlation (Pearson, Spearman or Cramer's V)
#'
#' @param y a data.frame containing two or more columns
#' @param method a character string indicating the type of correlation to be calculated. One of "pearson", "spearman" or "cramer".
#' @param use a character string giving a method for computing correlations in the presence of missing values. This must be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is "everything". See Details.
#' @param useNA a character string, one of "no" and "ifany" indicating whether NA should be treated as a category when calculating Cramer's V. Default is "no".
#' @param p_value logical (TRUE or FALSE), indicating whether results should include p-values. Default is FALSE.
#' @param sig logical (TRUE or FALSE), indicating whether results should include asterisks flagging 'significant' values (i.e. those with p-values less than 0.01, 0.05 and 0.1). Defaut is FALSE.
#' @param ci NULL (default) or a numeric value in the range 0 to 100 indicating what % confidence intervals are required. (e.g 95 for 95% CIs)
#' @param var_name a character string giving the name of a variable in y for which all pair-wise correlations are required. Default is NULL, which results in the return of all possible pairwise correlations.
#' @param sort_by a character string indicating on which column the output should be sorted by. Options are "x", "y", "r", "p", "s", "ci.lb" and "ci.ub". Default is "r".
#' @param sort_abs logical (TRUE or FALSE). Default is FALSE. If TRUE, output sorted on a column containing numeric values will be sorted on the absolute value of thise numeric values (i.e. ignoring the sign (+/-) of the values).
#' @param matrix logical (TRUE or FALSE). Default is FALSE. If TRUE, instead of output being returned in one data.frame, the results are returned as a list of data.frames (one per type of output requested), in a format suitable for direct input to cor_plot( ).
#' @param fill logical (TRUE or FALSE). Default is TRUE. If FALSE, only the top half of each matrix is completed. Applies only if matrix = TRUE.
#'
#' @return If matrix = FALSE, one data.frame containing all results in 'long' format. If y contains only two columns, then only one row of results returned (col1 x col2), instead of all possible correlations (col1 x col1, col1 x col2, col2 x col1, col2 x col2). If matrix = TRUE, a list of data.frames is returned, one per requested measure, and the data.frame will be in 'matrix' format.
#' @export
#'
#' @examples
#' # Correlation of two variables
#' survey |> dplyr::select( Income, Height ) |> cor( "pearson" )
#'
#' # Correlation between all variables in a supplied data.frame,
#' # plus return of p-value, sig and CIs in addition to r.
#' #survey |>
#' #   dplyr::select_if( is.numeric) |>
#' #   cor( "spearman", p_value = TRUE, sig = TRUE, ci = 95 )
#'
#' # As above, but with results filtered to report only the correlations
#' # with one named variable, and sorted in order of the contents of column y.
#' #survey |>
#' #   dplyr::select_if( is.numeric) |>
#' #   cor( "spearman", p_value = TRUE, sig = TRUE, ci = 95,
#' #        var_name = "Height", sort_by = "y" )
#'
#' # Results reported in matrix format (suitable as an input to cor_plot)
#' # survey |>
#' #   dplyr::select_if( is.numeric ) |>
#' #   cor( "spearman", p_value = TRUE, sig = TRUE, ci = 95,
#' #        matrix = TRUE )
#'
cor <- function( y,  method = NULL, use = "everything", useNA = "no",
                 p_value = FALSE, sig = FALSE, ci = NULL,
                 var_name = NULL, sort_by = "r", sort_abs = FALSE,
                 matrix = FALSE, fill = TRUE ) {

  ### PART ONE: Check validity of inputs

  # Check that y is a data.frame or tibble with at least two columns of data
  if ( is.null( y ) )
    stop( "y must be a data.frame or tibble", call. = FALSE )

  if ( is.data.frame( y )  == FALSE & tibble::is_tibble( y ) == FALSE )
    stop( "y must be a data.frame or tibble" )

  if ( ncol( y ) < 2 )
    stop( "To calculate a correlation, at least two columns of data are required",
          call. = F )

  # Check that method is valid; convert to lower case it if is
  if (is.null(method)) {
    stop("method must be one of pearson, spearman or cramer", call. = FALSE )
  } else if ( !( tolower( method ) %in%
                 c( "spearman", "pearson", "cramer" ) ) ) {
    stop("method must be one of pearson, spearman or cramer",
         call. = FALSE )
  }
  method <- tolower( method )


  # Check that value of 'use' is valid
  if ( is.null( use ) )
    stop( "use must take one of the following values:\n",
          "all.obs, complete.obs, na.or.complete, pairwise.complete.obs",
          call. = FALSE )

  use_values <- c( "everything", "all.obs", "complete.obs",
                   "na.or.complete", "pairwise.complete.obs" )

  use <- tolower(use)

  if ( use  %in% use_values == FALSE )
    stop( cat( "use must take one of the following values:\n",
               use_values ),
          call. = FALSE )

  # Check that useNA is valid, having first converted it to lower case
  useNA_values <- c( "no", "ifany" )
  if ( is.null( useNA ) )
    stop( cat( "useNA must take one of the following values:\n",
               useNA_values ),
          call. = FALSE )

  useNA <- tolower( useNA )
  if ( useNA %in% useNA_values == FALSE ) {
    stop( "useNA must take one of the following values:\n",
          useNA_values,
          call. = FALSE )
  }

  # Check that p_value is valid; convert to logical if necessary
  if ( is.null( p_value ) )
    stop( "p_value must be either TRUE or FALSE", call. = FALSE )
  p_value <- toupper(p_value)
  if ( p_value %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE )
    stop( "p_value must be either TRUE or FALSE", call. = FALSE )
  if ( p_value == "TRUE" ) p_value <- TRUE
  if ( p_value == "FALSE" ) p_value <- FALSE

  # Check that sig is valid; convert to logical if necessary
  if ( is.null( sig ) )
    stop( "sig must be either TRUE or FALSE", call. = FALSE )
  sig <- toupper(sig)
  if ( sig %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE )
    stop( "sig must be either TRUE or FALSE", call. = FALSE )
  if ( sig == "TRUE" ) sig <- TRUE
  if ( sig == "FALSE" ) sig <- FALSE


  # Check that ci is valid
  if ( !is.null( ci ) ) {
    if ( is.numeric( ci) == FALSE ) {
      stop( "ci must be a numeric value between 0 and 100", call. = FALSE )
    } else if ( ci < 0 | ci > 100 ) {
      stop( "ci must be a numeric value between 0 and 100", call. = FALSE )
    }
  }

  # Check that var_name is valid
  if ( is.null( var_name ) == FALSE )
    if ( var_name %in% names( y)  == FALSE )
      stop( cat( "var_name does not match any of the variables names in ",
                 "supplied dataset. Available variables are:\n",
                 names( y ) ) )

  # Check that sort_by is valid
  sort_by_values <- c( "x", "y", "r", "p", "sig", "ci.lb", "ci.ub" )
  if ( is.null( sort_by ) == TRUE )
    stop( cat( "sort_by must take one of the following values:\n",
               sort_by_values ),
          call. = FALSE )

  sort_by <- tolower( sort_by )
  if ( sort_by %in% sort_by_values == FALSE ) {
    stop( cat("sort_by must take one of the following values:\n",
              sort_by_values),
          call. = FALSE )
  }

  # Check that sort_abs is valid; convert to logical if necessary
  if ( is.null( sort_abs ) )
    stop( "sort_abs must be either TRUE or FALSE", call. = FALSE )
  sort_abs <- toupper(sort_abs)
  if ( sort_abs %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE )
    stop( "sort_abs must be either TRUE or FALSE", call. = FALSE )
  if ( sort_abs == "TRUE" ) sort_abs <- TRUE
  if ( sort_abs == "FALSE" ) sort_abs <- FALSE

  if ( sort_abs == TRUE & sort_by %in% c( "x", "y", "s") )
    warning( paste0( "sort_by = ",sort_by, ". Therefore sort_abs = TRUE ",
                     "not applicable." ), call. = F )

  # Check that matrix is valid; convert to logical if necessary
  if ( is.null( matrix ) )
    stop( "matrix must be either TRUE or FALSE", call. = FALSE )
  matrix <- toupper(matrix)
  if ( matrix %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE )
    stop( "matrix must be either TRUE or FALSE", call. = FALSE )
  if ( matrix == "TRUE" ) matrix <- TRUE
  if ( matrix == "FALSE" ) matrix <- FALSE

  if ( is.null( var_name ) == FALSE & matrix == TRUE ) {
    matrix <- FALSE
    warning( "var_name supplied, so matrix set to FALSE", call. = F )
  }

  # Check that fill is valid; convert to logical if necessary
  if ( is.null( fill ) )
    stop( "fill must be either TRUE or FALSE", call. = FALSE )
  fill <- toupper(fill)
  if ( fill %in% c(TRUE, FALSE, "TRUE", "FALSE") == FALSE )
    stop( "fill must be either TRUE or FALSE", call. = FALSE )
  if ( fill == "TRUE" ) fill <- TRUE
  if ( fill == "FALSE" ) fill <- FALSE


  ### PART TWO: Apply the 'use' setting to filter input data accordingly ###

  # if use == all.obs, stop and issue warning if ANY value is dataset is NA
  if ( use == "all.obs" )
    if ( any( apply( y,
                     MARGIN = 2,
                     FUN = function(x) any( is.na(x) ) )
    ) == TRUE
    ) stop( "Missing observations in supplied dataset", call. = F )

  # if use == complete.obs or na.or.complete, delete all cases (rows)
  # containing NA values
  if ( use == "complete.obs" | use == "na.or.complete" )
    y <- stats::na.omit( y )

  # if use == complete.obs and no rows left, stop and report error
  if ( use == "complete.obs") {
    if ( nrow( y ) == 0 )
      stop( "All cases (rows) contain NA values", call. = F )
    if ( nrow( y ) == 1 )
      stop( "All but one case (row) contains NA values", call. = F )
  }

  # if use == na.or.complete and no rows left, return NAs for all correlations
  # [This is executed later on in the code]

  # if use = pairwise.complete.obs, this is the default action,
  # so no additional coding required


  ### PART THREE Find correlation for each variable pair in supplied data.frame

  # This is done separately each variable pair, via cor_test or
  # cramersV_test, except for pearson/spearman when only the value of
  # r is required.

  col.y <- ncol( y )
  r <- matrix( ncol = col.y, nrow = col.y )
  p <- r
  s <- r
  ci.lb <- r
  ci.ub <- r

  for(i in 1:(col.y - 1)){
    for(j in (i + 1):col.y){

      total_NA <- sum( is.na( y[ ,i] ) )  + sum( is.na( y[, j] ) )

      if( use == "everything" & total_NA > 0 ) {

        r[i,j] <- NA
        p[i,j] <- NA
        s[i,j] <- NA
        ci.lb[i,j] <- NA
        ci.ub[i,j] <- NA

        warning( "One or both of ", names(y)[1], " and ", names(y)[2],
                 " contain NA ",
                 "values, so NA returned for value of Cramer's V. Consider ",
                 "changing the default setting of cor( ) from ",
                 "'use = everything'.",
                 call. = F )

      }  else if ( use == "na.or.complete" & nrow( y ) < 2 ) {

        r[i,j] <- NA
        p[i,j] <- NA
        s[i,j] <- NA
        ci.lb[i,j] <- NA
        ci.ub[i,j] <- NA

        warning( "One or zero rows contain complete observations.", call. = F )

      } else {

        if ( method == "cramer" ) {
          result <- cramersV_test( y[,i], y[,j],
                                   p_value = p_value,
                                   sig = sig,
                                   ci = ci,
                                   useNA = useNA,
                                   NaN_warning = FALSE )

          if ( is.nan( result$cramer) == TRUE ) {
            warning( "The cross-tabulation of ",
                     names(y)[i], " by ",names(y)[j], " produces a table ",
                     "containing one or more row or column totals of zero. ",
                     "Therefore a Cramer's V of NaN is returned for this ",
                     "variable pair.",
                     call. = F)
          }

        }

        if ( method %in% c( "pearson", "spearman" ) ) {
          result <- cor_test( y[,i], y[,j],
                              method = method,
                              p_value = p_value,
                              sig = sig,
                              ci = ci )
        }

        r[i,j] <- result[ , 1] # correlation always stored in first column
        # but column name is method specific

        if (p_value == TRUE) p[i,j] <- result$p.value

        if (sig == TRUE) s[i,j] <- result$sig
        if ( !is.null(ci) ) {
          ci.lb[i,j] <- result[ , ncol(result) - 1 ]
          ci.ub[i,j] <- result[ , ncol(result) ]
        }

      }

    } # next j
  } # next i

  ### PART FOUR:Assign the diagonal values

  # NA if use = everything and NAs present; else for r and cis: 1,
  # for p-value: 0; and for sig: **

  for(i in 1:col.y){
    total_NA <- sum( is.na( y[ ,i] ) )
    if( use == "everything" & total_NA > 0) {
      r[i,i] <- NA
      if (p_value == TRUE) p[i,i] <- NA
      if (sig == TRUE) s[i,i] <- NA
      if ( !is.null(ci) ) {
        ci.lb[i,i] <- NA
        ci.ub[i,i] <- NA
      }
    } else if ( use == "na.or.complete" & nrow( y ) < 2 ) {
      r[i,i] <- NA
      if ( p_value == TRUE) p[i,i] <- NA
      if ( sig == TRUE) s[i,i] <- NA
      if ( !is.null(ci) ) {
        ci.lb[i,i] <- NA
        ci.ub[i,i] <- NA
      }
    }  else {
      r[i,i] <- 1
      if (p_value == TRUE) p[i,i] <- 0
      if ( sig == TRUE) s[i,i] <- "**"
      if ( !is.null(ci) ) {
        ci.lb[i,i] <- 1
        ci.ub[i,i] <- 1
      }
    }
  } # next i

  ### PART FIVE: Complete upper-half of correlation matrix, if required
  if (fill) {
    for (i in 1:ncol(r)) {
      r[, i] <- r[i, ]
      if (p_value == TRUE) p[, i] <- p[i, ]
      if (sig == TRUE) s[, i] <- s[i, ]
      if ( !is.null(ci) ) {
        ci.lb[,i] <- ci.lb[i,]
        ci.ub[,i] <- ci.ub[i,]
      }
    }
  }

  ### PART SIX Convert results into data.frames and add row and column names
  r <- data.frame(r)
  row.names(r) <- names(y)
  names(r) <- names(y)

  p <- data.frame(p)
  row.names(p) <- names(y)
  names(p) <- names(y)

  s <- data.frame(s)
  row.names(s) <- names(y)
  names(s) <- names(y)

  ci.lb <- data.frame(ci.lb)
  row.names(ci.lb) <- names(y)
  names(ci.lb) <- names(y)

  ci.ub <- data.frame(ci.ub)
  row.names(ci.ub) <- names(y)
  names(ci.ub) <- names(y)

  ### PART SEVEN if matrix output required, return required results as a
  ###           matrix or list of matrices

  if ( matrix == TRUE ) {

    if ( p_value == FALSE & sig == FALSE & is.null(ci) ) {

      return( r )

    } else {


      result <- c( list( r = r),
                   if ( p_value == TRUE ) list( p = p ),
                   if ( sig == TRUE ) list( sig = s ),
                   if ( !is.null(ci) ) list( ci.lb = ci.lb, ci.ub = ci.ub ) )

      attributes(result)$method <- method
      class( result ) <- append( class( result ), "corrplot_input" )

      return( result )

    }
  }

  ### PART EIGHT if all results required in one data.frame, convert
  ### each wide data.frame of results into a long data.frame, then apply any
  ### required filters; then sort as required; then return result

  if ( matrix == FALSE ) {

    r <- convert_df( r, "r", var_name = var_name )

    if ( p_value == TRUE ) {
      p <- convert_df( p, "p", var_name = var_name )
      r <- dplyr::left_join( r, p, by = c("x", "y") )
    }

    if ( sig == TRUE ) {
      s <- convert_df( s, "s", var_name = var_name )
      r <- dplyr::left_join( r, s, by = c("x", "y") )
    }

    if ( is.null(ci) == FALSE ) {
      ci.lb <- convert_df( ci.lb, "ci.lb", var_name = var_name )
      ci.ub <- convert_df( ci.ub, "ci.ub", var_name = var_name )
      r <- dplyr::left_join( r, ci.lb, by = c("x", "y") )
      r <- dplyr::left_join( r, ci.ub, by = c("x", "y") )
    }

    # Apply any sorting required
    r <- sort_df( r, sort_by = sort_by, sort_abs = sort_abs )

    # Make df column names more user-friendly (but therefore less generic)


    r <- r |> dplyr::rename( !!method := r )

    if ( sig == TRUE) r <- r |> dplyr::rename( "sig" = s )

    if ( is.null(ci) == FALSE ) {
      lb <- paste0( ci, "pct.ci.lb" )
      ub <- paste0( ci, "pct.ci.ub" )
      r <- r |> dplyr::rename( !!lb := ci.lb,
                               !!ub := ci.ub )
    }

    # If only 2 columns in input dataset, return only one row
    # containing the correlation of x with y, instead of all four rows
    # from the x~x, x~y y~x, y~y correlation matrix.
    if ( ncol(y)  == 2 ) r <- r[ !(r$x == r$y),  ] |> dplyr::slice(1)

    return( r )

  } # matrix = FALSE

}
