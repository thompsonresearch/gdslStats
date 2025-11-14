#' Convert regression model coefficients and fits into a data.frame
#'
#' @param ... one or more lm or glm regression models
#' @param measure character vector. One or more of the measures made available from broom::tidy( model ). Default value of "all" ensures all available measure are returned.
#' @param fit character vector. One or more of the measures of model fit made available from broom::glance( model). Default value NULL returns none. Value of "all" returns all available measures of fit.
#'
#' @return a data.frame containing the requested measures and fits for each supplied regression model.
#' @export
#'
#' @examples
#' model1 <- lm( Height ~ Income, data = survey )
#' model2 <- lm( Height ~ Income + Sex, data = survey )
#' model3 <- glm( Sex ~ Age,
#'                data = survey,
#'                family = binomial( link = "logit" ) )
#'
#' # Capture all measures for both models, but no fits
#' coeffs( model1, model2 )
#'
#' # Capture selected measures and fits
#' coeffs( model1, model2,
#'         measure = c("estimate", "p.value"),
#'         fit = c("r.squared", "adj.r.squared", "AIC") )
#'
#' # Capture regression coefficients only
#' coeffs( model1, model2, measure = "estimate" )
#'
#' # Capture all measures and fits
#' coeffs( model1, model2, measure = "all", fit = "all" )
#'
#' # Additional measure (expB) and fit (tjur.rsq) available for
#' # logistic regression models
#' coeffs( model3, measure = "all", fit = "all" )
#'
#' # Extract odds ratio and AIC only for logistic regression model
#' coeffs( model3, measure = "expB", fit = "AIC" )
#'
coeffs <- function( ..., measure = "all", fit = NULL ) {

  x <- list(...)

  # Declare function to check that all inputs are lm or glm models
  model_check <- function( x ) {
    return( all( class(x) %in% c("lm", "glm") ) )
  }

  # Check to see that all inputs are models
  check_res <- lapply( x, FUN = model_check )

  if ( any( check_res == FALSE ) )
    stop("Not all supplied inputs are regression models")

  # Identify valid measures for given model type
  valid_measures <- broom::tidy( x[[1]] )  |> dplyr::select(-term) |> names( )
  if ( "glm" %in% class( x[[1]] ) )
    valid_measures[ length( valid_measures ) +1 ] <- "expB"

  # If measure == "all" measures = all valid measures
  # else measures = list of measures supplied by user
  if ( length(measure) == 1 ) {
    if ( measure == "all" ) {
      measures <- valid_measures
    } else {
      measures <- measure
    }
  }

  if ( length( measure ) > 1 ) measures <- measure

  # Capture number of measures required in output
  n_measures <- length( measures )

  # Check measures supplied are all valid
  if( any( measures %in% ( valid_measures ) == FALSE ) )
    stop( cat( "One or more of supplied values of measures is invalid. ",
               "Possible values are:\n",
               valid_measures ), call. = F )

  # Check that supplied fit values are all valid
  valid_fit <- names( broom::glance( x[[1]] ) )
  if ( "glm" %in% class( x[[1]] ) )
    valid_fit[ length( valid_fit ) +1 ] <- "tjurs.rsq"

  if ( length(fit) == 1 )
    if ( fit == "all" )
      fit <- valid_fit

  if ( is.null(fit) == FALSE )
    if( any( fit %in% valid_fit == FALSE ) )
      stop( "One or more of supplied values of fit is invalid. ",
            "Valid options are:\n",
            cat( valid_fit ), call. = F )

  # Apply tidy( ) to the list of models and save results as a list
  # [term, estimate, std.error, statistic, p.value, [expB] ]

  # Extract all measures
  tidy_x <- lapply( x, FUN = broom::tidy )

  # Add expB if logistic regression model
  if ( "glm" %in% class( x[[1]] ) )
    tidy_x <- purrr::imap( tidy_x,
                           ~ dplyr::mutate( .x, expB = exp(estimate) )
    )

  # Keep only requested measures unless "all" measures requested
  if ( "all" %in% measure == FALSE)
    tidy_x <- lapply( tidy_x,
                      FUN = function(x)
                        dplyr::select(x, term, tidyselect::all_of(measures) )
    )

  # Combine list of tidy( ) results into a single data.frame
  tidy_df <-
    purrr::reduce( tidy_x, dplyr::full_join, by = 'term' ) |>
    as.data.frame( )

  # Set count of no. of measures of fit to 0 if fit is NULL
  if ( is.null(fit)  == TRUE) {

    n_fit <- 0

    # else pull together measures of fit (if fit is not NULL)
  } else {

    # Calculate all available measures of fit using glance( ) and tjurs_rsq( ),
    # saving both results as a list
    glance_x <- lapply( x, broom::glance )
    Tjurs_Rsq_x <- lapply( x, tjurs_rsq )

    # Merge the two sets of results into one list
    glance_x <- purrr::map2( glance_x,
                             Tjurs_Rsq_x,
                             ~.x |> dplyr::mutate(tjurs.rsq = .y) )

    # convert each tibble into a data.frame, and place the names of the
    # measures of fit in a 'term' column to allow addition to the 'term'
    # column in tidy_df and renaming columns called V1 to 'estimate'.
    glance_x <- lapply( glance_x, FUN = function(x)
      dplyr::select(x, tidyselect::all_of( fit ) ) |>
        t( ) |>
        as.data.frame( ) |>
        tibble::rownames_to_column( var = "term" ) |>
        dplyr::rename( estimate = "V1" ) )

    # Combine list of model fits into a single data.frame
    glance_df <- purrr::reduce( glance_x, dplyr::full_join, by = 'term' )

    # Find number of rows in tidy_df and glance_df
    n_terms <- nrow( tidy_df )
    n_fit <- nrow( glance_df )

    # Add values from glance_df to bottom of relevant column in tidy_df
    # [names of measure fit to term column; measures of fit to the model-
    # specific 'estimate' column]

    # Capture names of cols in final version of glance_df
    # [i.e. term + one col per valid measure ]
    glance_df_names <- glance_df |> names(  )

    # Capture names of columns in tidy_df to which measures of fit
    # should be added
    tidy_df_names <- tidy_df |> names( )
    tidy_df_fit_cols <- tidy_df_names[ c( 1,
                                          seq(2,
                                              length(tidy_df_names),
                                              n_measures )
    ) ]

    # Take values from glance_df and add, transposed tidy_df
    # fit names are added to the term column; values of fits are added
    # to the first measure column for each model
    for (i in 1:length( glance_df_names ) ) {
      tidy_df[ (n_terms+1):(n_terms + n_fit), tidy_df_fit_cols[i] ] <-
        glance_df[ 1:n_fit , glance_df_names[i] ]
    }

  } #if fit is not NULL

  # Find number of models being processed
  n_models <- length( x )

  # If more than 1 model being processed, create new names for each column
  # in tidy_df that include the model number
  # (i.e.  model 1 estimate; model 1 std. error; model 2 estimate etc.)
  if ( n_models > 1 ) {
    new_names <- "term"
    for (i in 1:n_models ) {
      new_names <-
        append( new_names,
                paste0( "model ",i," ",
                        measures ) )
    }

    # Assign new column names to tidy_df
    names( tidy_df ) <- new_names

  }

  # Add an attribute recording number of measures of fit
  # (for use by coeffs_pub)

  if ( !is.null(fit) ) {
    attributes(tidy_df)$n_fit <- n_fit
  } else {
    attributes(tidy_df)$n_fit <- 0
  }

  return( tidy_df )

}
