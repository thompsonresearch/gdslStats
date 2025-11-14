#' Regression predictor adequacy
#'
#' @param model the output from a regression model fitted using lm( ) or glm( )
#' @param graph logical. Default = FALSE. See return for details.
#'
#' @details Calculates the improvement in AIC that the supplied model makes compared to the NULL model (intercept only). Then fits a separate regression model for each predictor variable in the supplied model (fitted to the same cases as the full model), and for each model calculates the associated improvement in AIC over the NULL model. Finally, calculates the 'adquacy' of each predictor = AIC improvement for predictor on its own / AIC improvement of full model x 100. Based on Harrell F E (2015) Regression modeling strategies, 2nd ed., New York: Springer, pp. 207-8.
#'
#' @return A data.frame if graph = FALSE; a ggplot2 graph if graph = TRUE.
#' @export
#'
#' @examples
#' # The adequacy of predictors in an OLS regression model
#' lm( Sex ~ Age + Tenure + Income, data = survey ) |>
#'  adequacy( )
#'
#' # The adequacy of predictors in a logistic regression model
#' glm( Sex ~ Age + Tenure + Income, data = survey,
#'      family = stats::binomial( link = "logit" ) ) |>
#'   adequacy( )
#'
adequacy <- function( model = x, graph = FALSE ) {

  # A function to calculate the the relative importance of the predictors in
  # a logistic regression model, as measured by caculating the size of the
  # improvement in AIC for each model compared to the null model, then
  # calculating the improvement of each sub-model relative to the full model.
  # N.B. Any cases with a missing value for any of the
  # fitted models is excluded from all of the models being compared, in order
  # to ensure that same set of cases are used for ALL models, and that hence
  # the AICs are directly comparable.

  # Check to see that input is a regression model
  if ( all( class( model ) %in% c( "lm", "glm") ) == FALSE )
    stop("Supplied input is not a regression model")

  # Capture model type
  if ( "glm" %in% class( model ) ) {
    model_type <- "glm"
  } else {
    model_type <- "lm"
  }


  # Extract name of the model's outcome variable

  outcome <- names( model$model )[1]

  # Extract the names of the model's predictor variables
  # (i.e. the main effects; not any interaction terms)

  main_effects <- names( model$model )[-1]

  # Create a data.frame containing each of the main effect variables
  model_predictors <- data.frame( matrix( ncol = length( main_effects ), nrow = 0) )
  colnames( model_predictors )  <- main_effects

  # Add a row for each main effect variable containing FALSE in all columns
  model_predictors[ 1:(length( main_effects )), ] <- FALSE

  # Assign each main effect variable a column in which it is 'TRUE'
  for ( i in 1:(length( main_effects )) ) {
    j <- i
    model_predictors[ i , j ] <- TRUE
  }

  # Each row in model_predictors represents a model containing
  # 1 or more predictors. For each row in turn, create a
  # formula representing this model, and save the result in list format
  model_formulas <-
    apply( model_predictors,
           1,
           function( x ) stats::as.formula(
             paste( outcome,
                    "~ ",
                    paste( main_effects[ x ], collapse = "+" ),
                    sep="") ) )


  # Extract dataset of 'complete cases' used by the full model

  if ( model_type == "lm" )
    df <- model$model

  if ( model_type == "glm" )
    df <- model$data |>
    dplyr::select( tidyr::all_of( c( outcome, main_effects ) ) ) |>
    tidyr::drop_na( )

  # Fit each model in turn and save the results to model_results,
  # ensuring that all models are fitted to exactly the same cases

  if ( model_type == "lm" ) {

    model_results <-
      lapply( model_formulas,
              function(x) stats::lm(x, data = df,
                                    na.action = stats::na.exclude ) )
  } else {

    model_results <-
      lapply( model_formulas,
              function(x) stats::glm(x,
                                 data = df,
                                 family = stats::binomial( link = "logit" ),
                                 na.action = stats::na.exclude ) )

  }

  # Extract the AIC for each fitted model
  model_predictors$AIC <-
    model_results |>
    lapply( function(x) stats::AIC(x) ) |>
    unlist()

  # Calculate the AIC for the NULL model
  # [the NULL model contains one parameter - the intercept, and AIC =
  #  deviance + (2 x no. of parameters)
  if ( model_type == "lm" )
    null_model_AIC <-
    stats::lm( paste( outcome, "~ 1"), data = df ) |>
    stats::AIC( )

  if ( model_type == "glm" )
    null_model_AIC <- model$null.deviance + 2


  # Calculate AIC improvement of the FULL model
  full_model_AIC_imp <- null_model_AIC - stats::AIC( model )

  # Caculate the AIC improvement compared to NULL model for each predictor model
  model_predictors$AIC_imp <- null_model_AIC - model_predictors$AIC

  # Calculate the adequacy of each model (i.e. AIC improvement as a % of
  # model improvement of the BEST model)
  model_predictors$adequacy <-
    model_predictors$AIC_imp / full_model_AIC_imp * 100

  # Drop the TRUE/FALSE columns and replace with a column naming the models
  model_predictors <-
    model_predictors |>
    dplyr::mutate( model = main_effects ) |>
    dplyr::relocate( model ) |>
    dplyr::select( -tidyr::all_of( main_effects ) )

  # Add the FULL model as a first row
  model_predictors <- tibble::add_row( model_predictors, .before = 1 )
  model_predictors[ 1, 1 ] <- "FULL"
  model_predictors[ 1, 2: ncol(model_predictors) ] <-
    c( stats::AIC( model ), full_model_AIC_imp, 100)

  # Add the NULL model as a new first row
  model_predictors <- tibble::add_row( model_predictors, .before = 1 )
  model_predictors[ 1,  1] <- c("NULL")
  model_predictors[ 1, 2:ncol( model_predictors ) ] <-
    c( null_model_AIC, 0, NA )


  adequacy_graph <-
    ggplot2::ggplot( data = dplyr::slice( model_predictors, -c(1:2) )  ) +
    ggplot2::geom_col( ggplot2::aes( x = forcats::fct_reorder( model,
                                                      dplyr::desc( adequacy )
                                                    ),
                            y = adequacy )
             ) +
    ggplot2::xlab( "Variable" ) +
    ggplot2::ylab ( "Adequacy (%)") +
    ggplot2::theme_classic( )

  if ( graph == TRUE ) {
    return( adequacy_graph )
  } else {
    return( model_predictors )
  }

}
