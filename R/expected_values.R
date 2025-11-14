#' Title Report the (minimum) expected values under-pinning a glm( ) regression model
#'
#' @param model The output from a model fitted using the glm( ) function
#'
#' @return Writes results to screen; invisibly returns in list format the following three values: min_expected (the minimum expected value); pct_5plus (the % of cells containing a count of 5 or more); expected_values (a data.frame reporting the observed and expected cell count for each cell in a cross-tabulation of the model outcome variable with all of the model predictor variables )
#' @export
#'
#' @examples
#' # Fit glm model
#' model <-
#'   stats::glm( Sex ~ Tenure, data = survey,
#'               family = stats::binomial( link = "logit" ),
#'               na.action = stats::na.omit )
#'
#' ## Identify minimum expected value and % of expected counts < 5
#' expected_values( model )
#'
#' ## Inspect the invisibly returned set of results
#' res <- expected_values( model )
#' res
#'
expected_values <- function(model) {

  # Check supplied model has been fitted using glm( )
  if ( "glm" %in% class( model ) == FALSE )
    stop( "model must be a model created using the glm( ) function." )

  #Create a 'main effects only' model formula (in case the supplied model includes interaction terms)
  #since the 'main effects only' provides the expected counts in the absence of any interactions.

  #Also create a 'main effects only' forumla to use when summing up the expected counts across the
  #main effect variable(s)

  var.names <- names(model$model)
  var.names

  #Find the no. of variables in the data.frame
  n.vars <- length(var.names)

  #Create the 'main effects only' version of the model formula required to fit a log-linear model
  loglin.model.formula <-
    stats::as.formula(
      paste(  "Freq", "~",
              paste( var.names[1:(n.vars-1)], "+", collapse=''),
              var.names[(n.vars)]
      )
    )

  #Create the 'main effects only' version of the formula required by the aggregate function
  aggregate.formula <-
    stats::as.formula(
      paste( "expected.count",
             "~",
             paste(var.names[1:(n.vars-1)], "+",collapse=''),
             var.names[(n.vars)]
      )
    )

  #Create a data.frame storing the frequency count for each table cell, in order to
  #provide a labelled data.frame to which fitted loglinear expected values can be added
  df <- as.data.frame( tab( model$formula, data = model$model ) )

  #Fit the 'main effects only' log-linear model
  model.main.effects <-
    stats::glm( loglin.model.formula, data=df,
                family = stats::poisson,
                na.action= stats::na.omit )

  #Add the expected counts from the main effects only loglinear model
  df$expected <- model.main.effects$fitted

  #Change the name of the 'Freq' column to 'observed.count'
  names(df)[names(df)=="Freq"] <- "observed"

  #Save results in output format
  min_expected <- min(df[,"expected"])
  pct_5plus <- sum(df[, "expected"] >= 5) / nrow(df) * 100
  expected_values <- df[ order( df[, "expected"] ), ]

  #Report the minimum expected value
  print( paste( "Minimum expected value:",
                format( round( min_expected , 2 ),
                        nsmall=2 )
  )
  )
  print( paste( "% of expected values >= 5:",
                format( round( pct_5plus, 2 ),
                        nsmall=2 )
  )
  )

  #Return results 'invisibly'
  invisible( list( min_expected=min_expected,
                   pct_5plus=pct_5plus,
                   expected.values=expected_values ) )

}
