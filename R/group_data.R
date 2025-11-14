#' Group continuous data
#'
#' @param data A vector of \emph{numeric} values.
#' @param method One of "equal" (equal class intervals), "quantile" (equal class observations) or "user" (user-defined class boundaries). Default is "equal".
#' @param breaks Either a single number specifying the number of categories required, or a numeric vector specifying the required class boundaries. E.g. \code{c(0, 5, 10, 15)}.
#' @param sep The character string used to separate the two parts of a class boundary. Default value: " to ".
#' @param output.dp Number of decimal places to be used when reporting class boundaries. Default is to retain the precision of the input values.
#' @param include.lowest Logical. If \code{TRUE} (default) the boundaries of the lowest class are defined as  \eqn{ lb \le{x} \lt{ub} } ). See Details.
#' @param integer.dp Threshold for identifying a set of \code{data} values as integer. The default is 6 (i.e. treat a set of values in which no fractional part of a number is >= 0.000001 as integer). See Details.
#' @param mid.point Logical. If \code{TRUE} then class is labelled by mid-point rather than by class boundaries. Default is \code{FALSE}.
#'
#' @details
#' The `cut( )` function under-pinning `group_data( )` is supplied with a set of class boundaries defining the lowest boundary of the first (lowest) class, and the upper boundary of the first and any subsequent classes.
#' Class boundaries are defined such that, for any given class, \eqn{ lb \lt{x} \le{ub} }. The one exception to this is the first (lowest) class, for which the lower boundary is defined by the minimum value of \emph{x}. In other words, \eqn{ lb \le{x} \le{ub} }.
#' Hence if `breaks = c(1, 2, 8, 10)`, the following classes will be defined: \eqn{1 \le{x} \le{2}}, \eqn{2 \lt{x} \le{8}}, and \eqn{8 \lt{x} \le{10}}. In this example, a data value of 8 would be allocated to the middle category of \eqn{2 \lt{x} \le{8}} because 8 is less than or equal to the upper class boundary of 8. It would not be allocated to the top category of \eqn{8 \lt{x} \le{10}}, because in the top category the lower boundary of 8 is not less than the data value of 8.
#' In contrast, a value of 1 would be allocated to the first category, \eqn{1 \le{x} \le{2}}, because the lower bound of 1 is less than or equal to the data value of 1.
#' To force the treatment of the  lower boundary of the first category to be consistent with the treamtment of the lower boundary of the other categories, set `include.lowest` to \code{FALSE}. Note that doing so will leave any observations with a value equal to the lower boundary of the first class unclassified.
#'
#' Users can choose to change the threshold at which `group_data( )` starts to treat values as integer via the \code{integer.dp} option. By default `group_data( )` treats a set of values which contain fractional components of 0.000001 or less as a set of integer values. (e.g. the set of values 3.0000001 and 9.000001 would be treated as the integer values 3 and 9.) This is to cater for floating point errors arising from a set of mathematical operations.
#'
#' The `cut( )` function uses a class label of the form \code{[lb,ub]} to indicate a class interval where \eqn{ lb 'le{x} \le{ub} }; and the class label \code{(lb,ub]} to indicate a class interval where \eqn{ lb 'lt{x} \le{ub} }.
#' `group_data( )` replaces this with a class label which is easier to understand (e.g. 1 to 2, 3 to 8, 9 to 10). If the default `output.dp` are used, then the class boundaries will be defined in a way which ensures that the boundaries (a) do not overlap and (b) clearly indicate which class every data value falls in.
#' The default precision (decimal places) used to define the class boundaries matches that of the precision of the input data beingn classified. For example, if the input data is specified to 3 d.p. (e.g. 1.123) then class boundaries of the form 1.000 to 1.777 and 1.778 to 2.412 would be used. As the input data are measured only to 3 d.p., there can be no value of 1.7775, and hence no uncertainty regarding which class each data value will fall in.
#' However, to make for more readable class labels, the user can request that fewer decimal places are used in the class labels (although full precision will still be used in the class allocation). For example, if the `output.dp` were set to 1, data recorded to three decimal places might be classified using the class boundaries 1.000 to 1.777, and 1.778 to 2.412, but labelled as the classes 1.0 to 1.8 and 1.8 to 2.4.
#' `group_data( )` offers two other features not offered by `cut( )`. First, it automatically calculates the clas boundaries for quantile-based classes. Second, it automatically identifies and removes any duplicate upper class boundaries. E.g. the set of breaks 0, 2, 2, 2, 4, 8, 10 would automatically be converted into the set of class boundaries 0, 2, 4, 8, 10, accompanied by a warning message that this had been done.
#'
#' @return Factor vector, providing one class label per \code{data} value.
#' @export
#'
#' @examples
#' # Group data into five equal-interval categories
#' group_data( data = survey$Height, breaks = 5 ) |> table( )
#'
#' # Report class boundaries to n decimal places
#' group_data( data = survey$Height, breaks = 5, output.dp = 2 ) |> table( )
#'
#' # Group data into five quantiles
#' group_data( data = survey$Height, method = "quantile", breaks = 5,
#'  output.dp = 2 ) |> levels( )
#'
#' # Group data into three user-defined categories
#' group_data( data = survey$Height, method = "user",
#'   breaks = c(130, 160, 180, 210), output.dp = 2 ) |> table( )
#'
#' # Report class mid-point instead of upper and lower bounds
#' group_data( data = survey$Height, breaks = 5, output.dp = 2,
#'             mid.point = TRUE ) |> table( )
#'
#' # Threshold for treating a data value as integer rather than continuous
#' group_data( data = survey$Height, breaks = 5, integer.dp = 0 ) |> table( )
#'
#' # Change separator used to construct class labels
#' group_data( data = survey$Height, breaks = 5, integer.dp = 0, sep = "-") |>
#'  table( )
group_data <- function( data = data, method = "equal",  breaks = 5,
                        sep = " to ", output.dp=NULL, include.lowest=TRUE,
                        integer.dp= 6, mid.point=FALSE) {

  # This is a wrapper for the cut() function designed to provide more immediately user-friendly
  # (if less scientifically accurate) category labels.
  # cut() provides category labels in "(LB,UB]" format, with each LB being the same as its
  # preceding UB, since (LB,UB] means LB < x <= UB
  # For novice analysts (and non-expert consumers of research results), the unfamiliar
  # mathematical representation of bounds is problematic,
  # Their preferred output format is "LB-UB", using LBs that do not overlap their preceding
  # UBs. (E.g. 16-18; 19-21 etc.)
  # This wrapper uses cut() to categorise the supplied data, but then amends the category
  # labels returned by cut() along the lines outlined above.
  # For integer data, the revised category labels lose no information.
  # For real data, the conversion can leave unclear which category a fractional sits in if
  # the data are measured to greater precision than the class boundaries.
  # E.g. Does 0.71 fall in category 0.6-0.7 or 0.8-0.9.
  # For the intended user base and results audience, this loss in scientific precision
  # is, hopefully, more than compensated for by  more immediately accessible class boundaries.

  # Function to find number of decimal places in each data value
  # [Vectorised version of answer to
  #  https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r]

  decimal_places <- function(x) {

    dps <- NULL

    # Strip out any NA values, as they break the subsequent code
    # and are not relevant to finding the number of decimal places
    # used in valid values
    x <- x[ which( !is.na( x ) ) ]

    for (i in 1:length(x) ) {
      if (abs(x[i] - base::round(x[i])) > .Machine$double.eps^0.5) {
        dps[i] <-
          nchar( base::strsplit( sub('0+$', '', as.character( x[i] ) ),
                                 ".",
                                 fixed = TRUE)[[1]][[2]])
      } else {
        dps[i] <- 0
      }
    }

    return(dps)

  }

  # If data is one column data.frame, convert to vector; report error if
  #  data is a 2+ column data.frame
  if ( is.data.frame( data) ) {
    if ( ncol( data ) > 1 )
      stop("Only vectors or single column data.frames/tibbles accepted as ",
           "as valid data input")
    # Use dplyr::pull to ensure that any single column data.frames supplied
    # via dplyr::select( ) are treated as vectors instead
    data <- dplyr::pull( data )
  }

  #Check to see if, to all intents and purposes, data are actually integer
  fractional.values <- which((data %% 1) > 10^-integer.dp)

  #data <- as.integer(data) doesn't stop cut() returning fractional value labels; therefore
  #need to auto-detect of values to be cut are integer and take appropriate action as a result
  #[i.e. produce class labels using integer-based class boundaries]
  if (length(fractional.values)==0)  {
    integer.values <- TRUE
  } else {
    integer.values <- FALSE
  }

  #Find size of max. integer (i.e. places above the decimal point)
  integer.places <- nchar(as.character(floor(max(data, na.rm=TRUE))))

  #If output.dp supplied by user is >0, but integer data supplied, over-ride
  # and set to 0dp (unless mid.point == TRUE);
  # then warn user that this has been done
  if ( ( is.null( output.dp ) == FALSE ) & ( integer.values == TRUE ) &
       ( mid.point == FALSE ) ) {
    if (output.dp > 0) {
      output.dp <- 0
      warning("For integer data values, only integer class bounds will be ",
              "returned, so output.dp reset to 0 ", call. = F)
    }
  }

  # Identify number of decimal places data are measured to
  dps <- max( decimal_places( data ) )

  #If output.dp not supplied by user, set to default value
  #[same as precision to which input data is measured, or one more
  # if mid.point == TRUE]
  if (is.null(output.dp)==TRUE) {
    if (integer.values==TRUE) {
      #if (mid.point == FALSE) {
        output.dp <- 0
      #} else {
      #  output.dp <- 1
      #}
    } else {
      output.dp <- dps
    }
  }

  #Set dig.lab to no. of places in supplied data (+ requested output.dp, if any) + 1
  #This:
  #(a) avoids the danger of cut() returning values in scientific notation (e.g. 2e+01),
  #which it will if dig.lab < places
  #[Obviously not great if handling very large values, but this is unlikely to
  #be an issue for social survey analysis]
  #(b) ensures a precision one dp greater than that used in final labelling,
  #allowing use of strategy of LB = rounded cut() UB labels; LB = UB - 10^output.dp

  if (is.null(output.dp)==TRUE) {
    dig.lab <- integer.places +1
  } else {
    dig.lab <- integer.places + output.dp + 1
  }

  # Check that supplied grouping  method is valid
  method <- tolower( method )
  if ( method %in% c("user", "equal", "quantile") == FALSE )
    stop("Supplied grouping method is not one of user, equal or quantile.",
         call. = F)

  # If method = user, check that 2+ breaks have been supplied, and are
  # numeric.
  if ( method == "user" ) {
    if ( length( breaks ) == 1)
      stop("User-defined grouping method specified, but only one break value ",
           "supplied when minimum required is two.", call. = F)
    if ( is.numeric( breaks ) == FALSE )
      stop("User-supplied breaks are not numeric.", call. = F)
  }

  # If more than one break value supplied and method is
  # equal (i.e. equal interval) OR quantile (i.e. equal data),
  # reset method to user and warn that corrective action has been taken
  if ( method == "equal" & length(breaks) > 1 ) {
    warning("More than one breaks value specified so method reset to user",
            call. = F )
    method <- "user"
  }
  if ( method == "quantile" & length(breaks) > 1 ) {
    warning("More than one breaks value specified so method reset to user",
            call. = F )
    method <- "user"
  }

  # Calculate breaks to be used, if quantiles required
  if ( method == "quantile" ) {
    cum_proportions <- seq( from = 0, to = 1, by = 1 / breaks )
    breaks <- stats::quantile( data, probs = cum_proportions, na.rm = TRUE )
  }

  # Calculate breaks to be used if equal  intervals required
  # [Although cut( ) will calculate the required breaks if called as
  # cut( data, breaks = 5), cut() over-inflates the top UB by +.001 x range.
  # This can therefore lead to misleading UBs: e.g. max value = 99, which cut
  # inflates to >= 100, and hence will never be rounded down to 99, regardless
  # of selected output.dp. The same problem applies at the bottom LB, which
  # cut() deflates by -0.001 x range. In addition, it is hard to extract the
  # precise break values used by cut( ) from the labels it returns, which
  # may or not report to the full precision used to generate the cut.]
  #
  # Instead, group_data currently:
  # (a) fixes top UB at max bound specified by user (if any);
  #     else at max data value
  # (b) fix bottom LB at min bound specified by user (if any);
  #     else at min data value
  # This may need to be re-visited to deduct smallest possible difference in
  # data values from first lower bound, and add this difference to final
  # upper bound if it turns  out that cut fails to correctly classify the
  # min and max data values due to rounding issues.

  if ( method == "equal" ) {
    range <- max( data, na.rm = TRUE ) - min( data, na.rm = TRUE )
    breaks <- seq( min( data, na.rm=TRUE ), max( data, na.rm=TRUE ),
                   by = range/breaks )
  }

  # Remove any duplicate breaks, providing warning if this is done
  if ( any( duplicated( breaks ) ) ) {
    warning("The following duplicate breaks (class boundaries) have been ",
            "removed: ", breaks[ duplicated( breaks ) ], call. = F )
    breaks <- breaks[ !duplicated(breaks) ]
    if (length(breaks) < 2)
      stop("After removal of duplicate breaks only one break value is left. ",
           "As a minimum two break values are required (one lower and ",
           "upper bound.")
  }

  # Classify values using cut( )
  cut.result <- cut( data, breaks = breaks,
                     include.lowest= include.lowest, dig.lab=dig.lab)

  # Capture number of categories
  n <- length(breaks) - 1

  # Capture upper bounds from breaks used
  upper.bounds <- breaks[2:length(breaks)]

  # All but first lower breaks = previous upper break + smallest possible
  # fractional difference between values given precision data have been
  # measured to. First lower break = first break value, unadjusted.
  lower.bounds <- breaks[1:n]
  lower.bounds[2:n] <- lower.bounds[2:n] + 10^-dps

  # Calculate difference between each upper bound
  bounds.interval <- upper.bounds[1] - lower.bounds[1]
  for (i in 2:n ) {
    bounds.interval[i] <- upper.bounds[i] - upper.bounds[i-1]
  }

  #Check to see if user has requested interval mid-points to be used as labels
  # instead of class boundaries. Convert labels to interval mid-points
  # if required
  if (mid.point == TRUE) {
     mid.points <- upper.bounds - (bounds.interval / 2)
     if ( upper.bounds[ n ] == Inf ) mid.points[ n ] <- Inf
  }

  # if mid-points have been requested, return suitable formatted mid-points,
  # else return suitably formatted class labels based on lower and upper
  # bounds

  if (mid.point == TRUE) {

    if ( integer.values == TRUE ) output.dp <- 1

    # Round mid.points to required output.dp, then convert into
    # character strings that retain thes dp, and removes preceding spaces
    labels <-
      format( base::round(mid.points, output.dp), nsmall = output.dp ) |>
      stringr::str_remove(" ")
  }

  if (mid.point != TRUE) {
    # Round bounds to required output.dp, 10^output.dp to all but
    # first lower bound, to avoid upper/lower bound overlap,
    # then convert into a character string
    # that retains these dp, and removes and preceding spaces
    # [earlier calculated version of lower bounds used maximum
    #  accuracy to provide maximum accuracy of mid-points, if needed]
    lower.bounds <- breaks[1:n]
    lower.bounds[2:n] <- lower.bounds[2:n] + 10^-output.dp
    lower.bound.labels <-
      format( base::round(lower.bounds, output.dp), nsmall = output.dp ) |>
      stringr::str_remove(" ")
    upper.bound.labels <-
      format( base::round(upper.bounds, output.dp), nsmall = output.dp ) |>
      stringr::str_remove(" ")

    #Check to see if the upper bound is infinite.
    #If so, replace with top-coding. E.g. 20+ instead of 20 to Inf
    if (breaks[length(breaks)]==Inf) {
      upper.bound.labels[n] <- paste(lower.bounds[n],"+",sep="")
    }

    # Create class labels
    labels <- paste0(lower.bound.labels, sep, upper.bound.labels)

  }

  # replace labels created by cut with new labels
  levels(cut.result) <- labels

  return(cut.result)

}
