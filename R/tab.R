#' (Cross-)tabulate survey and admin data
#'
#' @param data A data.frame or tibble containing the dataset to be tabulated
#' @param formula A formula specifying the tabulation required, of the form: ~ x, or y ~ x, or y ~ x + z
#' @param weights The name of the `data` variable, if any, to be used to weight the tabulation.
#' @param measure The type of tabulation required. Specifies both the metric to be used (count, percentage or proportion) and the directionality of this metric (row, column or joint). Options are "count", "row_pct", "col_pct", "joint_pct", "row_prop", "col_prop" and "joint_prop".
#' @param totals Which totals (table margins), the table should include. Options are: "none" (the default), row", "col" and "row_col".
#' @param cum_sum Whether or not values should be cumulated across the cells of the table. Options are FALSE (the default) or TRUE.
#' @param base_count Whether or not the base count associated with any percentages or proportions should be included in the table. Options are FALSE (the default) or TRUE.
#' @param total_3d  Whether or not three-way tables should include a final two-way table summing values across the third table dimension. Options are FALSE (the default) or TRUE.
#' @param na.rm  Whether or not NA (missing) values should be excluded from the table. Options are FALSE (the default) or TRUE.
#'
#' @return A one-, two-, or three-way tabulation, supplied as an object of class table, xtable and tab.
#' @export
#'
#' @examples
#' # Load toy dataset supplied with package
#' #load(survey)
#'
#' # Tabulation examples
#'
#' # Frequency table (counts), with and without missing values
#' survey |> tab( ~WorkStatus, na.rm = TRUE )
#' survey |> tab( ~WorkStatus, na.rm = FALSE )
#'
#' # Freuquency table (proportions and cumulative proportions)
#' survey |> tab( ~ Age, measure = "col_prop" )
#' survey |> tab( ~ Age, measure = "col_prop", cum_sum = TRUE )
#'
#' # Two-way cross-tabulation (column percentages; row percentages with row totals)
#' survey |> tab( Sex ~ Age, measure = "col_pct" )
#' survey |> tab( Sex ~ Age, measure = "row_pct", totals = "row_col" )
#'
#' # Two-way cross-tabulation (row percentages with row totals and base count)
#' survey |> tab( Sex ~ Age, measure = "row_pct", totals = "col",
#'                base_count = TRUE )
#'
#' # Three-way cross-tabulation including counts aggregated across third dimension
#' survey |> tab( Health ~ Age + Sex, measure = "count", total_3d = TRUE )
#'
#' # All-singing all-dancing three-way table
#' survey |> tab( Health ~ Age + Sex, measure = "row_pct", totals = "row",
#'                base_count = TRUE, total_3d = TRUE )
tab <- function( data=NULL, formula=NULL, weights,
                 measure="count", totals="none",
                 cum_sum=FALSE, base_count = FALSE,
                 total_3d = FALSE,
                 na.rm=FALSE) {

  #=== Declare internal add_3d_sum function ===

  add_3d_sum <- function( old_table ) {
    # For three-dimensional tables, add an extra final sub-table
    # that sums counts over the third dimension of the table

    # Identify required table dimensions for updated table
    table_dimensions <- dim(old_table)
    table_dimensions[3] <- table_dimensions[3] + 1

    # Capture attributes of existing table
    dimname_list <- dimnames(old_table)

    # Add a final 'All' category to the third table dimension/variable
    n_categories <- length(dimname_list[[names(dimname_list[3])]]) + 1

    dimname_list[[names(dimname_list[3])]][[n_categories]] <-
      paste0("All ",names(dimname_list[3]))

    # Create new table, appropriately dimensioned and labelled
    new_table <- array(NA, dim=table_dimensions)
    dimnames(new_table) <- dimname_list
    class(new_table) <- "table"

    # Copy in counts from original table
    new_table[ , , -dim(new_table)[3]] <- old_table

    # Sum across dimension 3 and store results in new final sub-table
    new_table[ , , dim(new_table)[3]] <- rowSums(old_table, dims = 2)

    return( new_table )

  } # add_3d_sum

  # === Declare internal add_totals function

  add_totals <- function( count_table, res_table, mes, mes_type, tot) {

    # Add requested margins, taking account of mix of measure and
    # table marginal (total) being requested.
    #
    # For counts and joint proportions, simply find requested
    #   row and/or col totals
    # For row percentages, row total = sum of row percentages
    # For col percentages, col total = sum of col percentages
    # For row percentages, col total = (i) find sum of col COUNTS;
    #   (ii) convert col sums to row percentages
    # For col percentages, row total = (i) find sum of col COUNTS;
    #   (ii) convert row sums to col percentages

    # Identify the total margins to be added to table
    # [or to each 2D table within a larger 3D table]
    if (tot == "none")
      stop("Error: add_totals called even though tot == none", call.=F)

    if (tot == "row_col") total_margin <- c(1,2)
    if (tot == "row") total_margin <- c(2)
    if (tot == "col") total_margin <- c(1)

    # Add relevant margin, given measure and total choices

    ## If measure is count or joint prop/pct, simply apply requested totals
    if (mes_type == "count" | mes_type == "joint")
      res_table <- stats::addmargins(res_table, total_margin)

    ## If row prop/pct and row total, or col prop/pct and col total,
    ## simply apply requested total
    if (mes_type == "row" & tot == "row")
      res_table <- stats::addmargins(res_table, total_margin)
    if (mes_type == "col" & tot == "col")
      res_table <- stats::addmargins(res_table, total_margin)

    # If measure = row prop/pct and total = col or row_col,
    # find sum of col counts, convert to row prop/pct, then add row total
    # if also required.
    if (mes_type == "row" & (tot == "col" | tot == "row_col") ) {

      ## Sum col prop/pct
      ## [which gives wrong answer, but creates table of correct dimensions]
      res_table <- stats::addmargins(res_table, 1)

      ## Replace with correct answer
      res_table[ nrow(res_table), ]  <-
        prop.table(
          stats::addmargins(count_table, 1),
          1
        )[nrow(res_table), ]

      # Convert correct answer from proportions to percent if required
      if (mes == "pct") res_table[ nrow(res_table), ] <-
        res_table[ nrow(res_table), ] * 100

      ## Add row total, if required
      if (tot == "row_col") res_table <- stats::addmargins(res_table, 2)

    } # if (mes_type == "row" & (tot == "col" | tot == "row_col") )

    ## If measure = col prop/pct and total = row or row_col,
    ## find sum of row counts, convert to col prop/pct, then add col total
    ## if also required.
    if ( mes_type == "col" & (tot == "row" | tot == "row_col") ) {

      ## Sum col prop/pct
      ## [which gives wrong answer, but creates table of correct dimensions]
      res_table <- stats::addmargins(res_table, 2)

      ## Replace with correct answer
      res_table[ , ncol(res_table) ]  <-
        prop.table(
          stats::addmargins(count_table, 2),
          2
        )[ , ncol(res_table) ]

      # Convert correct answer from proportions to percent if required
      if (mes == "pct") res_table[ , ncol(res_table) ] <-
        res_table[ , ncol(res_table) ] * 100

      ## Add col total, if required
      if (tot == "row_col") res_table <- stats::addmargins(res_table, 1)

    } # if (mes_type == "col" & (tot == "row" | tot == "row_col") )

    return( res_table )

  } #add_totals


  #=== Declare internal cumsum_2d function

  cumsum_2d <- function( table_in, met_type ) {

    if ( met_type %in% c("count", "joint") ) {
      tmp <- t( table_in )
      tmp[ , ] <- cumsum( t(table_in) )
    } else if ( met_type == "row" ) {
      tmp <- apply( table_in, 1, cumsum )
    } else if ( met_type == "col" ) {
      tmp <- t( apply( table_in, 2, cumsum ) )
    }

    return( t(tmp) )

  }

  #=== Declare internal add_base_count function

  add_base_count <- function( res_tab, count_tab, met_type, tot, n_var ) {

    if (met_type =="row") {

      # if res table includes a column total, add an extra row
      # containing the col total to the count table
      # (only applies to tables with 2+ variables because row totals not
      #  supported for 1-d tables)
      if ( tot %in% c("col", "row_col") & n_var > 1)
        count_tab <- stats::addmargins( count_tab, 1, sum )

      # Add an extra column to the results table, and change name from Sum to N
      res_tab <- stats::addmargins( res_tab, 2, sum )
      new_column <- length( colnames( res_tab ) )
      colnames( res_tab )[ new_column ] <- "N"
      # Replace contents of new column with row sum of counts
      res_tab[ , new_column ] <- rowSums( count_tab )
    }

    if (met_type == "col") {

      # if res table includes a row total, add an extra column
      # containing the row total to the count table
      # (only applies to tables with 2+ variables because row totals not
      #  supported for 1-d tables)
      if (tot %in% c("row", "row_col") & n_var > 1)
        count_tab <- stats::addmargins(count_tab, 2, sum)

      # Add an extra row to the results table, and change name from Sum to N
      res_tab <- stats::addmargins( res_tab, 1, sum )
      new_row <- length( rownames( res_tab ) )
      rownames( res_tab )[ new_row ] <- "N"
      # Replace contents of new column with row sum of counts
      res_tab[ new_row, ] <- colSums( count_tab )
    }

    return( res_tab )

  } # add_base_count

  #=== Declare internal change_sum_to_total function

  change_sum_to_total <- function( table_in, tot, base_count ) {

    if ( totals %in% c("row", "row_col") ) {
      sum_col <- ncol(table_in)
      if ( tolower( colnames( table_in )[ sum_col ] ) == "sum" )
        colnames( table_in )[ sum_col ] <- "Total"
    }

    if ( totals %in% c("col", "row_col") ) {
      sum_row <- nrow( table_in )
      if ( tolower( rownames( table_in )[ sum_row ] ) == "sum" )
        rownames( table_in )[ sum_row ] <- "Total"
    }

    return( table_in )

  } # change_sum_to_total


  #=== Start of main tal.R code ===

  # 1. Check values of parameters supplied

  ## (a) Check to see that a data.frame has been supplied
  if (is.null(data))
    stop('No data supplied: please specify a dataset using the data= parameter',
         call.=F)
  if (!is.data.frame(data))
    stop ( paste0('The first object supplied, via a pipe or argument name, ",
                  "must be a data.frame'),
           call.=F )

  ## (b) Check that a valid formula has been supplied.

  ### Count number of tildes in supplied formula

  # For some reason, adding this line of code allows causes the following
  # str_count command to work. Without it, an error is thrown

  formula_check <- purrr::is_formula( formula )

  n_tilde <- stringr::str_count( dplyr::as_label( dplyr::enquo(formula)), "~" )

  if ( n_tilde == 0 )
    stop( paste0("Table required must be specified using a formula ",
                 "(e.g. x ~ y), even if tabulating only one variable ",
                 "(e.g. ~ y)"),
          call.=F )

  if ( n_tilde > 1 )
    stop( paste0("Supplied formula contains more than one tilde. ",
                 "Required format is x ~ y OR ~ y"),
          call.=F )

  ### Extract a list of the variables names in the user-supplied formula
  ### and count the number of variables involved
  var_names <- all.vars(formula)
  n_vars <- length(var_names)

  ### Check that for formulas with 2+ variables, at least one is before tilde
  if (n_vars > 1) {
    text_before_tilde <-
      stringr::str_replace_all( dplyr::as_label( dplyr::enquo(formula)),
                                " ", "" ) |>
      stringr::str_split_i("~", 1)

    if ( nchar( text_before_tilde ) == 0 )
      stop( "For 2- and 3-way tables, a variable is required BEFORE the tilde.",
            " (e.g. x ~ y OR x ~ y + z)",
            call. = F )
  } # n_vars > 1

  ### Check that some text (i.e. a variable name) has been supplied
  ### AFTER the tilde
  ### [This code redundant because R won't pass x~  as a valid function
  ###  argument]
  text_after_tilde <-
    if ( nchar(stringr::str_replace_all( dplyr::as_label( dplyr::enquo(formula) ),
                                         " ", "" ) |>
               stringr::str_remove(".*~")
    ) == 0 )
      stop("A variable name is required AFTER the tilde. (e.g. x ~ y OR ~ y)",
           call. = F )

  ### Belt-and-braces final check to make sure a formula has been supplied
  if( methods::is(formula, "formula") == FALSE )
    stop( paste0('Specification of required tabulation not given in ',
                 'formula format (e.g. x ~ y OR ~ y'),
          call.=F )


  ### If object IS a formula:
  ### (TRUE by definition if the code gets this far due to checks above)

  ## (c) Check that only a 1, 2 or 3 dimensional table has been requested
  ##     [because function doesn't support 4+ dimensions]


  if (n_vars > 3)
    stop( paste0("This function only supports the generation of one, two and ",
                 "three-way tables. You have requested a ", n_vars,
                 "-dimensional table using the following variables: ",
                 paste0(var_names,collapse=' ') ),
          call.=F )

  ## (d) Check that the variable names in the user-supplied formula are valid
  ##     (i.e. appear in the  user-supplied data.frame)

  ### Identify variables (if any) not present in the user-supplied data.frame

  invalid_var_names <- which(var_names %in% names(data)==FALSE)

  ### If there are 1+ invalid variables in the supplied formula,
  # halt and provide an appropriate error message
  if (length(invalid_var_names) > 0) {

    ### If fewer than 50 variables in data.frame, list the available variables;
    ### otherwise tell user how to generate a list of available variables
    if (length(names(data))<50) {
      stop('\n',
           'The following variable(s) cannot be found in supplied data.frame:',
           '\n',
           paste0(var_names[invalid_var_names],collapse=' '), '\n', '\n',
           paste0("Variable names are case-sensitive. Variables available for ",
                  "selection are:"),'\n',
           paste0(names(data), collapse= " "),
           call. = F )
    } else {
      stop('The following variable names are not present in supplied ',
           'data.frame:','\n',
           paste0(var_names[invalid_var_names],collapse=' '), '\n', '\n',
           paste0("Variable names are case-sensitive. Use the names( ) ",
                  "function to view valid options."),
           call. = F )
    }

  }  # if > 0 invalid.var.names

  ## (e) Check that the supplied weight (if any) is valid.

  ### If a weights parameter has been supplied
  if (rlang::is_missing( dplyr::ensym(weights) ) == FALSE) {

    ## (e)[i] Check that name is valid (i.e. exists in supplied data.frame)
    if( dplyr::quo_name( dplyr::enquo(weights) ) %in% names(data) == FALSE )
      stop("Weight variable not present in supplied data.frame. Check spelling
            and capitalisation.", call. = F )

    ## (e)[ii] Check that supplied weight is a numeric vector
    if ( data |> dplyr::pull({{weights}}) |> is.numeric( ) == FALSE )
      stop( "Values of weights must be numeric, but are supplied as ",
            class( data |> dplyr::pull( {{weights}} ) ),
            call. = F )

    ## (e)[iii] Capture the name of the valid weights variable
    weights_name <- dplyr::quo_name( dplyr::enquo(weights) )

  } else {

    weights_name <- NULL

  }

  ## (f) Check that the requested measure is valid, then split measure
  ##     into its sub-components (metric and metric_type)
  valid.measures <- c("count", "col_pct",   "row_pct",  "joint_pct",
                      "col_prop", "row_prop", "joint_prop")

  measure <- tolower(measure)

  if( measure %in% valid.measures == FALSE)
    stop( "A valid measure was not requested. Available measures are:", "\n",
          paste0(valid.measures, collapse=' '),
          call. = F )

  if ( n_vars == 1 & measure %in% c("row_pct", "row_prop") ) {
    if (measure == "row_pct") measure <- "col_pct"
    if (measure == "row_prop") measure <- "col_prop"
    warning( paste0("Row-based measures not appropriate for univariate tables.",
                    "\nMeasure changed to ", measure ),
             call. = F )
  }

  metric <-
    stringr::str_remove(measure,"_") |>
    stringr::str_remove("row|col|joint")

  metric_type <-
    stringr::str_remove(measure,"_") |>
    stringr::str_remove("pct|prop")


  ## (g) Check that the requested type of total is valid

  valid_totals <- c("none", "row",   "col",  "row_col")

  totals <- tolower(totals)

  if( totals %in% valid_totals == FALSE)
    stop( "A valid total was not requested. Available totals are:", "\n",
          paste0(valid_totals, collapse=' '),
          call. = F )

  if( n_vars == 1 & ( totals %in% c("row", "row_col") ) ) {
    totals <- "col"
    warning( "Row totals are not appropriate for univariate tables.\n",
             "Column totals calculated instead.",
             call. = F )
  }

  ### Give warning if totals do not match requested row/col measure
  if ( stringr::str_detect(totals, "row") &
       stringr::str_detect(measure, "col") &
       n_vars != 1 )
    warning("Row totals represent the overall proportion/percentage in ",
            "that row, NOT the total of the row values",
            call. = F )

  if ( stringr::str_detect(totals, "col") &
       stringr::str_detect(measure, "row") &
       n_vars != 1 )
    warning("Column totals represent the overall proportion/percentage in ",
            "that column, not the total of the column values.",
            call. = F )

  ## (h) Check the total_3d request is valid

  valid_logical <- c(TRUE,FALSE, T, F)

  total_3d <- toupper(total_3d)

  if(total_3d %in% valid_logical == FALSE)
    stop('Valid values for total_3d are: TRUE and FALSE', call. = F )

  if( (total_3d == TRUE) & (n_vars < 3) ) {
    total_3d <- FALSE
    warning(
      paste0("total_3d = TRUE ignored: table has less than 3 dimensions,",
             " so sum of counts over third dimension not possible. "),
      call. = FALSE )
  }

  ## (i) Check the cum_sum request is valid

  valid_logical <- c(TRUE,FALSE, T, F)

  cum_sum <- toupper(cum_sum)

  if(cum_sum %in% valid_logical == FALSE)
    stop('Valid values for cumsum are: TRUE and FALSE', call. = F )

  if ( cum_sum == TRUE & totals != "none" ) {
    warning('Cumulative sum only reported if totals = none, to avoid ',
            'confusing output. \nTherefore value of cum_sum reset to FALSE, ',
            'and totals reported instead.',
            call. = F )
    cum_sum <- FALSE
  }

  ## (j) Check the base_count request is valid

  base_count <- toupper(base_count)

  if(base_count %in% valid_logical == FALSE)
    stop('Valid values for base_count are: TRUE and FALSE', call. = F)

  if (base_count == TRUE & metric_type %in% c("count", "joint") ) {
    warning('Base counts only available for row and column based measures.\n',
            'Therefore base_count reset to FALSE', call.=F )
    base_count <- FALSE
  }


  ## (k) Check the na.rm request is valid

  na.rm <- toupper(na.rm)

  if(na.rm %in% valid_logical == FALSE)
    stop('Valid values for na.rm are: TRUE and FALSE', call. = F )


  # 1. If a variable in the table has NA values, make NA a valid
  #    level for that variable
  #    [Unlike the xtabs( addNA = TRUE) option, this approach, allied with
  #     exclude = NA or NULL stops xtabs from dropping levels from other
  #     variable(s) in the table when they only cross-tabulate with NA values,
  #     instead returning a count of 0 against those level(s) ]

  for (i in 1:n_vars) {
    if ( any( is.na( data[,var_names[i]] ) ) )
      data[ , var_names[i]] <- addNA( data[ , var_names[i]] )
  }


  # 2. Convert the valid formula object (valid class; valid variables) into a
  #    xtabs formula format [i.e. weights ~ a + b]

  # 2(a) For 1-d tables, xtabs returns results as a 1d row vector. To
  #      force the required return of output in columns, it is necessary to:
  #      (i) add an additional column, 'Count', to the dataset being tabulated,
  #      with each entry in this new column comprising the single word 'All';
  #      (ii) convert the formula to var_name ~ Count format.

  if (n_vars == 1) {

    # Add new column 'Count', with the text 'All' in each row
    data <- cbind( data, Count=rep( "All" , nrow(data) ) )

    # Create required xtabs formula
    xtabs.formula <-
      stats::as.formula(

        paste(weights_name,
              "~",
              var_names,
              "+",
              "Count",
              collapse='' )
      )
  }

  ## 2(b) For 2- and 3-d tables, create valid xtabs formula
  if (n_vars > 1) {

    xtabs.formula <-
      stats::as.formula( paste(
        weights_name,
        "~",
        paste(var_names[1:length(var_names)-1], "+",collapse=''),
        var_names[length(var_names)]
      )
      )

  }

  ## 3. Create initial count table using xtabs.formula, treating
  ##    NA values as requested via na.rm

  if (na.rm == TRUE) {
    count_table <- stats::xtabs( xtabs.formula, data= data, exclude = NA )
  } else {
    count_table <- stats::xtabs( xtabs.formula, data= data, exclude = NULL )
  }


  # 4. Add sum of counts over third-dimension, if requested and possible

  if ( total_3d == TRUE & n_vars == 3 )
    count_table <- add_3d_sum( count_table )

  ## 4. For 1-way tables, change column header from "All" to relevant metric,
  ##    and change column dimension name from 'Count' to ''
  if ( n_vars == 1) {

    if ( metric == "count" ) {
      colnames( count_table ) <- "Frequency"
    } else if ( metric == "pct" ) {
      colnames( count_table ) <- "Percentage"
    } else if ( metric == "prop" ) {
      colnames( count_table ) <- "Proportion"
    }

    names(attributes(count_table)$dimnames)[2] <- ""

  }

  # 5.  Make res_table a direct copy of count_table

  res_table <- count_table


  # 6. Record number of 'inner tables'[where a 2d table has 0 inner tables,
  #     and a 3D table has one inner tables per third dimension category,
  #     including 1 for any 3d_total table]

  # inner tables
  if (n_vars < 3) {
    n_inner_tables <- 0
  } else if (n_vars == 3) {
    n_inner_tables <- dim(res_table)[3]
  }

  # 7. Convert to proportions if a percentage or proportion table required,

  if ( metric %in% c("prop", "pct")) {

    # Identify associated numeric prop_type, for use by prop.table()
    if (metric_type == "row") {
      prop_type <- 1
    } else if (metric_type == "col") {
      prop_type <- 2
    } else if (metric_type == "joint") {
      prop_type <- NULL
    }

    # Convert table from counts to proportions,
    # using user-specified metric_type (row, col, joint)
    # [necessary for ALL prop and pct tables]

    if ( metric_type %in% c("row", "col", "joint") ) {

      #If table has 1 or 2 dimensions
      if ( n_vars < 3 ) {
        res_table <- prop.table( res_table, margin = prop_type )
      }

      # If table has 3-dimensions, convert the constituent 2D tables into
      # proportions one at a time
      if ( n_vars == 3 ) {

        #Convert inner table to proportions along specified margin
        for ( i in 1:n_inner_tables ) {
          res_table[,,i] <- prop.table( res_table[,,i], margin = prop_type )
        } #next i
      } #if n_vars == 3

    } #if metric_type %in% row, col, joint

    # Correct for empty rows/cols/tables
    # [for rows/cols/tables containing only counts of zero,
    #  prop.table returns an empty (blank) row/col/table instead
    #  of a row/col/table zeros]

    if (n_vars == 1) {

      # For col proportions, set empty columns to 0
      if (metric_type == "col")
        res_table[ is.nan( colSums(res_table[ ]) ) ] <- 0

      # For row proportions, set empty rows to zero
      if (metric_type == "row")
        res_table[ is.nan( rowSums(res_table[ ]) ) ] <- 0

      # For joint proportions, set empty tables to 0
      if (metric_type == "joint")
        if ( is.nan( sum(res_table[ ]) ) ) res_table[ ] <- 0

    } else if ( n_vars == 2) {

      # For col proportions, set empty columns to 0
      if (metric_type == "col")
        res_table[ , is.nan( colSums(res_table[ , ]) ) ] <- 0

      # For row proportions, set empty rows to zero
      if (metric_type == "row")
        res_table[ is.nan( rowSums(res_table[ , ]) ), ] <- 0

      # For joint proportions, set empty tables to 0
      if (metric_type == "joint")
        if ( is.nan( sum(res_table[ , ]) ) ) res_table[ , ] <- 0

    } else if (n_vars == 3) {

      for (i in 1:n_inner_tables) {

        # For col proportions, set empty columns to 0
        if (metric_type == "col")
          res_table[ , is.nan(colSums(res_table[,,i])) , i] <- 0

        # For row proportions, set empty rows to zero
        if (metric_type == "row")
          res_table[ is.nan(rowSums(res_table[,,i])), , i] <- 0

        # For joint proportions, set empty tables to 0
        if (metric_type == "joint")
          if (is.nan(sum(res_table[,,i]))) res_table[,,i] <- 0

      } # next inner table

    }


    # If required, convert proportions into percentages

    if (metric == "pct") res_table <- res_table * 100

  } # if metric is prop or pct


  # 8. Add table totals (margins), if required

  if ( totals != "none") {

    # For 1 and 2d tables, call is straight-forward...
    if (n_vars < 3)
      res_table <-
        add_totals( count_table, res_table, metric, metric_type, totals )

    # For 3d tables, call is also straight-forward for metric_types
    # 'count' and 'joint', and for calls where totals == metric_type
    # [e.g. row totals for row metric_type]
    if (n_vars == 3 &
        ( metric_type %in% c("count", "joint") | metric_type == totals ) )
      res_table <-
        add_totals( count_table, res_table, metric, metric_type, totals )

    # For 3d tables where metric_type is row or col AND this doesn't
    # match requested totals, then need to call add_totals once for
    # for each inner table, since need to use table counts to
    # calculate the requested total independently of the already
    # calculated interior cells of each inner table.
    if ( n_vars == 3 &
         metric_type %in% c("row", "col") &
         metric_type != totals ) {

      # Create temporary table large enough to store results
      # for each inner table
      temp_table <- res_table

      if ( totals %in% c("row", "row_col") )
        temp_table <- stats::addmargins(temp_table, margin = 2)
      if ( totals %in% c("col", "row_col") )
        temp_table <- stats::addmargins(temp_table, margin = 1)

      for (i in 1:n_inner_tables) {
        temp_table[,,i] <-
          add_totals( count_table[,,i], res_table[,,i],
                      metric, metric_type, totals )
      } #for

      res_table <- temp_table

    } #if (n_vars == 3)

    # Change the row/column label 'Sum' to 'Total'
    res_table <- change_sum_to_total( res_table, tot, base_count )

  } # if totals != "none"

  # 9. Convert table to cumulative sums, if required

  if ( cum_sum == TRUE ) {

    # Only available if totals == none
    # (checks for validity of cum_sum already screen for this)

    # Not appropriate for 1-d table with metric_types row_xxx
    # (checks for validity of measure already screen for this )

    if ( n_vars == 1) {
      # Cum sum for 1-d tables
      res_table[ ] <- cumsum( res_table )
    } else if (n_vars == 2) {
      # Cum sum for 2-d tables
      res_table <- cumsum_2d( res_table, metric_type )
    } else if (n_vars == 3) {
      # Cum sum for 3-d tables
      for (i in 1:n_inner_tables) {
        res_table[,,i] <- cumsum_2d( res_table[,,i], metric_type )
      }
    }

  } # if cum_sum == TRUE

  # 10. Add base count (if requested)

  if (base_count == TRUE) {

    if (n_vars < 3)
      res_table <-
        add_base_count( res_table, count_table, metric_type, totals, n_vars )

    if (n_vars == 3) {
      # Create temporary version of res_table with the additional
      # rows/cols required to store the result of adding a base count
      tmp_table <- res_table

      #if (metric_type == "row" & (totals %in% c("col", "row_col") ) )
      if ( metric_type == "row" )
        tmp_table <- stats::addmargins(tmp_table, 2, sum)

      #if (metric_type == "col" & (totals %in% c("row", "row_col") ) )
      if ( metric_type == "col" )
        tmp_table <- stats::addmargins(tmp_table, 1, sum)

      # Add a base count to each inner table in turn
      for (i in 1:n_inner_tables) {
        tmp_table[,,i] <-
          add_base_count( res_table[,,i], count_table[,,i],
                          metric_type, totals, n_vars )
      }

      # Update res_table
      res_table <- tmp_table

      # Change name of base_count row/column from 'Sum' to 'N'
      # [needed because the row/column names from add_base_count function
      #  results are NOT retained when copied into tmp_table]
      if ( metric_type == "col" ) {
        base_row <- length( rownames( res_table ) )
        rownames(res_table)[ base_row ] <- "N"
      }
      if ( metric_type == "row" ) {
        base_column <- length( colnames( res_table ) )
        colnames(res_table)[ base_column ] <- "N"
      }


    } # if n_vars == 3

  } # if base_count == TRUE

  # Assign attribute to table for use by publish_tab function
  attributes(res_table)$n_vars <- n_vars
  attributes(res_table)$totals <- totals
  attributes(res_table)$base_count <- base_count
  attributes(res_table)$metric <- metric
  attributes(res_table)$metric_type <- metric_type

  # Update classes assigned to res_table
  class( res_table ) <- append( class( res_table ), "tab" )

  return( res_table )


} # end of function
