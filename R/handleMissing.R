#' handleMissing
#'
#' @description 'handleMissing' takes a dataframe and filsl or removes missing values. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class. Function defaults to user interaction.
#'
#' @param df A dataframe with any combination of variable classes, dates must be properly classified as dates.
#' @param no_drop A boolean indicating whether or not to drop values, TRUE skips all dropping and proceeds to imputation.
#' @param no_impute A boolean indicating whether or not to impute values. TRUE skips imputation.
#' @param drop_col_tol A percent tolerance to automatically drop columns with percent missing values greater than or equal to this value. Report in percent from 0 to 100.
#' @param drop_row_tol A percent tolerance to automatically drop rows with percent missing values greater than or equal to this value.If values are provided for both columns and rows, columns will be dropped first.Report in percent from 0 to 100.
#' @param drop_user_level An indicator of whether the user will provide input or if the user would like to fully automate the drop process, 1 indicates user interaction.
#' @param impute_user_level An indicator of whether the user will provide input or if the user would like to fully automate the imputation process, 1 indicates user interaction.
#' @param impute_method A string indicating the method of imputation. If no_impute is set to TRUE, this is ignored.
#' @param impute_factors A boolean indicating whether or not to impute factor columns. If set to TRUE, the value occurring most frequently is applied to missing values.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'  \item \code{df}:  A dataframe with missing values dropped and/or imputed, according to user input.
#'  \item \code{missing_stats}: A dataframe providing details about which columns were dropped and what percent missing values were identified throughout several steps, according to user input. If imputation is selected, this will include the method of imputation performed on each column.
#'  \item \code{dropped_cols}: A list of column names that were dropped from the original dataframe.
#'  \item \code{dropped_rows}: A list of row indices that were dropped from the original dataframe.
#'  }
#'
#' @export
#'
#' @examples
#' # Define the dataframe, clear of special characters
#' df <- handleSpecial(fires, special_user_level =0)$df
#'
#' # Store function results
#' out <- handleMissing(df, drop_col_tol = 80,  = 0)
#' str(out$df)
#'
handleMissing <- function(df, no_drop = FALSE, no_impute = FALSE, drop_col_tol = NULL, drop_row_tol = NULL, drop_user_level = 1, impute_user_level = 1, impute_method = 'median', impute_factors = FALSE){

  # Check to make sure user has supplied enough parameters to execute the function. Must specifiy at least one drop tolerance, or utilize user interaction.
  if(is.null(drop_col_tol) & is.null(drop_row_tol) &  drop_user_level== 0 & impute_user_level == 0) stop("Please supply at least one tolerance, or set at least one user_level to 1.")
  if(no_drop & no_impute) stop("You have indicated that you do not want to drop or impute. No actions to perform with these selections. Please set no_drop or no_impute to FALSE.")

  # Handle various impute_method methods
  means <- c('Mean', 'mean', 'mean()', 'Mean()','Average', 'average', 'avg', 'AVG', 'Avg')
  medians <- c('Median', "median", 'median()', 'Median()' ,'Med', 'med', 'middle','Middle')
  randos <- c('Random', 'random', 'rand', 'Rand', 'random sample', 'Random sample', 'Random Sample')
  modes <- c('Mode', 'mode', 'most frequent', 'Most frequent', 'Most Frequent')
  zeros <- c('Zero' ,'zero', '0')

  # If the impute_method provided does not match a valid method, stop and request different impute method.
  if(!impute_method %in% means & !impute_method %in% medians & !impute_method %in% randos & !impute_method %in% modes & !impute_method %in% zeros) stop("The specified imputation method is not a supported method. Please reference the handleMissing() documentation to see supported functions.")

  # Calculate the percentage of missing values in each column of the dataframe and save information to missing_stats
  # Perform separate calculation on date/time columns
  initial_percent_missing <- sapply(df, function(column) {
    if (inherits(column, "POSIXlt") | inherits(column, "Date") |inherits(column, "POSIXct")) {
      return(100 * sum(is.na(column)) / length(column))
    } else {
      return(100 * sum(is.na(column) | column == '') / length(column))
    }
  })

  missing_stats <- data.frame(variable = names(initial_percent_missing), initial_percent_missing = round(initial_percent_missing, 1) )

  # Define blank vectors for storing names of dropped columns and rows
  dropped_cols <-c()
  dropped_rows <- c()

  # If the user wants to drop columns or rows
  if (no_drop == FALSE){

    # If user defines a column drop tolerance, drop columns which have greater than or equal value of % missing
    if (!is.null(drop_col_tol)){
      remove <- missing_stats$variable[missing_stats$initial_percent_missing  >= drop_col_tol]
      dropped_cols <- unique(append(dropped_cols, as.vector(remove)))
      df <- df[, !names(df) %in% dropped_cols]


      # Update stats with % missing after dropping
      after_drop <- data.frame(after_col_drop_percent = round( sapply(df, function(column) {
        if (inherits(column, "POSIXlt") | inherits(column, "Date") |inherits(column, "POSIXct")) {
          return(100 * sum(is.na(column)) / length(column))
        } else {
          return(100 * sum(is.na(column) | column == '') / length(column))
        }
      }), 1))
      after_drop$variable <- rownames(after_drop)
      missing_stats <- suppressWarnings(merge(missing_stats, after_drop, by = 'variable', all =TRUE, no.dup=TRUE))
      missing_stats[is.na(missing_stats)] <- 'dropped'


    }

    # If the user indicated they want input in the drop process.
    if ( drop_user_level == 1){
      print(missing_stats)
      cat('\n')
      input <- utils::menu(c("Drop all missing", "Select which to drop", "Set a drop tolerance", "See missing by row", "None, I'm done dropping"),title = "Above are the percent missing by column for your dataframe. Which would you like to do? ")

      # If the user wants to drop all columns with any missing values.
      if (input == 1){
        remove <- missing_stats$variable[missing_stats$initial_percent_missing  > 0]
        dropped_cols <- unique(append(dropped_cols, as.vector(remove)))
        df <- df[, !names(df) %in% dropped_cols]
      }

      # If the user wants to select which columns to drop
      if (input == 2){
        if(ncol(missing_stats) == 2){
          options <- sprintf("%-30s %.1f", missing_stats$variable, missing_stats$initial_percent_missing)
        }
        else{
          options <- sprintf("%-30s %-6s %-4s", missing_stats$variable, missing_stats$initial_percent_missing, missing_stats$after_col_drop_percent)
        }

        to_drop <- coda::multi.menu(options, title = "Which would you like to drop?")

        if(sum(to_drop) != 0 ){
          drop_names <- missing_stats$variable[to_drop]
          df <- df[which(!names(df) %in% drop_names)]
          dropped_cols <- append(dropped_cols, drop_names)
        }
      }

      # If the user wants to set a column drop tolerance
      if (input == 3){
        tol <- readline(prompt = "What tolerance would you like to set? ")
        remove <- missing_stats$variable[missing_stats$initial_percent_missing >= tol ]
        dropped_cols <- unique(append(dropped_cols, as.vector(remove)))
        df <- df[, !names(df) %in% dropped_cols]
      }

      # If the user wants to view percent missing by observation (row)
      if (input == 4){

        # Coerce dates to numeric to prevent errors and calculate % missing by row
        df_temp <- .tryNumeric(df)
        obs_initial_percent_missing <- apply(df_temp, 1, function(x) 100 * sum(is.na(x) | x =='') / length(x))

        # Output a histogram of percent missing
        graphics::par(mfrow=c(1,1))
        graphics::hist(obs_initial_percent_missing, main = "Histogram of percent missing by rows", xlab ="Percent missing")

        # Get user feedback on how they would like to progress.
        row_input <- utils::menu(c("Set row drop tolerance", "Exit, I don't want to drop any rows!"), title="Please view the histogram of observation percent missing (by row). What would you like to do now? ")

        # If user would like to set a drop tolerance for rows, take tolerance, remove rows and update dropped_rows
        if (row_input == 1){
          row_tol <- readline(prompt = "What tolerance would you like to set? ")
          df <- df[which(obs_initial_percent_missing < row_tol), ]
          dropped_rows <- append(dropped_rows,as.vector(which(obs_initial_percent_missing >= row_tol)))
        }
      }

      # Update stats with % missing after dropping
      after_drop_percent <- data.frame(after_drop_percent = round(sapply(df, function(column) {
        if (inherits(column, "POSIXlt") | inherits(column, "Date") |inherits(column, "POSIXct")) {
          return(100 * sum(is.na(column)) / length(column))
        } else {
          return(100 * sum(is.na(column) | column == '') / length(column))
        }
      }), 1))
      after_drop_percent$variable <- rownames(after_drop_percent)
      missing_stats <- suppressWarnings(merge(missing_stats, after_drop_percent, by = 'variable', all =TRUE, no.dups = TRUE))
      missing_stats[is.na(missing_stats)] <- 'dropped'

    }

    # If user defines a row drop tolerance, drop rows which have greater than or equal value of % missing
    if (!is.null(drop_row_tol)){
      df_temp <- .tryNumeric(df)
      obs_initial_percent_missing <- apply(df_temp, 1, function(x) 100 * sum(is.na(x) | x =='') / length(x))
      df <- df[which(obs_initial_percent_missing < drop_row_tol), ]
      dropped_rows <- append(dropped_rows, as.vector(which(obs_initial_percent_missing >= drop_row_tol)))
    }
  }

  # Once drop process complete, or if the user does not want to drop columns or rows, move to imputation.
  if (no_impute == FALSE){

    # Save column order to reorder output prior to return
    column_order <- colnames(df)

    # Separate out data types and add impute method to missing stats for each type
    # Dates and times
    dates_times <- names(df[sapply(df, function(column) inherits(column, 'Date')) | sapply(df, function(column) inherits(column, 'POSIXct')) | sapply(df, function(column) inherits(column, 'POSIXlt'))])
    missing_stats[which(missing_stats$variable %in% dates_times), 'impute method'] <- 'None_Date Type'

    # Factors
    factors <- names(df[sapply(df, function(column) inherits(column, 'factor'))])
    # If the user wants to impute factors, replace all missing values with the most frequently occurring factor
    if (impute_factors){
      for (column in factors){
        df[(is.na(df[[column]]) | df[[column]]== ''), column] <- names(which.max(table(stats::na.omit(df[[column]]))))
      }
      missing_stats[which(missing_stats$variable %in% factors), 'impute method'] <- 'Most Frequent Factor'
    }else{
      missing_stats[which(missing_stats$variable %in% factors), 'impute method'] <- 'None_Factor'
    }

    # Numeric
    nums <- names(dplyr::select_if(df, is.numeric))

    # Strings and characters
    not_nums <- names(df[!colnames(df) %in% nums & !colnames(df) %in% dates_times & !colnames(df) %in% factors])
    missing_stats[which(missing_stats$variable %in% not_nums), 'impute method'] <- 'None_Not Numeric'

    # Retrieve names of missing_stats, this will vary depending upon user input.
    stat_column <- names(missing_stats)

    # Iterate through columns in numeric and impute according to user input. Update Missing_stats with method used.
    for (column in nums){
      if (as.numeric(missing_stats[which(missing_stats$variable == column), stat_column[length(stat_column)-1]]) > 0){

        # If user indicated they want input in the process
        if (impute_user_level == 1){
          method_invalid <- TRUE
          while (method_invalid){
            impute_method <- readline(prompt = sprintf("%s has %.2f%% missing. What impute method would you like to use? Options are: mean, median, mode, random, or zero. Do not use quotation marks. ", noquote(column), as.numeric(missing_stats[which(missing_stats$variable == column), stat_column[length(stat_column) - 1]])))
            if(impute_method != 'Exit' & impute_method !='exit' & !impute_method %in% means & !impute_method %in% medians & !impute_method %in% randos & !impute_method %in% zeros){
              cat(noquote(c(impute_method, ' is not a valid imputation method. Please try again, or type "Exit" to skip imputation.')))
            }
            else(method_invalid = FALSE)
          }
        }
        if (impute_method %in% means){
          df[is.na(df[[column]]), column] <- mean(df[[column]], na.rm = TRUE)
          missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'Mean'
        }
        if (impute_method %in% medians){
          df[is.na(df[[column]]), column] <- median(df[[column]], na.rm = TRUE)
          missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'Median'
        }
        if (impute_method %in% randos){
          df[is.na(df[[column]]), column] <- sample(stats::na.omit(df[[column]]), length(df[is.na(df[[column]]), column]))
          missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'Random Replacement'
        }
        if (impute_method %in% modes){
          df[is.na(df[[column]]), column]  <- names(which.max(table(stats::na.omit(df[[column]]))))
          missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'Mode'
        }
        if (impute_method %in% zeros){
          df[is.na(df[[column]]), column] <- 0
          missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'Zero'
        }

      }

      else{ missing_stats[which(missing_stats$variable == column), 'impute method'] <- 'No Missing Values'}

    }

  }

  # Update stats with final % missing
  end_percent_missing <- data.frame(end_percent_missing = round( sapply(df, function(column) {
    if (inherits(column, "POSIXlt") | inherits(column, "Date") |inherits(column, "POSIXct")) {
      return(100 * sum(is.na(column)) / length(column))
    } else {
      return(100 * sum(is.na(column) | column == '') / length(column))
    }
  }), 1))
  end_percent_missing$variable <- rownames(end_percent_missing)
  missing_stats <- merge(missing_stats, end_percent_missing, by = 'variable', all =TRUE, no.dups = TRUE)
  missing_stats[is.na(missing_stats)] <- 'dropped'

  return(list('df' = df[column_order], 'missing_stats' = missing_stats, 'dropped_cols'= dropped_cols, 'dropped_rows'= dropped_rows))

}


