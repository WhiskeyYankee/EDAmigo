#' handleMissing
#'
#' @param df A dataframe.
#' @param drop_tol A percent tolerance to automatically drop columns with percent missing values greater than or equal to this value.
#' @param user_level An indicator of whether the user will provide input or if the user would like to fully automate the process, 1 indicates user interaction.
#'
#' @return The dataframe with dropped or imputated missing values.
#' @export
#'
#' @examples
#' # Define the dataframe, clear of special characters
#' df <- handleSpecial(fires,user_level =0)$df
#'
#' # Store function results
#' out <- handleMissing(df, drop_col_tol = 80, user_level = 0)
#' str(out$df)
#'
handleMissing <- function(df, drop_col_tol = NULL, drop_row_tol = NULL, user_level = 1){

  # Check to make sure user has supplied enough parameters to execute the function. Must specifiy at least one drop tolerance, or utilize user interaction.
  if(is.null(drop_col_tol) & is.null(drop_row_tol) & user_level == 0) stop("Please supply at least one tolerance, or set user_level to 1.")

  # Calculate the percentage of missing values in each column of the dataframe and save information to missing_stats
  # Perform separate calculation on date/time columns
  percent_missing <- sapply(df, function(column) {
    if (inherits(column, "POSIXlt") | inherits(column, "Date") |inherits(column, "POSIXct")) {
      return(100 * sum(is.na(column)) / length(column))
    } else {
      return(100 * sum(is.na(column) | column == '') / length(column))
    }
  })

  missing_stats <- data.frame(percent_missing = round(percent_missing, 1) )

  dropped_cols <-c()
  dropped_rows <- c()

  # If user defines a column drop tolerance, drop columns which have greater than or equal value of % missing
  if (!is.null(drop_col_tol)){
    below_tolerance <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] < drop_col_tol, x, NA))
    df <- df[which(names(df) %in% names(stats::na.omit(below_tolerance)))]
    dropped_cols <- append(dropped_cols, as.vector(names(below_tolerance[is.na(below_tolerance)])))
  }

  # If user defines a row drop tolerance, drop rows which have greater than or equal value of % missing
  if (!is.null(drop_row_tol)){
    df_temp <- .tryNumeric(df)
    obs_percent_missing <- apply(df_temp, 1, function(x) 100 * sum(is.na(x) | x =='') / length(x))
    df <- df[which(obs_percent_missing < drop_row_tol), ]
    dropped_rows <- append(dropped_rows, as.vector(which(obs_percent_missing >= drop_row_tol)))
  }



  if (user_level == 1){
    print(missing_stats)
    cat('\n')
    input <- utils::menu(c("Drop all missing", "Select which to drop", "Set a drop tolerance", "See missing by row"),title = "Above are the percent missing by column for your dataframe. Which would you like to do? ")

    if (input == 1){
      kept <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] > 0, NA, x))
      df <- df[which(names(df) %in% names(stats::na.omit(kept)))]
      dropped_cols <- append(dropped_cols, as.vector(names(kept[is.na(kept)])))
    }

    if (input == 2){
      options <- sprintf("%-20s %-10s",names(df), round(percent_missing,1))
      to_drop <- coda::multi.menu(options, title = "Which would you like to drop?")

      if(to_drop != 0){
        drop_names <- names(df)[to_drop]
        df <- df[which(!names(df) %in% drop_names)]
        dropped_cols <- append(dropped_cols, drop_names)
      }
    }

    if (input == 3){
      tol <- readline(prompt = "What tolerance would you like to set? ")
      kept <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] >= tol, NA, x))
      df <- df[which(names(df) %in% names(stats::na.omit(kept)))]
      dropped_cols <- append(dropped_cols, as.vector(names(kept[is.na(kept)])))
    }


    # If the user wants to view percent missing by observation (row)
    if (input == 4){

      # Coerce dates to numeric to prevent errors and calculate % missing by row
      df_temp <- .tryNumeric(df)
      obs_percent_missing <- apply(df_temp, 1, function(x) 100 * sum(is.na(x) | x =='') / length(x))

      # Output a histogram of percent missing
      par(mfrow=c(1,1))
      hist(obs_percent_missing, main = "Histogram of percent missing by rows", xlab ="Percent missing")

      # Get user feedback on how they would like to progress.
      row_input <- menu(c("Set row drop tolerance", "Exit Function, I'm done!"), title="Please view the histogram of observation percent missing (by row). What would you like to do now? ")

      # If user would like to set a drop tolerance for rows, take tolerance, remove rows and update dropped_rows
      if (row_input == 1){
        row_tol <- readline(prompt = "What tolerance would you like to set? ")
        df <- df[which(obs_percent_missing < row_tol), ]
        dropped_rows <- append(dropped_rows,as.vector(which(obs_percent_missing >= row_tol)))
      }

    }

  }

  return(list('df' = df, 'missing_stats' = missing_stats, 'dropped_cols'= dropped_cols, 'dropped_rows'= dropped_rows))

}


