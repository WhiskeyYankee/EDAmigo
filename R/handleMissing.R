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
#' df <- fires
#' out <- handleMissing(df, drop_tol = 80, user_level = 0)
#' str(out$df)
#'
handleMissing <- function(df, drop_tol = NULL, user_level = 1 , drop_row = TRUE, drop_col = FALSE){

  # Calculate the percentage of missing values in each column of the dataframe and save information to missing_stats
  percent_missing <- sapply(df, function(column) 100* sum(is.na(column)| column == '')/ length(column))
  missing_stats <- data.frame(  percent_missing = round(percent_missing, 1) )

  # If user defines a drop tolerance, drop columns which have greater than or equal value of % missing
  if (!is.null(drop_tol)){
    below_tolerance <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] < drop_tol, x, NA))
    df <- df[which(names(df) %in% names(stats::na.omit(below_tolerance)))]
  }

  if (user_level == 1){
    print(missing_stats)
    cat('\n')
    input <- utils::menu(c("Drop all missing", "Select which to drop", "Set a drop tolerance"),title = "Above are the percent missing for your dataframe. Which would you like to do? ")

    if (input == 1){
      kept <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] > 0, NA, x))
      df <- df[which(names(df) %in% names(stats::na.omit(kept)))]
    }

    if (input == 2){
      options <-'a'
      to_drop <- coda::multi.menu(options, title = "Which would you like to drop?")

    }

    if (input == 3){
      tol <- readline(prompt = "What tolerance would you like to set? ")
      kept <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] >= tol, NA, x))
      df <- df[which(names(df) %in% names(stats::na.omit(kept)))]
    }

  }

  return(list('df' = df, 'missing_stats' = missing_stats))

}
