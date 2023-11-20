#' detectMissing
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
#' out <- detectMissing(df, drop_tol = 80)
#' str(out$df)
#'
detectMissing <- function(df, drop_tol = NULL, user_level = 1 ){

  # Calculate the percentage of missing values in each column of the dataframe and save information to missing_stats
  percent_missing <- sapply(df, function(column) 100* sum(is.na(column)| column == '')/ length(column))
  missing_stats <- data.frame(column = names(df),  percent_missing = round(percent_missing, 1) )

  # If user defines a drop tolerance, drop columns which have greater than or equal value of % missing
  if (!is.null(drop_tol)){
    above_tolerance <- apply(missing_stats, 1, function(x) ifelse(x[['percent_missing']] < drop_tol, x, NA))
    df <- df[which(names(df) == above_tolerance)]
  }

  return(list('df' = df, 'missing_stats' = missing_stats))

}
