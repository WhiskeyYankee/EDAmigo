#' autoClean
#'
#' @description
#''autoClean' takes a dataframe of mixed classes and removes special characters, coerces columns to an appropriate class, and handles missing values. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class. The function defaults to minimal (possibly not none) user interaction.
#'
#'
#'
#' @param df A dataframe with any combination of variable classes, dates must be properly classified as dates.
#' @param vals A gsub formatted list of characters to keep or remove.
#' @param special_user_level An indicator of whether the user will provide input or if the user would like to fully automate the removal of special characters, 1 indicates user interaction.
#' @param factor_tol A numeric value indicating the tolerance to automatically coerce a column to a factor. Report in percent of unique column values from 0 to 100.
#' @param type_user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which columns should be coerced to another class. Report in percent of unique column values from 0 to 100.
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
#'  \item \code{clean_df}: A dataframe with special characters removed, columns coerced to appropriate classes, and missing values handled according to user input.
#'  \item \code{special_found_replaced}: A dataframe providing details about the special characters found in each column and which characters were removed according to user input.
#'  \item \code{type_stats}: A dataframe providing details about which columns were coerced to another class. Includes the percent unique values per column.
#'  \item \code{missing_stats}: A dataframe providing details about which columns were dropped and what percent missing values were identified throughout several steps, according to user input. If imputation is selected, this will include the method of imputation performed on each column.
#'  \item \code{dropped_cols}: A list of column names that were dropped from the original dataframe.
#'  \item \code{dropped_rows}: A list of row indices that were dropped from the original dataframe.
#' }
#'
#' @export
#'
#' @examples
#' cleaned <- autoClean(fires, factor_tol = 10, drop_user_level = 0, impute_user_level = 0)
autoClean <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", special_user_level = 0, factor_tol = NULL, type_user_tol = 20, no_drop = FALSE,  no_impute = FALSE, drop_col_tol = 50, drop_row_tol = NULL,  drop_user_level = 1, impute_user_level = 1,  impute_method = 'median', impute_factors = FALSE){

  # Handle special characters. Save output as individual variables to pass out.
  specialOutput <- handleSpecial(df, vals, special_user_level)
  clearSpecial <- specialOutput$df
  special_found_replaced <- specialOutput$found_replaced

  # Coerce column types to proper class.
  set_types <- detectTypes(clearSpecial, factor_tol, type_user_tol)
  dates_times <- set_types$date_times
  numbers <- set_types$numbers
  characters <- set_types$characters
  factors <- set_types$factors
  typed_df <- set_types$df
  typeStats <- set_types$type_stats

  # Handle missing values, drop or impute.
  clean <- handleMissing(df = typed_df, no_drop = no_drop, no_impute = no_impute, drop_col_tol = drop_col_tol, drop_row_tol = drop_row_tol, drop_user_level = drop_user_level, impute_user_level = impute_user_level, impute_factors = impute_factors)
  missingStats <- clean$missing_stats
  dropped_cols <- clean$dropped_cols
  dropped_rows <- clean$dropped_rows
  cleanDf <- clean$df

  # Return all
  return_list <- list('clean_df' = cleanDf, 'special_found_replaced' = special_found_replaced, 'type_stats'= typeStats, 'missing_stats'= missingStats, 'dropped_cols'= dropped_cols, 'dropped_rows'= dropped_rows)

  return(return_list)

}
