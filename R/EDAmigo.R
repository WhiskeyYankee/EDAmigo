

#' EDAmigo
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
#' @param missing_user_level An indicator of whether the user will provide input or if the user would like to fully automate the process, 1 indicates user interaction.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'  \item \code{clean_df}:  A dataframe with missing values dropped and/or imputed, according to user input.
#'  }
#' @export
#'
#' @examples
#' cleaned <- EDAmigo(fires, factor_tol = 10)
EDAmigo <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", special_user_level = 0, factor_tol = NULL, type_user_tol = 20, no_drop = FALSE,  no_impute = FALSE, drop_col_tol = 50, drop_row_tol = NULL, missing_user_level = 0){

  # Clean the dataframe before processing. Store autoCelan output.
  clean <-  autoClean(df, vals = vals, special_user_level = special_user_level , factor_tol = factor_tol, type_user_tol = type_user_tol, no_drop = no_drop,  no_impute = no_impute, drop_col_tol = drop_col_tol, drop_row_tol = drop_row_tol, missing_user_level = missing_user_level)
  clean_df <- clean$clean_df
  special_found_replaced <- clean$special_found_replaced
  type_stats <- clean$type_stats
  missing_stats <- clean$missing_stats
  dropped_cols <- clean$dropped_cols
  dropped_rows <- clean$dropped_rows



  return(clean_df)

}
