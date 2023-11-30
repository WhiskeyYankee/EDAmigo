

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
#' @param drop_user_level An indicator of whether the user will provide input or if the user would like to fully automate the drop process, 1 indicates user interaction.
#' @param impute_user_level An indicator of whether the user will provide input or if the user would like to fully automate the imputation process, 1 indicates user interaction.
#' @param impute_method A string indicating the method of imputation. If no_impute is set to TRUE, this is ignored.
#' @param impute_factors A boolean indicating whether or not to impute factor columns. If set to TRUE, the value occurring most frequently is applied to missing values.
#'
#' @return
#' #' A list with the following elements:
#' \itemize{
#'  \item \code{df}:  A dataframe with missing values dropped and/or imputed, according to user input.
#' }
#' @export
#'
#' @examples
#' result <- EDAmigo(f)
EDAmigo <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", special_user_level = 0, factor_tol = NULL, type_user_tol = 20, no_drop = FALSE,  no_impute = FALSE, drop_col_tol = 50, drop_row_tol = NULL, drop_user_level = 0, impute_user_level = 0 ){

  list(cleanDf, special_found_replaced, typeStats,  missingStats, dropped_cols, dropped_rows) <-  autoClean(df, vals = vals, special_user_level = special_user_level , factor_tol = factor_tol, type_user_tol = type_user_tol, no_drop = no_drop,  no_impute = no_impute, drop_col_tol = drop_col_tol, drop_row_tol = drop_row_tol, drop_user_level = drop_user_level, impute_user_level =  impute_user_level)


}