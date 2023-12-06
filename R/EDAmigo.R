

#' Automated Exploratory Data Analysis
#'
#' \strong{EDAmigo} automatically cleans, processes, transforms, and visualizes user data. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class. The function defaults to minimal user interaction.
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
#' @param impute_method A string indicating the method of imputation. If no_impute is set to TRUE, this is ignored. Options are: median, mean, mode, zero, and random.
#' @param impute_factors A boolean indicating whether or not to impute factor columns. If set to TRUE, the value occurring most frequently is applied to missing values.
#' @param lambda Numeric value(s) indicating what power(s) to use in the Box Cox transformation. If not provided, boxCox will use -3 , -2.99, ... , 2.99, 3
#' @param cols A vector indicating the column numbers or the names of the columns one wishes to evaluate. If NULL then all numeric columns will be evalutated.
#' @param alpha A numeric value used to determine the shift parameter when 0s and or negative values are detected in the data. In the case of the default 0.001, if the data contains 0s but no negative values, then the shift parameter is set to 0.001.
#' @param FILTER A Boolean value that determines if the output will filter out transformations that don't improve the adherence to normality and reduce outliers. The default is TRUE but user may wish to set it to false to either see why a value is not suggested or to perform a desired transformation.
#' @param interactive_view Boolean TRUE/FALSE to indicate if the user wants to interactively walk through the plots. If set to FALSE, the function outputs a list of plots.
#' @param n_top The number of variable comparisons to plot.
#' @param line_color The regression line color.
#'
#' @export
#'
#' @return
#'A list with the following elements:
#' \itemize{
#'  \item \code{clean_df}: A dataframe with special characters removed, columns coerced to appropriate classes, and missing values handled according to user input.
#'  \item \code{special_found_replaced}: A dataframe providing details about the special characters found in each column and which characters were removed according to user input.
#'  \item \code{type_stats}: A dataframe providing details about which columns were coerced to another class. Includes the percent unique values per column.
#'  \item \code{missing_stats}: A dataframe providing details about which columns were dropped and what percent missing values were identified throughout several steps, according to user input. If imputation is selected, this will include the method of imputation performed on each column.
#'  \item \code{dropped_cols}: A list of column names that were dropped from the original dataframe.
#'  \item \code{dropped_rows}: A list of row indices that were dropped from the original dataframe.
#'  \item \code{boxCox_Results}: A dataframe containing the estimates for each column evaluated by the function
#'  \item \code{lambda_1}: A vector containing all of the lambdas evaluated by the function.
#'  \item \code{log_Like}: A dataframe containing the log-liklihood calculations for each lambda.
#'  \item \code{transformations}: A dataframe of transformed values.
#'  \item \code{tranform_plots}: distribution plots of the data pre and post transformation to help users select appropriate transformations for their data.
#'  \item \code{amigo_plots}: A dataframe containing the correlation, slope, and R^2 values for the highest correlated  variables.
#' }
#'
#'
#' @examples
#'
#' Amigo_results <- EDAmigo(fires)
#'
EDAmigo <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", special_user_level = 0,
                    factor_tol = 10, type_user_tol = 10, no_drop = FALSE,
                    no_impute = FALSE, drop_col_tol = 50, drop_row_tol = NULL,
                    drop_user_level = 0, impute_user_level = 0,
                    impute_method = 'median', impute_factors = TRUE,
                    lambda = NULL, cols = NULL, alpha = 0.001, FILTER = TRUE,
                    interactive_view = FALSE, n_top = 4, line_color = 'dodgerblue'){

  cleaned <- autoClean(df, factor_tol = factor_tol, drop_user_level = drop_user_level,
                       impute_user_level = impute_user_level, impute_factors = impute_factors)

  test_results <- boxCox(cleaned$clean_df, lambda = lambda, cols = cols, alpha = alpha, FILTER = FILTER)

  transform_plots <- boxCox_Vis(cleaned$clean_df, test_results, interactive_view = interactive_view)


  auto_plots <- amigoPlot(cleaned$clean_df, n_top = n_top, line_color = line_color)

  # Return all
  return_list <- list('clean_df' = cleaned$cleanDf, 'special_found_replaced' = cleaned$special_found_replaced,
                      'type_stats'= cleaned$typeStats, 'missing_stats'= cleaned$missingStats,
                      'dropped_cols'= cleaned$dropped_cols, 'dropped_rows'= cleaned$dropped_rows,
                      'boxCox_Results' = test_results$boxCox_Results, 'lambda_1' = test_results$lambda,
                      'log_Like' = test_results$log_Like, 'transformations' = test_results$transformations,
                      'transform_plots'= transform_plots, 'amigo_plots'= auto_plots
                      )

}

