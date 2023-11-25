#' autoClean
#'
#' @param df A dataframe.
#' @param factor_tol A numeric value indicating the tolerance of unique integer values to automatically coerce a column to a categorical variable. Report in percent from 0 to 100.
#' @param user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which integer columns should be coerced to categorical, date, or retain as an integer. Report in percent from 0 to 100.
#' @param vals A gsub formatted list of characters to keep or remove.
#' @param drop_tol A percent tolerance to automatically drop columns with percent missing values greater than or equal to this value.
#' @param user_level An indicator of whether the user will provide input or if the user would like to fully automate the process, 1 indicates user interaction.
#'
#' @return A dataframe with special characters removed and column types set to the appropriate class.
#' @export
#'
#' @examples
#' cleaned <- autoClean(fires, factor_tol = 10)
autoClean <- function(df, factor_tol = NULL, user_tol = 30, vals = "[^0-9A-Za-z.,[:space:]-]", drop_tol = NULL, user_level = 0){

    # Handle special characters. Save output as individual variables to pass out.
    specialOutput <- handleSpecial(df, vals, user_level)
    clearSpecial <- specialOutput$df
    specialFound <- specialOutput$found
    specialRemoved <- specialOutput$removed

    set_types <- detectTypes(clearSpecial, factor_tol, user_tol)

    clean <- handleMissing(df, drop_tol , user_level)$df

    return_list <- list('cleanDf' = clean, 'cleared')

    return(clean)

}
