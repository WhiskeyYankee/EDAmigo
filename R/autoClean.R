#' autoClean
#'
#' @param df A dataframe.
#' @param dateForm A string, or vector of strings, indicating the format of any dates contained in the dataframe.
#' @param cat_tol A numeric value indicating the tolerance of unique integer values to automatically coerce a column to a categorical variable. Report in percent from 0 to 100.
#' @param user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which integer columns should be coerced to categorical, date, or retain as an integer. Report in percent from 0 to 100.
#' @param vals A gsub formatted list of characters to keep or remove.
#' @param user_level An indicator of whether the user will provide input or if the user would like to fully automate the process, 1 indicates user interaction.
#'
#' @return A dataframe with special characters removed and column types set to the appropriate class.
#' @export
#'
#' @examples
autoClean <- function(df, dateForm = "%m/%d/%Y", cat_tol = NULL, user_tol = 80, vals = "[^0-9A-Za-z.,[:space:]-]", drop_tol = NULL, user_level = 0){

    clearSpecial <- handleSpecial(df, vals, user_level)

    set_types <- detectTypes(clearSpecial, dateForm, cat_tol, user_tol)

    clean <- detectMissing(df, drop_tol , user_level)

    return(clean)

}
