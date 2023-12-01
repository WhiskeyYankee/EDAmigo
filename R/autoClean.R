#' autoClean
#'
#' @param df
#' @param dateForm
#' @param cat_tol
#' @param user_tol
#' @param vals
#' @param user_level
#'
#' @return
#' @export
#'
#' @examples
autoClean <- function(df, dateForm = "%m/%d/%Y", cat_tol = NULL, user_tol = 80, vals = "[^0-9A-Za-z.,[:space:]-]", drop_tol = NULL, user_level = 0){

    clearSpecial <- handleSpecial(df, vals, user_level)

    set_types <- detectTypes(clearSpecial, dateForm, cat_tol, user_tol)

    clean <- detectMissing(df, drop_tol , user_level)

    return(clean)

}
