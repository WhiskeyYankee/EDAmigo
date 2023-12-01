#' detectMissing
#'
#' @param df
#' @param drop_tol
#' @param user_level
#'
#' @return
#' @export
#'
#' @examples
detectMissing <- function(df, drop_tol = NULL, user_level = 1 ){

  pm <- sapply(df, function(column) 100* sum(is.na(column)| column == '')/ length(column))
  missing_stats <- data.frame(column = names(df),  percent_missing = round(pm, 1) )

  if (!is.null(drop_tol)){
    df <- apply(missing_stats, 1, )

  }

  return(list('df' = df, 'missing_stats' = missing_stats))

}
