#' tryDate attempts to coerce non-numeric columns to a date
#'
#' @param not_nums
#' @param column
#' @param dateForm
#'
#' @return
#' @export
#'
#' @examples
tryDate <- function(not_nums, column, dateForm){
  for (datefrm in dateForm){


    dates <- try(as.Date(not_nums[[column]], format = datefrm), silent = TRUE)

    # If the column can transform to a date, coerce to date and replace appropriate not_nums with date
    if (!inherits(dates, "try-error") & !all(is.na(dates))){
      return(as.Date(not_nums[[column]], format = datefrm))
    }
    else
      { return(not_nums[[column]]) }
  }
}
