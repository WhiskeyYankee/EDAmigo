#' tryDate
#'
#' 'tryDate' attempts to coerce non-numeric columns to a date
#'
#' @param column A dataframe column to attempt to coerce to a date.
#' @param dateForm A string, or vector of strings, indicating the format of any dates contained in the dataframe.
#'
#' @noRd
#' @return The coerced date column, or the original column, if coercion is not possible.

.tryDate <- function(column, dateForm){
  original <- column

  for (datefrm in dateForm){
    if (sum(grepl("\\d", column)) == length(stats::na.omit(column))){

      dates <- try(as.Date(column, format = datefrm), silent = TRUE)

      # If the column can transform to a date, coerce to date and replace appropriate df with date
      if (!inherits(dates, "try-error") & !all(is.na(dates))){
        return(as.Date(column, format = datefrm))
      }
      else
        { return(original) }
    } else{ return(original) }

  }
}
