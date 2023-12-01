#' tryNumeric
#'
#' @param df A dataframe with no dates/times.
#'
#' @return numeric strings coerced to numbers.
#' @noRd
#'
#'
.tryNumeric <- function(df){

  for(column in names(df)){
    if(!any(grepl("[a-zA-Z]",df[[column]]))){
      nums <- suppressWarnings(try(as.numeric(df[[column]]), silent = TRUE))

      # If the column can transform to numeric, return numeric column. Else, return original.
      if (!inherits(nums, "try-error") & !all(is.na(nums))){
        df[[column]] <- as.numeric(df[[column]])
      }
    }
  }
  return(df)
}

