# Currently in Production
library(dplyr)


detectTypes <- function(df, dateForm){
  n <- nrow(df)
  cols <- character(0)
  nums <- select_if(df, is.numeric)
  not_nums <- df[!colnames(df) %in% colnames(nums)]

  # iterate through columns that are not numbers and try to transform to a date
  for (column in names(not_nums)){
    dates <- try(as.Date(not_nums[[column]], format = dateForm), silent = TRUE)

    # if the column can transform to a date, coerce to date and replace appropriate not_nums with date
    if (!inherits(dates, "try-error") & !all(is.na(dates))){
      cols <- c(cols, column)
      not_nums[[column]] <- as.Date(not_nums[[column]], format = dateForm)
    }
  }

  # iterate through columns that are numbers to identify integers and determine if they are categorical
  for (column in names(nums)){
    if (class(df[[column]]) == 'integer'){
      unique_ints <- length(as.vector(unique(df[[column]])))
      percent_unique <- 100 * unique_ints / length(na.omit(df[[column]]))

      if (percent_unique <= 50){ # [TODO] implement a user provided tolerance to automatically mark cat variables?
        is_cat <- menu(c('Categorical','Date', 'Keep as an Integer!'), title = cat(column, ' contains ', 100 * unique_ints / n ,'% unique values. Is this:'))
      }
      if (is_cat == 1){
        nums[[column]] <- as.character(df[[column]])
      }
      if (is_cat == 2){
        date_format <- readline(prompt ='What is the format of this date? Example: for day of year, provide "%j"')
        dates <- try(as.Date(nums[[column]], format = date_format), silent = TRUE)

        # if the column can transform to a date, coerce to date and replace appropriate not_nums with date
        if (!inherits(dates, "try-error") & !all(is.na(dates))){
          nums[[column]] <- as.Date(nums[[column]], format = date_format)
        }
        else { cat("The provided format did not work! Double check your input.")}
      }

    }
  }
  # return transformed data
  return(cbind(nums,not_nums))
}


# Test Case
df <- read.csv("C:/Users/moore/Documents/600/Project/fires.csv")
dateForm <- "%m/%d/%Y"
clean <- detectTypes(df, dateForm)
