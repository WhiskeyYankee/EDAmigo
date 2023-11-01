# Currently in Production
library(dplyr)



detectTypes <- function(df, dateForm){
  n <- nrow(df)
  cols <- character(0)
  nums <- select_if(df, is.numeric)
  not_nums <- df[!colnames(df) %in% colnames(nums)]
  for (column in names(not_nums)){
    dates <- try(as.Date(not_nums[[column]], format = dateForm), silent = TRUE)

    if (!inherits(dates, "try-error") & !all(is.na(dates))){
      cols <- c(cols, column)
      not_nums[[column]] <- as.Date(not_nums[[column]], format = dateForm)
    }
  }
  return(cbind(nums,not_nums))
}


# Test Case
df <- read.csv("C:/Users/moore/Documents/600/Project/Zillow.csv")
dateForm <- "%m/%d/%Y"
df$add1 <- rep("a", nrow(df))
df$add2 <- rnorm(nrow(df))
clean <- detectTypes(df, dateForm)
