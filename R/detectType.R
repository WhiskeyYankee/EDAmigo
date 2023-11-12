#' @title detectTypes
#'
#' @description 'detectTypes' takes a raw dataframe and coerces variables to the appropriate class.
#'
#' @param df A dataframe with any combination of variable classes.
#' @param dateForm A string, or vector of strings, indicating the format of any dates contained in the dataframe.
#' @param cat_tol A numeric value indicating the tolerance of unique integer values to automatically coerce a column to a categorical variable. Report in percent from 0 to 100.
#' @param user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which integer columns should be coerced to categorical, date, or retain as an integer. Report in percent from 0 to 100.
#'
#' @return The dataframe with columns coerced to the appropriate classes.
#'
#' @export
#'
#' @examples
#' # Define a random data frame with dates and categorical integers that are not properly classified
#' df <- data.frame(matrix(rnorm(20), nrow = 10))
#' df$Date <- rep('12/01/2014', 10)
#' df$DOY <- rep('123', 10)
#' df$int2cat <- rep(c(1,2,3,4,5), 2)
#'
#' # View classes of the variables in the dataframe
#' str(df)
#'
#' # Define the format of the dates in the dataframe
#' dateForm <- c('%j', "%m/%d/%Y")
#'
#' # Clean the dataframe using a tolerance of 1%
#' clean <- detectTypes(df, dateForm, cat_tol = 1)
#' str(clean)
#'
detectTypes <- function(df, dateForm = "%m/%d/%Y", cat_tol = NULL, user_tol = 80){
  n <- nrow(df)
  nums <- dplyr::select_if(df, is.numeric)
  not_nums <- df[!colnames(df) %in% colnames(nums)]

  # Iterate through columns that are not numbers and try to transform to a date
  for (column in names(not_nums)){
    not_nums[[column]] <- EDAmigo:::.tryDate(not_nums[[column]], dateForm)
  }

  # Iterate through columns that are numbers to identify integers and determine if they are true integers
  for (column in names(nums)){
    if (sum(df[[column]] %% 1, na.rm= TRUE) == 0){ # Identify any columns of all integers
      unique_ints <- length(as.vector(unique(df[[column]])))
      percent_unique <- 100 * (unique_ints / length(na.omit(df[[column]])))
      is_cat = 0

      if (!is.null(cat_tol) ) {
        if (percent_unique <= cat_tol){
          nums[[column]] <- as.character(df[[column]])
        }
      }
      if (class(nums[[column]]) != "character"){
        if (percent_unique <= user_tol){
          is_cat <- menu(c('Categorical','Date', 'Keep as an Integer!'), title = cat(column, ' contains ', percent_unique,'% unique values. Is this:'))
        }

        if (is_cat == 1){ # Set column to character type
          nums[[column]] <- as.character(df[[column]])
        }

        if (is_cat == 2){ # Set column to Date type, using user specified format
          dates = EDAmigo:::.tryDate(not_nums[[column]], dateForm)
          # if the column can transform to a date, coerce to date and replace appropriate nums with date
          if (class(dates)== 'Date'){
            nums[[column]] <- dates
          }
          else{
            halt = FALSE
              while (halt == FALSE){
                date_format <- readline(prompt ='This date does not match any of your provided date formats. What is the format of this date? Example: for day of year, provide "%j" ')
                dates <- EDAmigo:::.tryDate(nums[[column]], date_format)
                if (class(dates) == 'Date'){
                  halt = TRUE
                  nums[[column]] <- dates
                }
                else (
                  halt <- menu(c("Try another date format", "Keep as numeric"), title = "That date format did not work! Would you like to: ") - 1
                )
              }

          }

         }
      }
    }
  }

  # return transformed data
  return(cbind(nums,not_nums))
}

