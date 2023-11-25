#' @title detectTypes
#'
#' @description 'detectTypes' takes a dataframe and coerces columns to the appropriate class. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class.
#'
#' @param df A dataframe with any combination of variable classes, dates must be properly classified as dates.
#' @param factor_tol A numeric value indicating the tolerance to automatically coerce a column to a factor. Report in percent from 0 to 100.
#' @param user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which columns should be coerced to another class. Report in percent from 0 to 100.
#'
#' @return 'df'= The dataframe with all variables coerced, 'date_times' = A dataframe with only the date/time columns, 'numbers' = A dataframe with only the numeric columns,
#'  'characters' = A dataframe with only char columns, 'factors' = A dataframe with only the factor columns, 'typeStats' = A dataframe listing the percentage of unique values in each column)
#'
#' @export
#'
#' @examples
#' # Define the dataframe to be properly classified.
#' df <- fires
#'
#' # View classes of the variables in the dataframe
#' str(df)
#'
#' # Clean the dataframe using a tolerance of 10%
#' clean <- detectTypes(df, factor_tol = 10, user_tol = 10)
#' str(clean$df)
#'
#' # View information about the type coercions
#' clean$type_stats
detectTypes <- function(df, factor_tol = NULL, user_tol = 30){

  n <- nrow(df)
  column_order <- names(df)

  # Separate out numeric columns
  nums <- dplyr::select_if(df, is.numeric)

  # Separate out date columns
  dates_times <- df[sapply(df, function(column) inherits(column, 'Date')) | sapply(df, function(column) inherits(column, 'POSIXct')) | sapply(df, function(column) inherits(column, 'POSIXlt'))]

  # Separate out non-numeric columns
  not_nums <- df[!colnames(df) %in% colnames(nums) & !colnames(df) %in% colnames(dates_times)]

  # Define empty dataframe for factors
  factors <- data.frame(rep(NA, n))

  # Find percent unique for each column
  typeStats <- data.frame(sapply(df, function(column) (100 * (length(as.vector(stats::na.omit(unique(column)))) / length(stats::na.omit(column))))))
  colnames(typeStats) <- 'percent_unique'
  typeStats$original_class <- sapply(df, function(column) class(column))

  # If user supplies factor_tol, coerce applicable columns to factor
  if (!is.null(factor_tol)){


    for (column in names(not_nums)){
      # If the column has less % unique values than the factor_tol, coerce to a factor
      # Remove the column from not_nums and add to factors dataframe
      if (typeStats[column, 'percent_unique'] <= factor_tol){
        factors[[column]] <- as.factor(df[[column]])
        not_nums <- not_nums[, !(names(not_nums) %in% column)]
      }
    }


    for (column in names(nums)){
      # If the column has less % unique values than the factor_tol, coerce to a factor
      # Remove the column from nums and add to factors dataframe
      if (typeStats[column, 'percent_unique'] <= factor_tol){
        factors[[column]] <- as.factor(df[[column]])
        nums <- nums[, !(names(nums) %in% column)]
      }
    }
  } # End factor_tol coercion



  # Iterate through non-numeric columns to request user input for factor coercion, using user_tol %
  for (column in names(not_nums)){

    to_type = 0

    if(typeStats[column, 'percent_unique'] <= user_tol){
      to_type <- utils::menu(c('Factor', 'Keep as a string!'), title = cat(column, ' contains ', typeStats[column, 'percent_unique'],'% unique values. Is this:'))
    }

    if (to_type == 1){ # Set column to factor
      factors[[column]] <- as.factor(df[[column]])
      not_nums <- not_nums[, !(names(not_nums) %in% column)]
    }

  }



  # Iterate through non-numeric columns to request user input for integer coercion to char, factor, using user_tol %
  for (column in names(nums)){

    if (sum(df[[column]] %% 1, na.rm= TRUE) == 0){ # Identify any columns of all integers

      to_type = 0

      # If column % unique is less than user_tol, request user input for coercion
      if (typeStats[column, 'percent_unique'] <= user_tol){
        to_type <- utils::menu(c('Character','Factor', 'Keep as an Integer!'), title = cat(column, ' contains ', typeStats[column, 'percent_unique'],'% unique values. Is this:'))
      }

      if (to_type == 1){ # Set column to character type
        not_nums[[column]] <- as.character(df[[column]])
        nums <- nums[, !(names(nums) %in% column)]
      }

      if (to_type == 2){ # Set column to factor
        factors[[column]] <- as.factor(df[[column]])
        nums <- nums[, !(names(nums) %in% column)]
       }

    } # End if sum
  } # End for column


  # Drop the column of NA placeholders from factors
  factors <- factors[,-1]

  # List the new class for each column
  typeStats$new_class <- sapply(cbind(nums, not_nums, dates_times, factors)[column_order], function(column) class(column))

  # Define the return list of dataframes for output
  return_list <- list('df'= cbind(nums, not_nums, dates_times, factors)[column_order], 'date_times' = names(dates_times), 'numbers' = names(nums), 'characters' = names(not_nums), 'factors' = names(factors), 'type_stats' = typeStats)
  # set to column names

  return(return_list)
}

