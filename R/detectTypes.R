#' @title detectTypes
#'
#' @description
#' 'detectTypes' takes a dataframe and coerces columns to the appropriate class. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class.
#'
#' @param df A dataframe with any combination of variable classes, dates must be properly classified as dates.
#' @param factor_tol A numeric value indicating the tolerance to automatically coerce a column to a factor. Report in percent of unique column values from 0 to 100.
#' @param type_user_tol A numeric value indicating the tolerance to receive user input prompts to assist in selecting which columns should be coerced to another class. Report in percent of unique column values from 0 to 100.
#'
#' @return
#' ' A list with the following elements:
#' \itemize{
#'  \item \code{df}: A dataframe with columsn coerced to an appropriate class, according to user input.
#'  \item \code{date_times}: A list of final column names that are dates or times.
#'  \item \code{numbers}: A list of final column names that are numeric.
#'  \item \code{characters}: A list of final column names that are character/string.
#'  \item \code{factors}: A list of final column names that are factors.
#'  \item \code{type_stats}: A dataframe providing details about which columns were coerced to another class. Includes the percent unique values per column.
#' }
#' 'df'= The dataframe with all variables coerced, 'date_times' = A dataframe with only the date/time columns, 'numbers' = A dataframe with only the numeric columns,
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
#' clean <- detectTypes(df, factor_tol = 10, type_user_tol = 10)
#' str(clean$df)
#'
#' # View information about the type coercions
#' clean$type_stats
detectTypes <- function(df, factor_tol = NULL, type_user_tol = 20){

  n <- nrow(df)
  column_order <- names(df)

  # Separate out date columns
  dates_times <- df[sapply(df, function(column) inherits(column, 'Date')) | sapply(df, function(column) inherits(column, 'POSIXct')) | sapply(df, function(column) inherits(column, 'POSIXlt'))]

  # Define empty dataframe for factors
  factors <- df[sapply(df, function(column) inherits(column, 'factor'))]

  # Separate out numeric columns and coerce numeric strings to numeric
  nums <- dplyr::select_if(df, is.numeric)
  coerce_to_num <-  suppressWarnings(.tryNumeric(df[!colnames(df) %in% colnames(dates_times) & !colnames(df) %in% colnames(nums) & !colnames(df) %in% colnames(factors)]))
  nums <- cbind(nums,dplyr::select_if(coerce_to_num, is.numeric))


  # Separate out non-numeric columns
  not_nums <- df[!colnames(df) %in% colnames(nums) & !colnames(df) %in% colnames(dates_times) & !colnames(df) %in% colnames(factors)]



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

  count_to_coerce <- sum(typeStats[, 'percent_unique'] <= type_user_tol)
  counter <- 1
  counter_non_num <- sum(row.names(typeStats[typeStats[, 'percent_unique'] <= type_user_tol, ] ) %in% names(not_nums))
  # Iterate through non-numeric columns to request user input for factor coercion, using type_user_tol %
  for (column in names(not_nums)){

    to_type = 0

    if(typeStats[column, 'percent_unique'] <= type_user_tol){
      cat(counter, ' of ', count_to_coerce, ' columns to coerce.\n \n' )
      print(DescTools::Desc(df[[column]], main = column))
      to_type <- utils::menu(c('Factor', 'Keep as a string', 'Hey! That is actually numeric!', 'Skip to integer analysis'), title = cat( column, ' contains ', typeStats[column, 'percent_unique'],'% unique values. Is this:'))
      counter <- counter + 1
    }

    if (to_type == 1){ # Set column to factor
      factors[[column]] <- as.factor(df[[column]])
      not_nums <- not_nums[, !(names(not_nums) %in% column)]
    }
    if (to_type == 3){ # Set column to numeric
      nums[[column]] <- as.numeric(df[[column]])
      not_nums <- not_nums[, !(names(not_nums) %in% column)]
    }
    if (to_type == 4){
      counter <- counter_non_num + 1
        break}

  }



  # Iterate through numeric columns to request user input for integer coercion to char, factor, using type_user_tol %
  for (column in names(nums)){

    if (sum(nums[[column]] %% 1, na.rm= TRUE) == 0){ # Identify any columns of all integers

      to_type = 0

      # If column % unique is less than type_user_tol, request user input for coercion
      if (typeStats[column, 'percent_unique'] <= type_user_tol){
        cat(counter, ' of ', count_to_coerce, ' columns to coerce.\n \n' )
        print(DescTools::Desc(df[[column]], main = column))
        to_type <- utils::menu(c('Character','Factor', 'Float', 'Keep as an Integer!','Exit'), title = cat(column, ' contains ', typeStats[column, 'percent_unique'],'% unique values. Is this:'))
        counter <- counter + 1
      }

      if (to_type == 1){ # Set column to character type
        not_nums[[column]] <- as.character(nums[[column]])
        nums <- nums[, !(names(nums) %in% column)]
      }

      if (to_type == 2){ # Set column to factor
        factors[[column]] <- as.factor(nums[[column]])
        nums <- nums[, !(names(nums) %in% column)]
      }

      if (to_type == 3){ # Set column to float
        nums[[column]] <- as.numeric(nums[[column]])
      }

      if (to_type == 4){ # Set column to int
        nums[[column]] <- as.integer(nums[[column]])
      }
      if (to_type == 5){break}

    } # End if sum
  } # End for column



  # List the new class for each column
  typeStats$new_class <- sapply(cbind(nums, not_nums, dates_times, factors)[column_order], function(column) class(column))

  # Define the return list of dataframes for output
  return_list <- list('df'= cbind(nums, not_nums, dates_times, factors)[column_order], 'date_times' = names(dates_times), 'numbers' = names(nums), 'characters' = names(not_nums), 'factors' = names(factors), 'type_stats' = typeStats)
  # set to column names

  return(return_list)
}

