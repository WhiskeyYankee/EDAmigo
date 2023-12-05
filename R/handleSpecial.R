#' Special Character Removal
#'
#' @description
#' \strong{handleSpecial} finds and removes special characters from a dataframe. The function defaults to user interaction.
#'
#' @param df A dataframe.
#' @param vals A gsub formatted list of characters to keep or remove.
#' @param special_user_level An indicator of whether the user will provide input or if the user would like to fully automate the removal of special characters, 1 indicates user interaction.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'  \item \code{df}: A dataframe with special characters removed, according to user input.
#'  \item \code{found_replaced}: A dataframe providing details about the special characters found in each column and which characters were removed according to user input.
#'}
#'
#' @export
#'
#' @examples
#' # Store function output for future use
#' out <- handleSpecial(finance,special_user_level = 0)
#'
#' # Examine the characters that were found vs. replaced
#' out$found_replaced
#'
handleSpecial <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", special_user_level = 1){

  special_found_replaced <- data.frame(special_found = character(), special_replaced = character())
  handle <- 0

  dates_times <- names(df[sapply(df, function(column) inherits(column, 'Date')) | sapply(df, function(column) inherits(column, 'POSIXct')) | sapply(df, function(column) inherits(column, 'POSIXlt'))])
  to_handle <- names(df[!colnames(df) %in% dates_times])
  for (column in to_handle){

    if( special_user_level == 1){

        # If the dataframe column contains special characters
      if (sum(grepl(vals, df[[column]])) > 0){
        special <- unique(unlist(regmatches(df[[column]], gregexpr(vals,df[[column]]))))
        special_found_replaced[column, 'special_found'] <- paste(special, collapse = " ")
        handle <- utils::menu(c("Remove special",  "Choose which to remove", "Keep special"), title = paste(column, "contains the following special characters: ", paste(special, collapse = " "),"  How would you like to proceed? "))
      }

      if (handle == 1){ # Remove all listed special characters
        df[[column]] <- gsub(vals,'', df[[column]])
        special_found_replaced[column, 'special_replaced'] <-  paste(special, collapse = " ")
        handle <- 0
      }

      if (handle == 2){ # Receive user input on which characters to remove
        val2 <- readline(prompt ='Please input the characters you would like to remove in the format of gsub, EX: "[$%#@!{}]"  ')
        df[[column]] <- gsub(val2,'', df[[column]])
        special_found_replaced[column, 'special_replaced'] <-  paste(special, collapse = " ")
        handle <- 0
      }


    }
    else { # If user indicated they do not want input, remove all special characters.
      special <- unique(unlist(regmatches(df[[column]], gregexpr(vals,df[[column]]))))
      special_found_replaced[column, 'special_found'] <-  paste(special, collapse = " ")
      special_found_replaced[column, 'special_replaced'] <-  paste(special, collapse = " ")
      df[[column]] <- gsub(vals,'', df[[column]])
    }

  }

  return(list('df' = df, 'found_replaced' = special_found_replaced))
}
