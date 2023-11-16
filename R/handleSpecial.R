#' handleSpecial
#'
#' @param df A dataframe.
#' @param vals A gsub formatted list of characters to keep or remove.
#' @param user_level An indicator of whether the user will provide input or if the user would like to fully automate the removal of special characters.
#'
#' @return The dataframe without special characters, a dataframe containing the list of all found special characters, and a dataframe with a list of all removed special characters.
#' @export
#'
#' @examples
#' out <- handleSpecial(finance, user_level = 0)
#' out[[2]]
#'
handleSpecial <- function(df, vals = "[^0-9A-Za-z.,[:space:]-]", user_level = 1){

  found <- data.frame(column = character(), special = character())
  replaced <- data.frame(column = character(), special = character())
  handle <- 0

  for (column in names(df)){

    if( user_level == 1){

        # If the dataframe column contains special characters
      if (sum(grepl(vals, df[[column]])) > 0){
        special <- unique(unlist(regmatches(df[[column]], gregexpr(vals,df[[column]]))))
        found <- rbind(found, data.frame(column = column, special = paste(special, collapse = " ")))
        handle <- utils::menu(c("Remove special",  "Choose which to remove", "Keep special"), title = paste(column, "contains the following special characters: ", paste(special, collapse = " "),"  How would you like to proceed? "))
      }

      if (handle == 1){ # Remove all listed special characters
        df[[column]] <- gsub(vals,'', df[[column]])
        replaced <- rbind(replaced, data.frame(column = column, special = paste(special, collapse = " ")))
      }

      if (handle == 2){ # Receive user input on which characters to remove
        val2 <- readline(prompt ='Please input the characters you would like to remove in the format of gsub, EX: "[$%#@!{}]"  ')
        df[[column]] <- gsub(val2,'', df[[column]])
        replaced <- rbind(replaced, data.frame(column = column, special = paste(special, collapse = " ")))
      }


    } else { # If user indicated they do not want input, remove all special characters.
      special <- unique(unlist(regmatches(df[[column]], gregexpr(vals,df[[column]]))))
      found <- rbind(found, data.frame(column = column, special = paste(special, collapse = " ")))
      replaced <- rbind(replaced, data.frame(column = column, special = paste(special, collapse = " ")))
      df[[column]] <- gsub(vals,'', df[[column]])
    }

  }

  return(list('df' = df, 'found' = found, 'replaced' = replaced))
}
