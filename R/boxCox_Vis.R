#' Visualize the Results of the boxCox Function
#'
#'\strong{boxCox_Vis} is a tool to visualize the results of EDAmigo's boxCox function.
#'
#' @param original_data The original data source passed into the boxCox function
#' @param boxCox_result  The list object produced by the boxCox function. If not proviced, it will be created using the original data.
#' @param interactive_view Boolean TRUE/FALSE to indicate if the user wants to interactively walk through the plots. If set to FALSE, the function outputs a list of plots.
#'
#' @return The boxCox_Vis function outputs distribution plots of the data pre and post transformation to help users select appropriate transformations for their data.
#' @export
#'
#' @examples
#' # Create a dataframe with 2 numeric columns
#' test_data = data.frame( X_1 = rchisq(1000, df = 1), X_2 = rchisq(1000, df = 5) )
#'
#'# Use the boxCox_Vis function with the default settings to search for
#'# meaningful transformations in the data
#' boxCox_Vis(test_data, interactive_view = FALSE)
boxCox_Vis = function(original_data, boxCox_result = NULL, interactive_view = TRUE){

  # if the box cox results havent been provided then calculate them
  if(is.null(boxCox_result)){boxCox_result = boxCox(original_data , lambda = NULL, cols = NULL, alpha = 0.001, FILTER = TRUE)}

  # Extract column names from boxCox and construct plot data
  plot_columns = boxCox_result$boxCox_Results$col_name
  plot_data = rbind(original_data[ , plot_columns] , boxCox_result$transformations)

  # Check if there is any column with the sames names as boxCox_source and if so, alter it
  if(any(names(plot_data) %in% c("boxCox_source"))){ names(plot_data)[names(plot_data) == "boxCox_source"] = "boxCox_source.org"}

  # Set up the source column for the plots
  plot_data[ , "boxCox_source"] = "Transformed Data"
  plot_data[1:nrow(original_data) , "boxCox_source" ] = "Raw Data"

  # Create a plot for each column and store the results in a list
  var_dist = function(data, column) {
    ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
      ggplot2::geom_boxplot(ggplot2::aes(y = -0.5, color = boxCox_source)) +
      ggplot2::geom_density()+
      ggplot2::xlab(column) +
      ggplot2::facet_wrap(~boxCox_source , nrow = 2, scales = "free")}

  var_dists <- lapply(plot_columns, var_dist, data = plot_data)

  # If the interactive option is set to TRUE then
  if(interactive_view == TRUE){
    proceed = 0
    for(i in 1:length(plot_columns)){
      if (proceed == 2){break}
      cat(i, ' of ', length(plot_columns), ' transformations to review.\n' )
      print(var_dists[[i]])
      review = t(boxCox_result$boxCox_Results[ i , , drop = FALSE] )
      colnames(review) = ""
      print(noquote(review ))
      cat('\n')
      proceed = utils::menu(c('Next Factor','Exit'), title = "Box-Cox Transformations Review:")
      if (proceed == 2){break}
      if (i == length(plot_columns)){break}
    }
  }else{return(var_dists)}
}
