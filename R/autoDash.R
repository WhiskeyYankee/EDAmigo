#' autoDash
#'
#' Automatically create an interactive dashboard
#'
#' @param df A dataframe, mixed data types allowed.
#'
#' @return Outputs an interactive HTML dashbaoard.
#' @export
#'

#'
autoDash <- function(df){

  rmarkdown::render("inst/Dashboard.Rmd", params= list(x= df$loan_amnt, y= df$funded_amnt,
                  pie = df[, c('term', 'loan_amnt')], box =df[,c('funded_amnt' , 'term')]))
  output_html <- "inst/Dashboard.html"

  # Open the HTML file in the default web browser
  browseURL(paste("file://", normalizePath(output_html), sep=""))
}
