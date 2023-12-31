% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxCox_Vis.R
\name{boxCox_Vis}
\alias{boxCox_Vis}
\title{Visualize the Results of the boxCox Function}
\usage{
boxCox_Vis(original_data, boxCox_result = NULL, interactive_view = TRUE)
}
\arguments{
\item{original_data}{The original data source passed into the boxCox function}

\item{boxCox_result}{The list object produced by the boxCox function. If not proviced, it will be created using the original data.}

\item{interactive_view}{Boolean TRUE/FALSE to indicate if the user wants to interactively walk through the plots. If set to FALSE, the function outputs a list of plots.}
}
\value{
The boxCox_Vis function outputs distribution plots of the data pre and post transformation to help users select appropriate transformations for their data.
}
\description{
\strong{boxCox_Vis} is a tool to visualize the results of EDAmigo's boxCox function.
}
\examples{
# Create a dataframe with 2 numeric columns
test_data = data.frame( X_1 = rchisq(1000, df = 1), X_2 = rchisq(1000, df = 5) )

# Use the boxCox_Vis function with the default settings to search for
# meaningful transformations in the data
boxCox_Vis(test_data, interactive_view = FALSE)
}
