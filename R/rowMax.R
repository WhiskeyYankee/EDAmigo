
#' Determine the maximum value of each row of a matrix. NAs are always ignored.
#'
#' @param X A numeric matrix
#'
#' @return A list containing 2 numeric vectors, 1 that has the maximum row value and the other that indicates which column the row max is found in.
#' @export
#'
#' @examples
#' X = matrix(rnorm(100), nrow = 10)
#' # Return the maximum value of each row
#' rowMax(X)$max_value
#' # Return which column contains the maximum value of each row
#' rowMax(X)$max_which_col
rowMax <- function(X){
  # determine rows and columns
  row_count = base::nrow(X)
  col_count = base::ncol(X)

  # Initialize maximum value vector and matrix
  max_value = base::rep(1,row_count)
  max_which_col = base::rep(1,row_count)

  # replace NAs with Inf
  X[ base::is.na(X) ] = -Inf

  # For each column
  for(i in 1:col_count){
    # For subsequent iterations check if the new distance is less that the previously know minimum and if so
    ## (i) assign to the ith class
    ## (ii) set the minimum distance
    if( i == 1){ max_value = X[ , i]} else {
      max_test = base::which(X[ , i] > max_value)
      max_which_col[max_test] = i
      max_value[max_test] = X[max_test , i]}
  }
  return(list(max_value = max_value, max_which_col = max_which_col))
}
