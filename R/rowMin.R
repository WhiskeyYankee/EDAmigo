#' Determine the minimum value of each row of a matrix. NAs are always ignored.
#'
#' @param X A numeric matrix
#'
#' @return A list containing 2 numeric vectors, 1 that has the minimum row value and the other that indicates which column the row min is found in.
#' @export
#'
#' @examples
#' X = matrix(rnorm(100), nrow = 10)
#' # Return the minimum value of each row
#' rowMin(X)$min_value
#' # Return which column contains the minimum value of each row
#' rowMin(X)$min_which_col
rowMin <- function(X){
  # determine rows and columns
  row_count = base::nrow(X)
  col_count = base::ncol(X)

  # Initialize maximum value vector and matrix
  min_value = base::rep(1,row_count)
  min_which_col = base::rep(1,row_count)

  # replace NAs with Inf
  X[ base::is.na(X) ] = Inf

  # For each column
  for(i in 1:col_count){
    # For subsequent iterations check if the new distance is less that the previously know minimum and if so
    ## (i) assign to the ith class
    ## (ii) set the minimum distance
    if( i == 1){ min_value = X[ , i]} else {
      min_test = base::which(X[ , i] < min_value)
      min_which_col[min_test] = i
      min_value[min_test] = X[min_test , i]}
  }
  return(list(min_value = min_value, min_which_col = min_which_col))
}
