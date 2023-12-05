#' Simultaneous Box Cox Power Transformations
#'
#' \strong{boxCox} is a function that aims to ease the EDA process by computing the Box Cox transformations for all of the specified columns in a matrix or dataframe at the same time.
#' The function uses the two-parameter Box-Cox transformation to account for any numeric columns that have either negative values or 0s. The alpha parameter is
#' used to control how the shift parameter lambda_2 is set. The function also outputs asymptotically constructed confidence intervals for the power transformation
#' parameter lambda_1 so users can easily select a simpler alternative power if they choose to. Normality is tested for both the raw and transformed data using the
#' Anderson-Darling method.
#'
#' This implementation of Box Cox uses the following form of the normal log likelihood when selecting the power transformation value lambda. \deqn{log(L(\hat{\mu} , \hat{\sigma})) = -(\frac{n}{2})(log(2\pi \hat{\sigma^2}) +1)+ n(\lambda -1)log(GM(y))} where GM is the geomtric mean.
#'
#' @param X A numeric matrix or a dataframe containing numeric columns
#' @param lambda Numeric value(s) indicating what power(s) to use in the Box Cox transformation. If not provided, boxCox will use -3 , -2.99, ... , 2.99, 3
#' @param cols A vector indicating the column numbers or the names of the columns one wishes to evaluate. If NULL then all numeric columns will be evalutated.
#' @param alpha A numeric value used to determine the shift parameter when 0s and or negative values are detected in the data. In the case of the default 0.001, if the data contains 0s but no negative values, then the shift parameter is set to 0.001.
#' @param FILTER A Boolean value that determines if the output will filter out transformations that don't improve the adherence to normality and reduce outliers. The default is TRUE but user may wish to set it to false to either see why a value is not suggested or to perform a desired transformation.
#'
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'  \item \code{boxCox_Results}: A dataframe containing the estimates for each column evaluated by the function
#'  \item \code{lambda_1}: A vector containing all of the lambdas evaluated by the function.
#'  \item \code{log_Like}: A dataframe containing the log-liklihood calculations for each lambda.
#'  \item \code{transformations}: A dataframe of transformed values.
#' }
#' @export
#'
#' @examples
#' # Create a dataframe with 2 numeric columns
#' test_data = data.frame( X_1 = rchisq(1000, df = 1), X_2 = rchisq(1000, df = 5) )
#'
#'# Use the boxCox function with the default settings to search for meaningful transformations in the data
#' test_results = boxCox(test_data)
#'
#' # View the suggested transformations
#' test_results$boxCox_Results
#'
#' # Create  boxplots of the data before and after the suggested transformation is made
#' par(mfrow = c(2,2))
#' boxplot(test_data$X_1 , main = "X_1 Before")
#' boxplot(test_results$transformations$X_1 , main = "X_1 After")
#' boxplot(test_data$X_2 , main = "X_2 Before")
#' boxplot(test_results$transformations$X_2 , main = "X_2 After")
boxCox = function( X, lambda = NULL, cols = NULL, alpha = 0.001, FILTER = TRUE){
  # Get Helper Function


  # Get the Names if they exist
  # Determine which columns are numeric
  if( is.matrix( X ) & is.numeric( X ) ){ num_col = rep( TRUE, ncol(X) )
    col_names = colnames( X ) }

  if( is.data.frame(X) ) {num_col = sapply(X, class) %in% c("integer","numeric")
    col_names = names(X)}


  # If there are no column names, set to NA
  if(is.null(col_names)){col_names = rep(NA, ncol(X))}

  # Check that alpha is non-zero number
  if(!is.numeric(alpha)){stop("alpha must be a non-zero number")} else {if(alpha <= 0){{stop("alpha must be a non-zero number")}}}

  # Lambda Checks
  if(!is.null(lambda) & !is.numeric(lambda)){
    lambda = NULL
    warning("The lambda provided is not a numeric vector, default will be used")
  }
  if(is.null(lambda)) { lambda = seq(-3,3,0.01) }


  # If cols is supplied, check that the names provided exist in the data and that
  if( !is.null(cols) ){
    if(class(cols) == "character"){
      if( !any(names(X) %in% cols) ){ stop("None of the column names provided exist in the indicated dataset.")}
      if( !any(names(X) %in% cols) ){ stop("None of the column names provided exist in the indicated dataset.")}
      cols = which(cols == names(X))
      }

    if( class(cols) %in% c("integer","numeric") ){
      if(!all(cols %in% 1:ncol(X))){ stop("Column indices provide are outside of the domain of X")}
    }
    if(!all(num_col[cols])){warning("Not all of the columns specified are numeric, only numeric columns will be used.")}
    cols = cols[ num_col[ cols ]]
  }

  # if cols is not supplied, use all of the numeric columns
  if(is.null(cols)){ cols = (1:ncol(X))[num_col]}


  # Set X to a matrix of the numeric column
  X = as.matrix(X[ , cols ])

  # Get the Dimensions of the Matrix X
  n = nrow(X)
  p = ncol(X)

  # Get minimum value of each predictor
  factor_min = rowMin(t(X))$min_value

  # Determine Lambda 2
  ## Initialize Lambda 2 to all 0s
  lambda_2 = rep( 0 , p)

  ## If there are 0s set the Lambda 2 to alpha
  lambda_2[ factor_min == 0 ] = factor_min[ factor_min == 0 ] + alpha

  ## If there are negative values set to -min + alpha:
  lambda_2[ factor_min < 0 ] = -1*factor_min[ factor_min < 0 ] + alpha

  # shift data
  X_shift = X +  matrix( 1, nrow = n) %*% lambda_2

  # Determine the Geometric mean of each column of X_shift
  geom_mean = exp( colMeans( log( X_shift ) , na.rm =  TRUE ))
  geom_mean_mat = matrix(1, nrow = length(lambda)) %*% geom_mean

  # Set up lambda matrix and Matrix to hold results
  lambda_mat = matrix(1, nrow = n) %*% lambda
  logLik_mat = matrix(0, nrow = length(lambda), ncol = p)
  results = matrix(NA , nrow = p, ncol = 10)
  transformations = matrix(NA, nrow = n, ncol = p)

# Loop through each predictor

  for(i in 1:p){
    # get the start time
    if(i == 1){bx_start = Sys.time()}

    # Set up X matrix for log-lik calculations
    X_mat =  X_shift[,i, drop = FALSE] %*% matrix(1, ncol = length(lambda))

    # Transform the data for all lambdas
    X_trans = (X_mat^lambda_mat - 1)/lambda_mat

    # Replace 0 transformation with log transformation
    X_trans[,which(lambda == 0)] = log(X_shift[,i, drop = FALSE])

    # Compute the mean  of each column and store the results as a matrix for ease of variance calculation
    X_mu = matrix(1, nrow = n) %*% colMeans(X_trans, na.rm = T)

    # Compute the variance of each transformation
    X_var = colSums( (X_trans - X_mu)^2, na.rm = T)/n

    # Calculate the log-likelihood
    logLike = (-n/2)*(log( 2 * pi* X_var) +1) +n*(lambda - 1) * log(geom_mean[i]) # Should be able to remove variance from this

    logLike[is.infinite(logLike)] = NA



    # Determine CI and log-likelihood prediction
    mx = which.max(logLike)

    # If a solution is found proceed
  if(length(mx) > 0){
    logLike_mx = logLike[mx]
    lambda_mx = lambda[mx]

    ci_ll = suppressWarnings( max( which(logLike[1:mx ] <= (logLike_mx - .5*qchisq(0.95,1)))))
    lambda_ll = lambda[ci_ll]

    ci_ul = suppressWarnings( min( which(logLike[mx:length(lambda) ] <= (logLike_mx - .5*qchisq(0.95,1)))))
    lambda_ul = lambda[ci_ul + mx -1]

    # Test the normality of the transformed data
    anderson_darling_old = tryCatch( nortest::ad.test(X[ , i]) , error = function(e){NA} )
    anderson_darling_new = tryCatch( nortest::ad.test(X_trans[ , mx]) , error = function(e){NA} )

    # Get a rough count of oulier improvement
    old_25 = as.numeric( tryCatch( stats::quantile(X[ , i] , 0.25, na.rm = T), error = function(e){NA} ) )
    old_75 = as.numeric( tryCatch( stats::quantile(X[ , i] , 0.75, na.rm = T), error = function(e){NA} ) )
    old_lwr = old_25 - 1.5 * (old_75 - old_25)
    old_upr = old_75 + 1.5 * (old_75 - old_25)
    old_outliers = sum( X[ , i] > old_upr | X[ , i] < old_lwr , na.rm = T )

    new_25 = as.numeric( tryCatch( stats::quantile(X_trans[ , mx] , 0.25, na.rm = T), error = function(e){NA} ))
    new_75 = as.numeric( tryCatch( stats::quantile(X_trans[ , mx] , 0.75, na.rm = T), error = function(e){NA} ))
    new_lwr = new_25 - 1.5 * (new_75 - new_25)
    new_upr = new_75 + 1.5 * (new_75 - new_25)
    new_outliers = sum( X_trans[ , mx] > new_upr | X_trans[ , mx] < new_lwr , na.rm = T )

    # Store Transformations
    transformations[, i ] =  X_trans[ ,mx , drop = FALSE ]

    # Store the resulting parameters from the BoxCox transformation
    results[ i , 1] = lambda_2[i]
    results[ i , 2] = lambda_ll
    results[ i , 3] = lambda_mx
    results[ i , 4] = lambda_ul
    if(any(is.na(anderson_darling_old))){
      results[ i , 5] = NA
      results[ i , 7] = NA
      warning(paste("Unable to reslove column: ",col_names[ cols[ i ]]))
    }else{
      results[ i , 5] = as.numeric( anderson_darling_old$statistic )
      results[ i , 7] = as.numeric( anderson_darling_old$p.value )}
    if(any(is.na(anderson_darling_new))){
      results[ i , 6] = NA
      results[ i , 8] = NA
      warning(paste("Unable to reslove column: ",col_names[ cols[ i ]]))
    }else{
      results[ i , 6] = as.numeric( anderson_darling_new$statistic )
      results[ i , 8] = as.numeric( anderson_darling_new$p.value )}
    if(any(is.na(old_outliers))){
      results[ i , 9] = NA
      warning(paste("Unable to reslove column: ",col_names[ cols[ i ]]))
    }else{
      results[ i , 9] = old_outliers}
    if(any(is.na(new_outliers))){
      results[ i , 10] = NA
      warning(paste("Unable to reslove column: ",col_names[ cols[ i ]]))
    }else{
      results[ i , 10] = new_outliers}
  }
    # Store the log likelihood calculations
    logLik_mat[ , i ] = logLike

    # If the estimated run time is more than 3 seconds, show progress bar
    if(i == 1){bx_end = Sys.time()

               show_progress = FALSE}

    if(i == 1 & difftime(bx_end, bx_start, "secs")*p > 3){
      show_progress = TRUE
      # Set progress bar
      pb = txtProgressBar(
        min = 0
        ,max = p -1
        ,style = 3
        ,width = 50
        ,char = "=")}

    if(i >1 & show_progress == TRUE){setTxtProgressBar(pb, i)}
  }

# if progress bar was used, close it
 if(show_progress == TRUE){close(pb)}

# Create a data frame to store the results of the boxCox analysis
  boxCox_Results = data.frame(
     col_num = cols
    ,col_name = col_names[cols]
    ,lambda_2 = results[ , 1]
    ,lambda_1 = results[ , 3]
    ,lambda_1_lwr = results[ , 2]
    ,lambda_1_upr = results[ , 4]
    ,raw_anderson_darling_stat = results[  , 5]
    ,trans_anderson_darling_stat = results[  , 6]
    ,raw_anderson_darling_p_val = results[  , 7]
    ,trans_anderson_darling_p_val = results[  , 8]
    ,raw_outlier_est = results[  , 9]
    ,trans_outlier_est = results[  , 10]
  )

# If any variables did not converge, warn the user
if(any(is.na(boxCox_Results$lambda_1_lwr)) | any(is.na(boxCox_Results$lambda_1_upr))){
  warning("One or more of the variables did not converge in the specified lambda_1 range")
}

transformations = as.data.frame(transformations)
names(transformations) = boxCox_Results$col_name

log_Like = as.data.frame(logLik_mat)
names(log_Like) = boxCox_Results$col_name

if(FILTER == TRUE){
 # Filter Results to transformations that show improvements
 improve = (boxCox_Results$trans_anderson_darling_stat <= boxCox_Results$raw_anderson_darling_stat &
          boxCox_Results$raw_outlier_est >= boxCox_Results$trans_outlier_est) &
          !(is.na(boxCox_Results$lambda_1_lwr |
          is.na(boxCox_Results$lambda_1_upr)))

 improve[ is.na(improve) ] = FALSE


 # Select only results that improve the results
 boxCox_Results = boxCox_Results[ improve ,  , drop = FALSE]

 # Select only transformations having a lambda_1 that converged and store the results in a data frame
 transformations = transformations[ , improve , drop = FALSE ]

 # Select only transformations having a lambda_1 that converged and store the results in a data frame
 log_Like = log_Like[ , improve , drop = FALSE ]

 # Determine Display order
 importance_order = order(  boxCox_Results$trans_anderson_darling_p_val
                         , boxCox_Results$raw_anderson_darling_stat - boxCox_Results$trans_anderson_darling_stat
                         , boxCox_Results$raw_outlier_est - boxCox_Results$trans_outlier_est, decreasing = T)

 # Reorder results
 boxCox_Results = boxCox_Results[ importance_order , ,drop = FALSE ]

 # Reorder results
 transformations = transformations[ , importance_order, drop = FALSE ]

 # Reorder results
 log_Like = log_Like[ , importance_order, drop = FALSE]

}

return(list(boxCox_Results = boxCox_Results, lambda_1 = lambda,  log_Like = log_Like, transformations = transformations))

}
