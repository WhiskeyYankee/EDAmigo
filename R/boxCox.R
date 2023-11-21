boxCox = function( X, lambda = NULL, eps = 0.01){
  # Get Helper Function
  source("R/rowMin.R")

  # Determin which columns are numeric
  num_col = sapply(X, class) == "numeric"

  # Set X to a matrix of the numeric column
  X = as.matrix(X[ , num_col])

  # Determine the Dimensions of the Matrix X
  n = nrow(X)
  p = ncol(X)

  # [TODO] if lambda is null set to...
  lambda = seq(-3,3,0.01)


  # Get minimum value of each predictor
  factor_min = rowMin(t(X))$min_value

  # Determine Lambda 2
  ## Initialize Lambda 2 to all 0s
  lambda_2 = rep( 0 , p)
  ## If there are 0s set the Lambda 2 to 0.001
  lambda_2[ factor_min == 0 ] = factor_min[ factor_min == 0 ] + 0.001
  ## If there are negative values,
  lambda_2[ factor_min < 0 ] = -1*factor_min[ factor_min < 0 ] + 0.001

  # shift data
  X_shift = X +  matrix( 1, nrow = n) %*% lambda_2

  # Determine the Geometric mean of each column of X_shift
  geom_mean = exp( colMeans( log( X_shift ) , na.rm =  TRUE ))
  geom_mean_mat = matrix(1, nrow = length(lambda)) %*% geom_mean

  # Make powers matrix
  ##powers =  lambda %*% matrix(1, ncol= p)
  ##powers = 2 * (powers - 1)

  # Set up lambda matrix and Matrix to hold results
  lambda_mat = matrix(1, nrow = n) %*% lambda
  logLik_mat = matrix(0, nrow = length(lambda), ncol = p)
  results = matrix(0 , nrow = p, ncol = 3)

# Loop through each predictor
  for(i in 1:p){
    #i = 1 # Testing only

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
    logLike = (-n/2)*(log( 2 * pi* X_var) +1) +n*(lambda - 1) * log(geom_mean[i])

    # Determine CI and log-likelihood prediction
    ci_ll = min(which(abs(logLike-(max(logLike)-.5*qchisq(0.95,1)))<=eps))
       ll = which.max(logLike)
    ci_ul = max(which(abs(logLike-(max(logLike)-.5*qchisq(0.95,1)))<=eps))

    results[ i , 1] = ifelse(ci_ll< ll, lambda[ci_ll], lambda[ll])
    results[ i , 2] = lambda[ll]
    results[ i , 3] = ifelse(ci_ul> ll, lambda[ci_ul], lambda[ll])

     # Store the results
    logLik_mat[ , i ] = logLike
  }
return(list(results = results, logLik_mat = logLik_mat))
}
