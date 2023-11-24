boxCox = function( X, lambda = NULL, cols = NULL, alpha = 0.001){
  # defaults:
  lambda = NULL; cols = NULL; alpha = 0.001
  fnct_start <<- Sys.time()
  # Get Helper Function
  source("R/rowMin.R")

  # Get the Names if they exist
  # Determine which columns are numeric
  if( is.matrix(X) & is.numeric(X)){ num_col = rep(TRUE, ncol(X))
    col_names = colnames(X)}
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

  ## If there are negative values set to:
  lambda_2[ factor_min < 0 ] = -1*factor_min[ factor_min < 0 ] + alpha

  # shift data
  X_shift = X +  matrix( 1, nrow = n) %*% lambda_2

  # Determine the Geometric mean of each column of X_shift
  geom_mean = exp( colMeans( log( X_shift ) , na.rm =  TRUE ))
  geom_mean_mat = matrix(1, nrow = length(lambda)) %*% geom_mean

  # Set up lambda matrix and Matrix to hold results
  lambda_mat = matrix(1, nrow = n) %*% lambda
  logLik_mat = matrix(0, nrow = length(lambda), ncol = p)
  results = matrix(0 , nrow = p, ncol = 5)
  transformations = matrix(NA, nrow = n, ncol = p)


# Loop through each predictor
  for(i in 1:p){
    #i = 2 # Testing only
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
    logLike = (-n/2)*(log( 2 * pi* X_var) +1) +n*(lambda - 1) * log(geom_mean[i])

    # Determine CI and log-likelihood prediction
    mx = which.max(logLike)
    logLike_mx = logLike[mx]
    lambda_mx = lambda[mx]

    ci_ll = suppressWarnings( max( which(logLike[1:mx ] <= (logLike_mx - .5*qchisq(0.95,1)))))
    lambda_ll = lambda[ci_ll]

    ci_ul = suppressWarnings( min( which(logLike[mx:length(lambda) ] <= (logLike_mx - .5*qchisq(0.95,1)))))
    lambda_ul = lambda[ci_ul + mx -1]

    # Suggest a standard transformation
    ## If boxCox didn't converge in range, don't suggest a transformation, otherwise select the simplest transformation in the CI range
    if(is.na(lambda_ll) | is.na(lambda_ul)){
      lambda_suggest = NA
    } else if(lambda_ll <= round(lambda_mx) & round(lambda_mx) <= lambda_ul) {
        lambda_suggest = round(lambda_mx)
    } else if(lambda_ll <= round(lambda_mx*2)/2 & round(lambda_mx*2)/2 <= lambda_ul) {
      lambda_suggest = round(lambda_mx*2)/2
    } else if(lambda_ll <= round(lambda_mx*3)/3 & round(lambda_mx*3)/3 <= lambda_ul) {
      lambda_suggest = round(lambda_mx*3)/3
    } else if(lambda_ll <= round(lambda_mx*4)/4 & round(lambda_mx*4)/4 <= lambda_ul) {
      lambda_suggest = round(lambda_mx*4)/4
    } else {lambda_suggest = lambda_mx }

    ## Store the transformed transformations associated with lambda_suggest
    if(lambda_suggest %in% lambda){
      transformations[, i ] =  X_trans[,which(lambda == lambda_suggest)]
    } else if(!is.na(lambda_suggest)) {
      if(lambda_suggest == 0){ transformations[, i ] = log(X_shift[,i, drop = FALSE])} else{
      transformations[, i ] =  (X_shift[, i, drop = FALSE]^lambda_suggest - 1)/lambda_suggest}
    }


    # Store the resulting parameters from the BoxCox transformation
    results[ i , 1] = lambda_2[i]
    results[ i , 2] = lambda_ll
    results[ i , 3] = lambda_mx
    results[ i , 4] = lambda_ul
    results[ i , 5] = lambda_suggest

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
if(show_progress == TRUE){close(pb)}

  boxCox_Results = data.frame(
     col_num = cols
    ,col_name = col_names[cols]
    ,lambda_2_selected = results[ , 1]
    #,lambda_1 = results[ , 3]
    ,lambda_1_ll = results[ , 2]
    ,lambda_1_ul = results[ , 4]
    ,lambda_1_selected = results[ , 5]
  )

if(any(is.na(boxCox_Results$lambda_1_ll)) | any(is.na(boxCox_Results$lambda_1_ll))){
  warning("One or more of the variables did not converge in the spcified lambda_1 range")
}

# Select only valid transformations
transformations = as.data.frame(transformations[ , !(is.na(boxCox_Results$lambda_1_selected)) ])
names(transformations) = boxCox_Results$col_name[ !(is.na(boxCox_Results$lambda_1_selected))  ]

fnct_end <<- Sys.time()

return(list(boxCox_Results = boxCox_Results, lambda_1 = lambda,  logLik_mat = logLik_mat, transformations = transformations))

}
