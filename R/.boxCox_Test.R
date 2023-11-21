source("R/boxCox.R")

test_data =
  data.frame(
     X_1 = rpois(1000, lambda = 0.5) +1
    ,X_2 = rchisq(1000, df = 5)
  )


### Test 1
test_bx_1 = MASS::boxcox(data = test_data, X_1~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)
test_bx_1$x[which.max(test_bx_1$y)]

test_1 = boxCox(test_data, eps = 0.1)

### Test 2
test_bx_2 = MASS::boxcox(data = test_data, X_2~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)
test_bx_2$x[which.max(test_bx_2$y)]

lambda[which.max(logLik_mat[, 2])]
plot(lambda,logLik_mat[, 2])
