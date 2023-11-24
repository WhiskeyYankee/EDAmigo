source("R/boxCox.R")

test_data =
  data.frame(
     X_1 = rchisq(1000, df = 1)
    ,X_2 = rchisq(1000, df = 5)
  )

boxCox(test_data)$boxCox_Results
### Test 1
test_bx_1 = MASS::boxcox(data = test_data, X_1~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)
test_bx_1$x[which.max(test_bx_1$y)]
test_trn_1 = (test_data$X_1^(-2) - 1)/(-2)

### Test 2
test_bx_2 = MASS::boxcox(data = test_data, X_2~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)
test_bx_2$x[which.max(test_bx_2$y)]
test_trn_2 = (test_data$X_2^(1/3) - 1)/(1/3)

(test_12_1 = boxCox(test_data)$boxCox_Results)
(test_12_2 = boxCox(test_data, suggest = FALSE)$boxCox_Results)

#### Test 3
data("finance")
test_bx_3 = MASS::boxcox(data = finance, out_prncp~1,lambda = seq(from =-5, to = 5, by = 0.01),plotit = TRUE)
test_bx_3$x[which.max(test_bx_3$y)]

test_3 = boxCox(finance)
test_3_1 = boxCox(finance, suggest = TRUE)

### Test 4
library(tidyverse)
library(tidymodels)
test_4_data  = as.data.frame(state.x77)
rec <- recipe(~., data = as.data.frame(state.x77))

bc_trans <- step_BoxCox(rec, all_numeric())

bc_estimates <- prep(bc_trans, training = as.data.frame(state.x77))

bc_data <- bake(bc_estimates, as.data.frame(state.x77))

plot(density(state.x77[, "Illiteracy"]), main = "before")

plot(density(bc_data$Illiteracy), main = "after")


tidy(bc_trans, number = 1)
tidy(bc_estimates, number = 1)

test_4 = boxCox(test_4_data)

