## load libaries
library(testthat)

## Load functions
#source("R/boxCox.R")

# Create toy data set
test_data =
  data.frame(
     X_1 = rchisq(1000, df = 1)
    ,X_2 = rchisq(1000, df = 5))

### Test 1 use the box cox funtion from the mass package to evaluate the data
mass_bxcx_1 = MASS::boxcox(data = test_data, X_1~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)

mass_bxcx_2 = MASS::boxcox(data = test_data, X_2~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)

mass_results = c(mass_bxcx_1$x[which.max(mass_bxcx_1$y)], mass_bxcx_2$x[which.max(mass_bxcx_2$y)])

EDAmigo_results = boxCox(test_data, FILTER = FALSE)

expect_equivalent(mass_results , EDAmigo_results$boxCox_Results$lambda_1)



#### Test 3
data("finance")

finance$loan_amnt = finance$loan_amnt +0.01
test_bx_3 = MASS::boxcox(data = finance, loan_amnt~1,lambda = seq(from =-3, to = 3, by = 0.01),plotit = TRUE)
test_bx_3$x[which.max(test_bx_3$y)]

finance_results = boxCox(finance , FILTER = FALSE)
finance_res = boxCox(finance , FILTER = TRUE)

finance_results_2 = boxCox(finance, cols = c("total_pymnt_inv", "total_pymnt"))
finance_results_3 = boxCox(finance, cols = c(37,36))

### Test 4
library(tidyverse)
library(tidymodels)
X = as.data.frame(state.x77)

state_x77  = as.data.frame(state.x77)
rec <- recipe(~., data = state_x77)

bc_trans <- step_BoxCox(rec, all_numeric(), limits = c(-3,3))

bc_estimates <- prep(bc_trans, training = state_x77)

bc_data <- bake(bc_estimates, state_x77)

plot(density(state_x77[, "Illiteracy"]), main = "before")

plot(density(bc_data$Illiteracy), main = "after")


tidy(bc_trans, number = 1)

tidymodel_est = tidy(bc_estimates, number = 1)


EDAmigo_est = boxCox(state_x77, lambda = seq(from =-5, to = 5, by = 0.01), FILTER = FALSE)

MASS_results = MASS::boxcox(data = state_x77, Income~1,lambda = seq(from =-5, to = 5, by = 0.01),plotit = TRUE)
(MASS_results$x[which.max(MASS_results$y)])


#ENV_results = EnvStats::boxcox(state_x77$Income, lambda = seq(from =-4.5, to = 4.5, by = 0.01), objective.name = "Log-Likelihood")
