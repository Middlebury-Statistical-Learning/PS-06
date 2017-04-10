# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
# Pakcage for built in cv
library(glmnet)
# Package for string manipulation:
library(stringr)


#objectives:

  # - Obtain the optimal λλ for ridge regression: λ∗ridgeλridge∗

  # - Obtain the optimal λλ for LASSO: λ∗lassoλlasso∗

  # - Compare linear regression, ridge regression, and LASSO using 10-fold cross-validation. Which is best?


# Setup ------------------------------------------------------------------------
# Load training data
train <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/train.csv") %>% 
  # Variable names in R can't start with numbers, so rename:
  dplyr::rename(
    FirstFlrSF = `1stFlrSF`,
    SecondFlrSF = `2ndFlrSF`,
    ThreeSsnPorch = `3SsnPorch`
  ) %>% 
  # To keep things simple for now, select only variables that 1) are numerical 
  # (i.e. no categorical) 2) don't have any missing values in both the training
  # and test sets
  select(
    Id, MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd, 
    FirstFlrSF, SecondFlrSF, LowQualFinSF,
    FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, 
    TotRmsAbvGrd, Fireplaces, WoodDeckSF, OpenPorchSF, 
    EnclosedPorch, ThreeSsnPorch, ScreenPorch, PoolArea, MiscVal, MoSold, 
    YrSold, SalePrice
  )

# Create the model formula for lm()
#LM ONLY
model_formula <- train %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("Id", "fold", "SalePrice")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("SalePrice ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

# Used in glmnet() function for ridge regression and LASSO

#turn model into matrix with values from training set.  
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$SalePrice #actual sale price - what we are trying to predict

# Number of folds to use for all CV
n_folds <- 10

model_lm <- lm(y ~ X)
lm_estimates <- model_lm %>%
  tidy()
lm_estimates

# Get regression coefficients
regression_coefficients <- model_lm %>%
  tidy() %>%
  tbl_df() %>%
  mutate(estimate=round(estimate, 4)) %>%
  select(term, estimate) %>%
  rename(regression_estimate = estimate)
regression_coefficients

# 1. Obtain optimal lambda for ridge regression --------------------------------

# Your code should
# 1. Save the optimal lambda (tuning parameter) in lambda_star_ridge (as a
# scalar and not a data frame). You will be using this optimal lambda later

# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16

# 3. And thus save a ggplot to PDF that shows the shrinking of each beta as 
# lambda varies AND has the optimal lambda marked with a vertical line

# Find the optimal lambda from these values. Feel free to toy with the from, to,
# and length values.
lambda_values <- 10^seq(from = 2, to = 4, length = 2500)

#aplha = 0 is ridge penalty. X is matrix oc observation. y is the response variable. 
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)

#model_ridge generates: df, %dev, lambda

#clean now have estimates, lambda and terms.
model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

#cross validate using prev. defined nfolds.
cvfit <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
str(cvfit)
# Let's plot:
plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_ridge <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

#cv results of ridge regression: lamda, estimate, std.error, conf. high.low, nzero, log_lambda
#estimate from this is the Cross validated MSE.
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))

ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  # geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_ridge), col="red")

# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16

# model_ridge has term, estimate, lambda

# coeffficients
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>% 
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") 


# coefficients has: term, step, estimate, lambda, dev.ratio, log_lambda

# Plot coefficients for each value of lambda, with optimal lambda marked
ridge_coefficients <-ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")
ridge_coefficients

# 3. And thus save a ggplot to PDF that shows the shrinking of each beta as 
# lambda varies AND has the optimal lambda marked with a vertical line

ggsave(filename="PS06_Bianca_Gonzalez_ridge_coefficients.pdf", 
       ridge_coefficients, width=11, height=8.5)


# 2. Obtain optimal lambda for LASSO -------------------------------------------

# Your code should
# 1. Save the optimal lambda (tuning parameter) in lambda_star_LASSO (as a
# scalar and not a data frame). You will be using this optimal lambda later

# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16

# 3. And thus save a ggplot to PDF that shows the shrinking of each beta as 
# lambda varies AND has the optimal lambda marked with a vertical line

# Find the optimal lambda from these values. Feel free to toy with the from, to,
# and length values.
lambda_values <- 10^seq(from = 1, to = 3.5, length = 2500)

#alpha = 1 is lasso penalty. X is matrix oc observation. y is the response variable. 
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#model_lasso generates: df, %dev, lambda

#clean now have estimates, lambda and terms.
model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

#cross validate using prev. defined nfolds.
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)
str(cvfit)
# Let's plot:
plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_lasso <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_lasso

#cv results of lasso regression: lamda, estimate, std.error, conf. high.low, nzero, log_lambda
#estimate from this is the Cross validated MSE.
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))

ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  # geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_lasso), col="red")

# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16

# Plot coefficients for each value of lambda, with optimal lambda marked

# The coefficients: For each of the 100 values of lambda, the 9 coefficients
coefficients <- model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio)) %>% 
  mutate(log_lambda = log(lambda)) %>% 
  filter(term != "(Intercept)") 
# model_lasso has term, estimate, lambda
 



# Plot coefficients for each value of lambda, with optimal lambda marked
lasso_coefficients <-ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_lasso)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="lasso Regression Coefficients")
lasso_coefficients

# Save to PDF
ggsave(filename="PS06_Bianca_Gonzalez_lasso_coefficients.pdf", 
       lasso_coefficients, width=11, height=8.5)



# 3. Cross-validation comparison of lm(), ridge, and LASSO ---------------------
# Create folds
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds) {
  
  pseudo_train <- train %>%
    filter(fold != i)
  
  pseudo_test <- train %>%
    filter(fold == i)
  
  # create new on psuedo train for lm, ridge, and lasso. 
  model_formula <- pseudo_train %>% 
    # Take all predictor variable names and separate them with + signs:
    names() %>% 
    setdiff(c("Id", "fold", "SalePrice")) %>% 
    stringr::str_c(collapse=" + ") %>% 
    # Add outcome variable and ~ sign and convert to formula
    stringr::str_c("SalePrice ~ ", .)    ## string turn to code
  model_formula <- as.formula(model_formula)
  
  #turn model into matrix with values from training set.  
  X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  y <- pseudo_train$SalePrice #actual sale price - what we are trying to predict
  
  # save 3 MSE for 3 methods. so for each fold, compute MSE

  # Find the optimal lambda from these values. Feel free to toy with the from, to,
  # and length values.
  lambda_values <- 10^seq(from = 2, to = 4, length = 2500)
  
  #aplha = 0 is ridge penalty. X is matrix oc observation. y is the response variable. 
  model_ridge1 <- glmnet(X, y, alpha = 0, lambda = lambda_values)
  
  #model_ridge generates: df, %dev, lambda
  
  #clean now have estimates, lambda and terms.
  model_ridge1 %>%
    tidy() %>%
    tbl_df() %>%
    select(-c(step, dev.ratio))
  
  #cross validate using prev. defined nfolds.
  cvfit_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
  
  # Optimal lambda from cross-validation!
  cvfit_ridge %>%
    glance()
  lambda_star_ridge1 <- cvfit_ridge %>%
    glance() %>%
    .[["lambda.min"]]
  lambda_star_ridge1
  
  lambda_values <- 10^seq(from = 1, to = 3.5, length = 2500)
  
  #alpha = 1 is lasso penalty. X is matrix oc observation. y is the response variable. 
  model_lasso1 <- glmnet(X, y, alpha = 1, lambda = lambda_values)
  
  #model_lasso generates: df, %dev, lambda
  
  #clean now have estimates, lambda and terms.
  model_lasso1 %>%
    tidy() %>%
    tbl_df() %>%
    select(-c(step, dev.ratio))
  
  #cross validate using prev. defined nfolds.
  cvfit_lasso <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)
  
  # Optimal lambda from cross-validation!
  cvfit_lasso %>%
    glance()
  lambda_star_lasso1 <- cvfit_lasso %>%
    glance() %>%
    .[["lambda.min"]]
  lambda_star_lasso1
  
  # Get predictions for all three methods.
  lm_predictions <- model_lm %>% 
    predict(newdata=pseudo_test) 
  
  # Create predictor matrix as before
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  
  ridge_predictions <- model_ridge1 %>%
    predict(newx=X_new, s=lambda_star_ridge1)
  
  LASSO_predictions <- model_lasso1 %>%
    predict(newx=X_new, s=lambda_star_lasso1) 
  
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      
      SalePrice = pseudo_test$y,
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
      
    ) %>% 
    # Switch to tidy format so we can use group_by() to compute MSE's for all
    # three methods easily
    tidyr::gather(method, yhat, -SalePrice) %>% 
    group_by(method) %>% 
    summarise(MSE = mean((SalePrice-yhat)^2)) %>% 
    # Include fold i variable and append/bind to results_all_folds
    mutate(fold=i) %>% 
    bind_rows(results_all_folds)
}

# Show results
results_all_folds %>% 
  group_by(method) %>% 
  summarise(MSE_CV = mean(MSE)) %>% 
  arrange(desc(MSE_CV))

# Run the CV comparision a few times manually (in order to account for sampling
# variability) and get a rough ranking of which method does best. You will use
# this method below


# 4. Submit Predictions to Kaggle ----------------------------------------------
sample_submission <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/sample_submission.csv")
test <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/test.csv") %>%
  # Variable names in R can't start with numbers, so rename:
  dplyr::rename(
    FirstFlrSF = `1stFlrSF`,
    SecondFlrSF = `2ndFlrSF`,
    ThreeSsnPorch = `3SsnPorch`
  ) %>% 
  # Add a "dummy" SalePrice outcome so that the model.matrix() commmand below
  # will still work
  mutate(SalePrice=1)

# In case your optimal method was ridge regression or LASSO, you'll need this 
# for glmnet()
test_X <- model.matrix(model_formula, data = test)[, -1]
y <- test$SalePrice #actual sale price - what we are trying to predict

lambda_values <- 10^seq(from = 1, to = 3.5, length = 2500)

#alpha = 1 is lasso penalty. X is matrix oc observation. y is the response variable. 
model_lasso2 <- glmnet(test_X, y, alpha = 1, lambda = lambda_values)

#model_lasso generates: df, %dev, lambda

#clean now have estimates, lambda and terms.
model_lasso2 %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

#cross validate using prev. defined nfolds.
cvfit_lasso <- cv.glmnet(test_X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

# Optimal lambda from cross-validation!
cvfit_lasso %>%
  glance()
lambda_star_lasso2 <- cvfit_lasso %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_lasso2

# Create predictor matrix as before
X_new <- model.matrix(model_formula, data = test)[, -1]

# Get predictions using what you think was the best method:
#specific_lambdas <- c(lambda_star_lasso2, 0.5)
predictions <-  model_lasso2 %>%
  predict(newx=X_new, s=lambda_star_lasso2) 
  # Write submissions to CSV
  sample_submission %>% 
  mutate(SalePrice = as.vector(predictions)) %>% 
  write_csv("PS06_Bianca_Gonzalez_submission.csv")
  getwd()
